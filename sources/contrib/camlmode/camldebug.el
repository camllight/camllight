;;; Run camldebug under Emacs
;;; Derived from gdb.el.
;;; Author: W. Schelter, University of Texas
;;;     wfs@rascal.ics.utexas.edu
;;; Rewritten by rms.

;; Some ideas are due to  Masanobu. 

;; This file is part of GNU Emacs.
;; Copyright (C) 1988 Free Software Foundation, Inc.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
;; to anyone for the consequences of using it or for whether it serves
;; any particular purpose or works at all, unless he says so in writing.
;; Refer to the GNU Emacs General Public License for full details.

;; Everyone is granted permission to copy, modify and redistribute GNU
;; Emacs, but only under the conditions described in the GNU Emacs
;; General Public License.  A copy of this license is supposed to have
;; been given to you along with GNU Emacs so you can know your rights and
;; responsibilities.  It should be in a file named COPYING.  Among other
;; things, the copyright notice and this notice must be preserved on all
;; copies.

(require 'comint)
(require 'shell)
(require 'caml)

;;; Variables.

(defvar camldebug-emacs-19 (not (string-lessp emacs-version "19")))

(defvar camldebug-last-frame)
(defvar camldebug-delete-prompt-marker)
(defvar camldebug-filter-accumulator nil)
(defvar camldebug-last-frame-displayed-p)

(defvar camldebug-prompt-pattern "^(cdb) *"
  "A regexp to recognize the prompt for camldebug.") 

(defvar camldebug-mode-map nil
  "Keymap for camldebug-mode.")

(defvar camldebug-overlay-event nil
  "Overlay for displaying the current event.")
(defvar camldebug-overlay-under nil
  "Overlay for displaying the current event.")
(defvar camldebug-event-marker nil
  "Marker for displaying the current event.")

(cond ((and camldebug-emacs-19 window-system)
       (make-face 'camldebug-event)
       (make-face 'camldebug-underline)
       (if (not (face-differs-from-default-p 'camldebug-event))
           (invert-face 'camldebug-event))
       (if (not (face-differs-from-default-p 'camldebug-underline))
           (set-face-underline-p 'camldebug-underline t))
       (setq camldebug-overlay-event (make-overlay 1 1))
       (overlay-put camldebug-overlay-event 'face 'camldebug-event)
       (setq camldebug-overlay-under (make-overlay 1 1))
       (overlay-put camldebug-overlay-under 'face 'camldebug-underline))
      (t
       (setq camldebug-event-marker (make-marker))
       (setq overlay-arrow-string "=>")))

;;; Keymaps.

(if camldebug-mode-map
   nil
  (cond (camldebug-emacs-19
	 (setq camldebug-mode-map (cons 'keymap comint-mode-map)))
	(t
	 (setq camldebug-mode-map (copy-keymap comint-mode-map))))
  (define-key camldebug-mode-map "\C-l" 'camldebug-refresh)
  (define-key camldebug-mode-map "\C-c\C-c" 'camldebug-control-c-subjob)
  (define-key camldebug-mode-map "\t" 'comint-dynamic-complete)
  (define-key camldebug-mode-map "\M-?" 'comint-dynamic-list-completions))

(if caml-mode-map
    (progn
      (define-key caml-mode-map "\C-x " 'camldebug-break)
      (define-key caml-mode-map "\C-x&" 'send-camldebug-command)))

;;Of course you may use `def-camldebug' with any other camldebug command.

(defmacro def-camldebug (name key &optional doc)
  (let* ((fun (intern (format "camldebug-%s" name)))
	 (cstr (list 'if '(not (= 1 arg))
		     (list 'format "%s %s" name 'arg)
		     name)))
    (list 'progn
 	  (list 'defun fun '(arg)
		(or doc "")
		'(interactive "p")
		(list 'camldebug-call cstr))
	  (if key
	      (list 'define-key 'camldebug-mode-map key  (list 'quote fun))
	    nil))))

(def-camldebug "step"   "\M-s" "Step one source line with display.")
(def-camldebug "back"   "\M-b" "Step backward one source line with display.")
(def-camldebug "last"   "\M-l" "Go back to previous time.")
(def-camldebug "run"   "\M-r" "Run.")
(def-camldebug "finish" "\C-c\C-f" "Finish executing current function.")
(def-camldebug "next"   nil "Step one source line (skip functions)")

(def-camldebug "up"     "\C-c<" "Go up N stack frames (numeric arg) with display")
(def-camldebug "down"   "\C-c>" "Go down N stack frames (numeric arg) with display")

;;; Camldebug mode.

(defun camldebug-mode ()
  "Major mode for interacting with an inferior Camldebug process.
The following commands are available:

\\{camldebug-mode-map}

\\[camldebug-display-frame] displays in the other window
the last line referred to in the camldebug buffer.

\\[camldebug-step],\\[camldebug-next], and \\[camldebug-nexti] in the camldebug window,
call camldebug to step,next or nexti and then update the other window
with the current file and position.

If you are in a source file, you may select a point to break
at, by doing \\[camldebug-break].

Commands:
Many commands are inherited from comint mode. 
Additionally we have:

\\[camldebug-display-frame] display frames file in other window
\\[camldebug-step] advance one line in program
\\[send-camldebug-command] used for special printing of an arg at the current point.
C-x SPACE sets break point at current line."
  (interactive)
  (comint-mode)
  (use-local-map camldebug-mode-map)
  (set-syntax-table caml-mode-syntax-table)
  (mapcar 'make-local-variable
	  '(camldebug-last-frame-displayed-p  camldebug-last-frame
	    camldebug-delete-prompt-marker    camldebug-filter-accumulator))
  (setq
   camldebug-last-frame nil
   camldebug-delete-prompt-marker nil
   camldebug-filter-accumulator nil
   major-mode 'camldebug-mode
   mode-name "Inferior CDB"
   comint-prompt-regexp camldebug-prompt-pattern
   camldebug-last-frame-displayed-p t)
  (make-local-variable 'shell-dirtrackp)
  (setq shell-dirtrackp t)
  (setq comint-input-sentinel 'shell-directory-tracker)
  (run-hooks 'camldebug-mode-hook))

(defvar current-camldebug-buffer nil)

;;;###autoload
(defvar camldebug-command-name "camldebug"
  "Pathname for executing camldebug.")

;;;###autoload
(defun camldebug (path)
  "Run camldebug on program FILE in buffer *camldebug-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for camldebug.  If you wish to change this, use
the camldebug commands `cd DIR' and `directory'."
  (interactive "fRun camldebug on file: ")
  (setq path (expand-file-name path))
  (let ((file (file-name-nondirectory path)))
    (switch-to-buffer (concat "*camldebug-" file "*"))
    (setq default-directory (file-name-directory path))
    (or (bolp) (newline))
    (insert "Current directory is " default-directory "\n")
    (make-comint (concat "camldebug-" file)
		 (substitute-in-file-name camldebug-command-name)
		 nil
		 "-emacs" "-cd" default-directory file)
    (set-process-filter (get-buffer-process (current-buffer))
			'camldebug-filter)
    (set-process-sentinel (get-buffer-process (current-buffer))
			  'camldebug-sentinel)
    (camldebug-mode)
    (camldebug-set-buffer)))

(defun camldebug-set-buffer ()
  (cond ((eq major-mode 'camldebug-mode)
	(setq current-camldebug-buffer (current-buffer)))))

;;; Filter and sentinel.

;; This function is responsible for inserting output from CDB
;; into the buffer.
;; Aside from inserting the text, it notices and deletes
;; each filename-and-character
;; that CDB prints to identify the selected frame.
;; It records the filename and line number, and maybe displays that file.
(defun camldebug-filter (proc string)
  (let ((inhibit-quit t))
    (if camldebug-filter-accumulator
	(camldebug-filter-accumulate-marker
	 proc
	 (concat camldebug-filter-accumulator string))
      (camldebug-filter-scan-input proc string))))

(defun camldebug-filter-accumulate-marker (proc string)
  (setq camldebug-filter-accumulator nil)
  (if (> (length string) 1)
      (if (= (aref string 1) ?\032)
	  (let ((end (string-match "\n" string)))
	    (if end
		(progn
		  (let* ((first-colon (string-match ":" string 2))
			 (second-colon
			  (string-match ":" string (1+ first-colon))))
		    (setq camldebug-last-frame
			  (cons (substring string 2 first-colon)
				(cons       (string-to-int
					     (substring string (1+ first-colon)
							second-colon))
					    (equal "before"
						   (substring string
							      (1+ second-colon)
							      end))))))
		  (setq camldebug-last-frame-displayed-p nil)
		  (camldebug-filter-scan-input proc
					       (substring string (1+ end))))
	      (setq camldebug-filter-accumulator string)))
	(camldebug-filter-insert proc "\032")
	(camldebug-filter-scan-input proc (substring string 1)))
    (setq camldebug-filter-accumulator string)))

(defun camldebug-filter-scan-input (proc string)
  (if (equal string "")
      (setq camldebug-filter-accumulator nil)
      (let ((start (string-match "\032" string)))
	(if start
	    (progn (camldebug-filter-insert proc (substring string 0 start))
		   (camldebug-filter-accumulate-marker proc
						 (substring string start)))
	    (camldebug-filter-insert proc string)))))

(defun camldebug-filter-insert (proc string)
  (let ((moving (= (point) (process-mark proc)))
	(output-after-point (< (point) (process-mark proc)))
	(old-buffer (current-buffer))
	start)
    (set-buffer (process-buffer proc))
    (unwind-protect
	(save-excursion
	  ;; Insert the text, moving the process-marker.
	  (goto-char (process-mark proc))
	  (setq start (point))
	  (insert-before-markers string)
	  (set-marker (process-mark proc) (point))
	  (camldebug-maybe-delete-prompt)
	  ;; Check for a filename-and-character number.
	  (camldebug-display-frame
	   ;; Don't display the specified file
	   ;; unless (1) point is at or after the position where output appears
	   ;; and (2) this buffer is on the screen.
	   (or output-after-point
	       (not (get-buffer-window (current-buffer))))
	   ;; Display a file only when a new filename-and-character-number appears.
	   t))
      (set-buffer old-buffer))
    (if moving (goto-char (process-mark proc)))))

(defun camldebug-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 ;; Stop displaying an arrow in a source file.
	 (camldebug-remove-events)
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 ;; Stop displaying an arrow in a source file.
	 (camldebug-remove-events)
	 ;; Fix the mode line.
	 (setq mode-line-process
	       (concat ": "
		       (symbol-name (process-status proc))))
	 (let* ((obuf (current-buffer)))
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 ;; Force mode line redisplay soon
		 (set-buffer-modified-p (buffer-modified-p))
		 (if (eobp)
		     (insert ?\n mode-name " " msg)
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the cdb buffer.
	     (set-buffer obuf))))))


(defun camldebug-refresh (&optional arg)
  "Fix up a possibly garbled display, and redraw the mark."
  (interactive "P")
  (recenter arg)
  (camldebug-display-frame))

(defun camldebug-display-frame (&optional nodisplay noauto)
  "Find, obey and delete the last filename-and-line marker from CDB.
The marker looks like \\032\\032FILENAME:CHARACTER\\n.
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (camldebug-set-buffer)
  (and camldebug-last-frame (not nodisplay)
       (or (not camldebug-last-frame-displayed-p) (not noauto))
       (progn (camldebug-display-line (car camldebug-last-frame) (car (cdr camldebug-last-frame)) (cdr (cdr camldebug-last-frame)))
	      (setq camldebug-last-frame-displayed-p t))))

;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
;; and that its character CHARACTER is visible.
;; Put the mark on this character in that buffer.

(defun camldebug-display-line (true-file character kind)
  (let* ((pre-display-buffer-function nil) ; screw it, put it all in one screen
	 (pop-up-windows t)
	 (buffer (find-file-noselect true-file))
	 (window (display-buffer buffer t))
	 (pos))
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(widen)
	(setq pos (+ (point-min) character))
	(camldebug-set-current-event pos (current-buffer) kind))
      (cond ((or (< pos (point-min)) (> pos (point-max)))
	     (widen)
	     (goto-char pos))))
    (set-window-point window pos)))

;;; Events.

(defun camldebug-remove-events ()
  (if (and camldebug-emacs-19 window-system)
      (progn
        (delete-overlay camldebug-overlay-event)
        (delete-overlay camldebug-overlay-under))
    (setq overlay-arrow-position nil)))

(defun camldebug-set-current-event (pos buffer before)
  (if (and camldebug-emacs-19 window-system)
      (progn
        (move-overlay camldebug-overlay-event pos (1+ pos) buffer)
        (if before
            (move-overlay camldebug-overlay-under (+ pos 1) (+ pos 3) buffer)
          (move-overlay camldebug-overlay-under (- pos 2) pos buffer)))
    (save-excursion
      (set-buffer buffer)
      (goto-char pos)
      (beginning-of-line)
      (move-marker camldebug-event-marker (point))
      (setq overlay-arrow-position camldebug-event-marker))))

;;; Miscellaneous.

(defun camldebug-module-name (filename)
  (substring filename (string-match "\\([^/]*\\)\\.ml$" filename) (match-end 1)))

(defun camldebug-call (command)
  "Invoke camldebug COMMAND displaying source in other window."
  (interactive)
  (goto-char (point-max))
  ;; Record info on the last prompt in the buffer and its position.
  ;; This is used in  camldebug-maybe-delete-prompt
  ;; to prevent multiple prompts from accumulating.
  (save-excursion
    (goto-char (process-mark (get-buffer-process current-camldebug-buffer)))
    (let ((pt (point)))
      (beginning-of-line)
      (setq camldebug-delete-prompt-marker
	    (if (= (point) pt)
		nil
	      (list (point-marker) (- pt (point))
		    (buffer-substring (point) pt))))))
  (camldebug-set-buffer)
  (process-send-string (get-buffer-process current-camldebug-buffer)
	       (concat command "\n")))

(defun camldebug-maybe-delete-prompt ()
  (if camldebug-delete-prompt-marker
      ;; Get the string that we used as the prompt before.
      (let ((prompt (nth 2 camldebug-delete-prompt-marker))
	    (length (nth 1 camldebug-delete-prompt-marker)))
	;; Position after it.
	(goto-char (+ (car camldebug-delete-prompt-marker) length))
	;; Delete any duplicates of it which follow right after.
	(while (and (<= (+ (point) length) (point-max))
		    (string= prompt
			     (buffer-substring (point) (+ (point) length))))
	  (delete-region (point) (+ (point) length)))
	;; If that didn't take us to where output is arriving,
	;; we have encountered something other than a prompt,
	;; so stop trying to delete any more prompts.
	(if (not (= (point)
		    (process-mark (get-buffer-process current-camldebug-buffer))))
	    (progn
	      (set-marker (car camldebug-delete-prompt-marker) nil)
	      (setq camldebug-delete-prompt-marker nil))))))

(defun camldebug-break ()
  "Set CDB breakpoint at this source line."
  (interactive)
  (let ((file-name (file-name-nondirectory buffer-file-name))
	(line (save-restriction
		(widen)
		(beginning-of-line)
		(1+ (count-lines 1 (point))))))
    (process-send-string (get-buffer-process current-camldebug-buffer)
			 (concat "break @ "
				 (camldebug-module-name file-name)
				 " "
				 line
				 "\n"))))

(defun camldebug-read-address ()
  "Return a string containing the core-address found in the buffer at point."
  (save-excursion
   (let ((pt (point)) found begin)
     (setq found (if (search-backward "0x" (- pt 7) t)(point)))
     (cond (found (forward-char 2)
		  (buffer-substring found
				    (progn (re-search-forward "[^0-9a-f]")
					   (forward-char -1)
					   (point))))
	   (t (setq begin (progn (re-search-backward "[^0-9]") (forward-char 1)
				 (point)))
	      (forward-char 1)
	      (re-search-forward "[^0-9]")
	      (forward-char -1)
	      (buffer-substring begin (point)))))))


(defvar camldebug-commands nil
  "List of strings or functions used by send-camldebug-command.
It is for customization by you.")

(defun send-camldebug-command (arg)

  "This command reads the number where the cursor is positioned.  It
 then inserts this ADDR at the end of the camldebug buffer.  A numeric arg
 selects the ARG'th member COMMAND of the list camldebug-print-command.  If
 COMMAND is a string, (format COMMAND ADDR) is inserted, otherwise
 (funcall COMMAND ADDR) is inserted.  eg. \"p (rtx)%s->fld[0].rtint\"
 is a possible string to be a member of camldebug-commands.  "


  (interactive "P")
  (let (comm addr)
    (if arg (setq comm (nth arg camldebug-commands)))
    (setq addr (camldebug-read-address))
    (if (eq (current-buffer) current-camldebug-buffer)
	(set-mark (point)))
    (cond (comm
	   (setq comm
		 (if (stringp comm) (format comm addr) (funcall comm addr))))
	  (t (setq comm addr)))
    (switch-to-buffer current-camldebug-buffer)
    (goto-char (point-max))
    (insert comm)))

(defun camldebug-control-c-subjob ()
  "Send a Control-C to the subprocess."
  (interactive)
  (process-send-string (get-buffer-process (current-buffer))
		       "\C-c"))

(provide 'camldebug)
