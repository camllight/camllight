;;; Run cdb under Emacs
;;; Derived from gdb.el:

;; Author: W. Schelter, University of Texas
;;     wfs@rascal.ics.utexas.edu
;; Rewritten by rms.

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

(defvar camldebug-xemacs (string-match "\\(Lucid\\|Xemacs\\)" emacs-version))
(defvar camldebug-emacs-19 (not (string-lessp emacs-version "19")))

(defvar camldebug-last-frame)
(defvar camldebug-delete-prompt-marker)
(defvar camldebug-filter-accumulator nil)
(defvar camldebug-last-frame-displayed-p)

(defvar camldebug-prompt-pattern "^(cdb) *"
  "A regexp to recognize the prompt for camldebug.") 

(defvar camldebug-mode-map nil
  "Keymap for camldebug-mode.")

(if (and camldebug-emacs-19 window-system)
    (progn
      (make-face 'camldebug-current-event)
      (if (not (face-differs-from-default-p 'camldebug-current-event))
	  (invert-face 'camldebug-current-event))))

;;; Keymaps.

(if camldebug-mode-map
   nil
  (cond (camldebug-xemacs
	 (setq camldebug-mode-map (make-sparse-keymap))
	 (set-keymap-name camldebug-mode-map 'camldebug-mode-map)
	 (set-keymap-parent camldebug-mode-map comint-mode-map))
	(camldebug-emacs-19
	 (setq camldebug-mode-map (cons 'keymap comint-mode-map)))
	(t
	 (setq camldebug-mode-map (copy-keymap comint-mode-map))))
  (define-key camldebug-mode-map "\C-l" 'camldebug-refresh)
  (define-key camldebug-mode-map "\C-c\C-c" 'camldebug-control-c-subjob)
  (define-key camldebug-mode-map "\t" 'comint-dynamic-complete)
  (define-key camldebug-mode-map "\M-e" 'camldebug-variable-list))

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

(def-camldebug "go"   "\M-g" "Go to given time.")
(def-camldebug "step"   "\M-s" "Step one source line with display.")
(def-camldebug "back"   "\M-b" "Step backward one source line with display.")
(def-camldebug "last"   "\M-l" "Go back to previous time.")
(def-camldebug "finish"   "\M-f" "Finish current function.")
(def-camldebug "run"   "\M-r" "Run.")
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

\\[camldebug-step],\\[camldebug-back], and \\[camldebug-next] in the camldebug window,
call camldebug to step, backstep or next and then update the other window
with the current file and position.

If you are in a source file, you may select a point to break
at, by doing \\[camldebug-break].

Commands:
Many commands are inherited from comint mode. 
Additionally we have:

\\[camldebug-display-frame] display frames file in other window
\\[camldebug-step] advance one line in program
C-x SPACE sets break point at current line."
  (interactive)
  (comint-mode)
  (use-local-map camldebug-mode-map)
  (set-syntax-table caml-mode-syntax-table)
  (mapcar 'make-local-variable
	  '(camldebug-last-frame-displayed-p  camldebug-last-frame
	    camldebug-delete-prompt-marker    camldebug-filter-accumulator
	    camldebug-duplicate               camldebug-value-buffer
	    camldebug-file-name))
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
  "*Pathname for executing camldebug.")

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
    (setq camldebug-file-name file)
    (camldebug-set-buffer)))

(defun camldebug-set-buffer ()
  (cond ((eq major-mode 'camldebug-mode)
	(setq current-camldebug-buffer (current-buffer)))))

;;; Values.

;; Does output from the debugger should be duplicated in the values
;; buffer ?
(defvar camldebug-duplicate nil)

;; Does output from the debugger should be written in the main buffer ?
(defvar camldebug-hidden nil)

;; Value buffer.
;; --- Buffer local.
(defvar camldebug-value-buffer nil)

;; Separator between values in value buffer.
(defvar camldebug-new-value "\014\n")

;; Regexp for searching this separator.
(defvar camldebug-new-value-regexp (concat "^" camldebug-new-value))

;; Description of values in value buffer.
(defvar camldebug-value-list nil)

;; The pattern given to the debugger when asking for a value.
(defvar camldebug-expected-pattern nil)

;; Accumulator name.
(defvar camldebug-accumulator-name "(accu)")

(defvar camldebug-process-buffer nil)

;; Major mode for browsing Caml values.
(defun camldebug-value-mode ()
  "Major mode for interacting with an inferior Camldebug process.
The following commands are available:

\\{camldebug-value-mode-map}

"
  (interactive)
  (use-local-map camldebug-value-mode-map)
  (mapcar 'make-local-variable
	  '(camldebug-value-list camldebug-process-buffer))
  (setq major-mode 'camldebug-value-mode
	mode-name "Value browser"
	buffer-read-only t)
  (run-hooks 'camldebug-value-mode-hook))

;; Keymap for `camldebug-value-mode'.
(defvar camldebug-value-mode-map nil)

(if camldebug-value-mode-map
    nil
  (setq camldebug-value-mode-map (make-sparse-keymap))
  (define-key camldebug-value-mode-map "s" 'camldebug-switch-to-debugger)
  (define-key camldebug-value-mode-map "d" 'camldebug-variable-list)
  (define-key camldebug-value-mode-map "n" 'camldebug-next-variable)
  (define-key camldebug-value-mode-map "p" 'camldebug-previous-variable)
  (define-key camldebug-value-mode-map "t" 'camldebug-variable-top)
  (define-key camldebug-value-mode-map "u" 'camldebug-value-up)
  (define-key camldebug-value-mode-map "^" 'camldebug-value-up)
;  (define-key camldebug-value-mode-map "l" 'camldebug-value-last)
  (define-key camldebug-value-mode-map " " 'scroll-up)
  (define-key camldebug-value-mode-map "\177" 'scroll-down)
  (define-key camldebug-value-mode-map "\C-m" 'camldebug-select-subexpression)
  (define-key camldebug-value-mode-map "f" 'camldebug-select-subexpression)
  (define-key camldebug-value-mode-map "q" 'bury-buffer)
  (if camldebug-emacs-19
      (if camldebug-xemacs
	  (define-key camldebug-value-mode-map [button2]
	    'camldebug-mouse-select-subexpression)
	(define-key camldebug-value-mode-map [mouse-2]
	  'camldebug-mouse-select-subexpression))))

;; Handle data from a `match' instruction.
(defun camldebug-handle-match (proc match)
  (save-excursion
    (set-buffer (process-buffer proc))
    (if (equal (car camldebug-expected-pattern) match)
	(progn (setq camldebug-duplicate t)
	       (setq camldebug-hidden t)
	       (camldebug-insert-in-value-buffer proc camldebug-new-value)
	       (camldebug-insert-in-value-buffer proc match)))))

;; Handle data from a `match' instruction.
(defun camldebug-handle-print (proc match)
  (save-excursion
    (set-buffer (process-buffer proc))
    (if (equal (car camldebug-expected-pattern) match)
	(setq camldebug-hidden t))
    (setq camldebug-duplicate t)
    (camldebug-insert-in-value-buffer proc camldebug-new-value)))

;; End of data.
(defun camldebug-handle-received-value (proc)
  (let (current-value)
    (save-excursion
      (set-buffer (process-buffer proc))
      (if camldebug-duplicate
	  (progn
	    (setq camldebug-duplicate nil)
	    (setq camldebug-hidden nil)
	    (set-buffer camldebug-value-buffer)
	    (let ((beg (point-min-marker))
		  (end (point-max-marker)))
	      (unwind-protect
		  (progn
		    (widen)
		    (goto-char (point-max))
		    (re-search-backward camldebug-new-value-regexp nil t)
		    (if (re-search-forward (concat camldebug-new-value
						   "\\([^ ]*\\) :")
					   nil t)
			(let* ((name (buffer-substring (match-beginning 1)
						       (match-end 1)))
			       (value (cons name nil))
			       (buffer-read-only nil))
			  (if (camldebug-find-value value)
			      (delete-region (progn (forward-line -1)
						    (point))
					     (point-max))
			    (setq current-value (cons name nil))
			    (goto-char (point-min))
			    (re-search-forward camldebug-new-value)
			    (re-search-backward camldebug-new-value)
			    (insert (concat name "\n"))
			    (if (= beg (point-min))
				(set-marker end (1- (point))))))
		      (setq current-value (cdr camldebug-expected-pattern)))
		    (setq camldebug-value-list
			  (append camldebug-value-list (list current-value))))
		(narrow-to-region beg end))))))
    (if (and camldebug-expected-pattern
	     (equal current-value
		    (cdr camldebug-expected-pattern)))
	(progn
	  (set-buffer (process-buffer proc))
	  (set-buffer camldebug-value-buffer)
	  (setq camldebug-expected-pattern nil)
	  (widen)
	  (goto-char (point-max))
	  (re-search-backward camldebug-new-value-regexp)
	  (re-search-forward camldebug-new-value)
	  (narrow-to-region (point) (1- (point-max)))
	  (display-buffer (current-buffer))))))

;; Empty the value buffer.
(defun camldebug-empty-value-buffer (proc)
  (save-excursion
    (set-buffer (process-buffer proc))
    (if camldebug-value-buffer
	(progn
	  (set-buffer camldebug-value-buffer)
	  (let ((buffer-read-only nil))
	    (setq camldebug-value-list nil)
	    (erase-buffer)
	    (narrow-to-region (point) (point)))))))

;; Insert a string at the end of the value buffer.
(defun camldebug-insert-in-value-buffer (proc string)
  (save-excursion
    (set-buffer (process-buffer proc))
    (if (and camldebug-value-buffer	; Initialize buffer if necessary.
	    (buffer-name camldebug-value-buffer)) 
	(set-buffer camldebug-value-buffer)
      (setq camldebug-value-buffer
	    (get-buffer-create (concat "*camldebug-"
				       camldebug-file-name
				       "(values)*")))
      (set-buffer camldebug-value-buffer)
      (camldebug-value-mode)
      (setq camldebug-process-buffer (process-buffer proc))
      (camldebug-empty-value-buffer proc))
    (let ((beg (point-min-marker))
	  (end (point-max-marker)))
      (unwind-protect
	  (let ((buffer-read-only nil))
	    (widen)
	    (goto-char (point-max))
	    (insert string))
	(narrow-to-region beg end)))))

;; Number of the value we are on.
;; nil if not on a value (on the variable list).
(defun camldebug-value-number ()
  (save-excursion
    (save-restriction
      (widen)
      (let ((n -1))
	(while (re-search-backward camldebug-new-value-regexp nil t)
	  (setq n (1+ n)))
	(if (eq n -1)
	    nil
	  n)))))

;; Search value number in value list.
(defun camldebug-find-value (value)
  (let ((list camldebug-value-list)
	(n 1))
    (while (and list
		(not (equal (car list) value)))
      (setq list (cdr list)
	    n (1+ n)))
    (and list
	 n)))

;; Select a value.
;; Ask the debugger if necessary.
(defun camldebug-select-value (value &optional buffer)
  (if buffer
      (set-buffer buffer))
  (let ((n (camldebug-find-value value)))
    (if n
	(progn
	  (widen)
	  (goto-char (point-min))
	  (re-search-forward camldebug-new-value-regexp nil t n)
	  (narrow-to-region
	   (point)
	   (save-excursion
	     (and (re-search-forward camldebug-new-value-regexp nil 0)
		  (re-search-backward camldebug-new-value))
	     (1- (point))))
	  (set-window-point (display-buffer (current-buffer)) (point)))
      (let (command
	    (variable-name (car value)))
	(if (equal variable-name camldebug-accumulator-name)
	    (setq variable-name "*"))
	(if (cdr value)
	    (setq command (concat "match " variable-name " "
				  (camldebug-pattern-of-path (cdr value))))
	  (setq command (concat "print " variable-name)))
	(setq camldebug-expected-pattern (cons (concat command "\n")
					       value))
	(and camldebug-process-buffer
	     (setq current-camldebug-buffer camldebug-process-buffer))
	(save-excursion
	  (set-buffer current-camldebug-buffer)
	  (camldebug-call command))))))

;; Go to the variable list.
(defun camldebug-variable-list (&optional other-window)
  (interactive)
  (set-buffer current-camldebug-buffer)
  (or camldebug-value-buffer
      (error "No variable selected."))
  (if other-window
      (switch-to-buffer-other-window camldebug-value-buffer)
    (switch-to-buffer camldebug-value-buffer))
  (widen)
  (goto-char (point-min))
  (or (re-search-forward camldebug-new-value-regexp nil t)
      (error "No variable selected."))
  (re-search-backward camldebug-new-value)
  (narrow-to-region (point-min) (1- (point)))
  (goto-char (point-min)))

(defun camldebug-variable-list-other-window ()
  (interactive)
  (camldebug-variable-list t))

;; Go to the next variable.
(defun camldebug-next-variable ()
  (interactive)
  (if (camldebug-value-number)
      nil
    (progn
      (end-of-line)
      (if (> (forward-line 1) 0)
	  (goto-char (point-min))))))

;; Go to the previous variable.
(defun camldebug-previous-variable ()
  (interactive)
  (if (camldebug-value-number)
      nil
    (if (< (forward-line -1) 0)
	(progn
	  (goto-char (point-max))
	  (beginning-of-line)))))

;; Go up the value.
(defun camldebug-value-up ()
  (interactive)
  (let ((n (camldebug-value-number)))
    (if n
	(let ((current-value (nth n camldebug-value-list)))
	  (if (cdr current-value)
	      (camldebug-select-value (cons (car current-value)
					    (cdr (cdr current-value))))
	    (camldebug-variable-list))))))

;; Go to the top of the value.
(defun camldebug-variable-top ()
  (interactive)
  (let ((n (camldebug-value-number)))
    (if n
	(camldebug-select-value (cons (car (nth n camldebug-value-list))
				      nil)))))

;; Select a subexpression.
(defun camldebug-select-subexpression (&optional num)
  (interactive "P")
  (let ((n (camldebug-value-number)))
    (if n
	(let ((current-value (nth n camldebug-value-list)))
	  (save-excursion
	    (goto-char (point-min))
	    (if (cdr current-value)
		(forward-line 1))
	    (camldebug-beginning-of-value))
	  (if (< (point) camldebug-beginning)
	      (error "Not on the value."))
	  (camldebug-select-value (cons (car current-value)
					(append (camldebug-initialize-path num)
						(cdr current-value)))))
      (camldebug-select-value (cons (buffer-substring
				     (progn (beginning-of-line)
					    (point))
				     (progn (end-of-line)
					    (point)))
				    nil)))))

(defun camldebug-mouse-select-subexpression (event)
  (interactive "@e")
  (let ((pos (if (fboundp 'event-point)
		 (event-point event)
	       (posn-point (event-start event)))))
    (if pos
	(save-excursion
	  (goto-char pos)
	  (camldebug-select-subexpression)))))

;; Identifier.
(defvar camldebug-not-extended-letter "[^0-9'_A-Za-z\300-\326\330-\366\370-\377]")
(defvar camldebug-letter "[A-Za-z\300-\326\330-\366\370-\377]")
(defvar camldebug-extended-letter "[0-9'A-Za-z\300-\326\330-\366\370-\377]")
(defvar camldebug-identifier
  (concat camldebug-letter
	  "\\(" "_?" camldebug-extended-letter "\\)*"))

;; Find the beginning of a value.
(defvar camldebug-beginning nil)

(defun camldebug-beginning-of-value ()
  (re-search-forward ".[^=]*=[ \n]*")
  (setq camldebug-beginning (point)))

;; Look whether in a string.
;; Go to the beginning of a string.
(defvar camldebug-in-string "\\([^\"\\\n]\\|\\\\.\\)*")
(defvar camldebug-in-char   "\\([^`\\\n]\\|\\\\.\\)*")
(defvar camldebug-outside   "[^\"`]*")
(defvar camldebug-string (concat "\"" camldebug-in-string "\""))
(defvar camldebug-char (concat "`" camldebug-in-char "`"))

(defvar camldebug-sequence (concat "^"
				   "\\(" camldebug-outside
				   "\\(" camldebug-string
				   "\\|" camldebug-char "\\)"
				   "\\)*"
				   camldebug-outside
				   "\\\\?"))

(defvar camldebug-no-quote "\\([^\\]\\|^\\)\\(\\\\\\\\\\)*")

(defvar camldebug-beginning-of-string
      (concat "\\(" camldebug-no-quote "\\)"
	      "\\(" "`" camldebug-in-char "\\|" "\"" camldebug-in-string "\\)"))

(defun camldebug-in-string ()
  (save-excursion
    (let ((start (point)))
      (re-search-backward camldebug-sequence)
      (not (eq start (match-end 0))))))


(defun camldebug-beginning-of-string ()
  (re-search-backward camldebug-beginning-of-string)
  (goto-char (match-end 1)))

;; Go to the beginning of current subexpression.
(defun camldebug-beginning-of-current-subexpression ()
  (if (looking-at "\\.\\.?\\.?$")
      (error "On ellipsis !!!"))
  (if (eolp)
      (forward-char -1))
  (cond
   ((camldebug-in-string)
    (camldebug-beginning-of-string))
   ((looking-at "[[({]"))
   ((looking-at "[])},;]")
    (camldebug-beginning-of-set))
   ((looking-at "|")
    (if (looking-at "|]")
	(camldebug-beginning-of-set)
      (forward-char -1)))
   (t
    (if (looking-at "=")
	(re-search-backward "[[|{(=;,\n]")
      (re-search-backward (concat "[[|{(=;,\n]" "\\|" camldebug-letter " ")))
    (re-search-forward "[^$] *")
    (if (looking-at camldebug-letter)
	(let ((old-point (point)))
	  (cond ((and (save-excursion (re-search-backward "; *" nil t))
		      (equal (match-end 0) old-point))
		 (camldebug-beginning-of-set)
		 (or (looking-at "{")
		     (goto-char old-point)))
		((progn
		    (let* ((column (current-column))
			   (spaces (make-string column ? )))
		      (beginning-of-line)
		      (while
			  (looking-at spaces)
			(forward-line -1))
		      (move-to-column (1- column))
		      (looking-at "{"))))
		(t
		 (goto-char old-point))))))))

;; Skip single-lined parenthesized expression.
(defun camldebug-skip-parenthesized-expression ()
  (while
      (progn
	(re-search-forward "[]})[{(]\\|$")
	(and (not (eolp))
	     (save-excursion (forward-char -1)
			     (looking-at "[[{(]"))))
    (camldebug-skip-parenthesized-expression)))

;; Go to the beginning of a structure, a list, a vector or a tuple.
(defun camldebug-beginning-of-set (&optional n)
  (or n
      (setq n 0))
  (re-search-backward "[]})[{(;,]\\|^")
  (cond ((looking-at "[]})]")
	 (camldebug-beginning-of-set)
	 (camldebug-beginning-of-set n))
	((looking-at "[[{(]")
	 n)
	((looking-at "[;,]")
	 (camldebug-beginning-of-set (1+ n)))
	((looking-at " ")
	 (re-search-forward " *")
	 (let ((column (1- (current-column))))
	   (while (progn (forward-line -1)
			 (move-to-column column)
			 (if (looking-at " [^ ]")
			     (setq n (1+ n)))
			 (looking-at " "))))
	 (setq n (1+ n))
	 (cond ((looking-at "[[{(]")
		n)
	       ((looking-at "|")
		(forward-char -1)
		n)
	       (t
		(camldebug-beginning-of-set n))))
	(t
	 n)))

;; Compute path.
(defun camldebug-initialize-path (&optional num)
  (save-excursion
    (camldebug-beginning-of-current-subexpression)
    (if (looking-at "\\.")
	(progn
	  (setq num (cond ((null num)
			   (setq num (read-string "Index: "))
			   (or (string-match "-?[0-9]+" num)
			       (error "The index must be a number."))
			   (string-to-int num))
			  ((listp num)
			   (car num))
			  ((eq '- num)
			   -1)
			  (num)))
	  (if (< num 0)
	      (error "The index must be positive."))
	  (camldebug-beginning-of-set)
	  (cons num
		(camldebug-build-path)))
      (camldebug-build-path))))

(defun camldebug-build-path ()
  (interactive)
  (let (path)
    (while (not (eq camldebug-beginning (point)))
					; End of path : beginning of value.
      (re-search-backward (concat "\\(" "=" "\\|" camldebug-letter "\\)" "[\n ]"
				  "\\|" "[^ \n]")
			  nil t)
      (cond ((looking-at "=")		; In structure.
	     (re-search-backward (concat camldebug-not-extended-letter
					 "\\(" camldebug-identifier "__" "\\)?"
					 "\\(" camldebug-identifier "\\)"))
	     (let ((label (buffer-substring (match-beginning 3)
					    (match-end 3))))
	       (or (looking-at "{")
		   (camldebug-beginning-of-set))
	       (setq path (cons label path))))
	    ((looking-at camldebug-letter) ; In header.
	     (camldebug-beginning-of-current-subexpression)
	     (setq path (cons nil path)))
	    (t				; In list, vector, tuple or parenthesize.
	     (forward-char 1)
	     (let ((count (camldebug-beginning-of-set)))
	       (cond ((looking-at "\\[") ; In list or vector
		      (setq path (cons count path)))
		     ((looking-at "(")	; In tuple or parenthesize.
		      (if (or
			   (save-excursion
			     (let ((column (1+ (current-column))))
			       (while (and (eq (forward-line 1) 0)
					   (progn (re-search-forward " *")
						  (> (current-column)
						     column))))
			       (eq column (current-column))))  
			   (save-excursion
			     (forward-char 1)
			     (while
				 (progn
				   (re-search-forward "[,)({[]\\|$")
				   (not (or (eolp)
					    (save-excursion
					      (forward-char -1)
					      (looking-at "[,)]")))))
			       (camldebug-skip-parenthesized-expression))
			     (forward-char -1)
			     (looking-at ",")))
			  (setq path(cons count path)))))))))
    (nreverse path)))			; Tuple.
		      
  
;; Convert path to pattern.
(defvar camldebug-value-name "value")

(defun camldebug-pattern-of-path (path)
  (let ((pattern camldebug-value-name) current)
    (while path
      (setq current (car path)
	    path (cdr path)
	    pattern (cond ((stringp current) ; Structure.
			   (concat "{" current "=" pattern "}"))
			  ((not current) ; Header
			   (concat ">" pattern))
			  (t		; Others
			   (concat (format "#%d" current) pattern)))))
    pattern))

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
		(let ((kind (aref string 2)))
		  (cond
		   ((eq kind ?M)	; Insert mark
		    (let* ((first-colon (string-match ":" string 3))
			   (second-colon
			    (string-match ":" string (1+ first-colon))))
		      (setq camldebug-last-frame
			    (cons (substring string 3 first-colon)
				  (cons (string-to-int
					 (substring string
						    (1+ first-colon)
						    second-colon))
					(equal "before"
					       (substring string
							  (1+ second-colon)
							  end))))))
		    (setq camldebug-last-frame-displayed-p nil)
		    (camldebug-empty-value-buffer proc))
		   ((eq kind ?H)	; Hide mark
		    (setq camldebug-last-frame nil)
		    (setq camldebug-last-frame-displayed-p t)
		    (camldebug-remove-current-event)
		    (camldebug-empty-value-buffer proc))
		   ((eq kind ?f)	; Empty value cache.
		    (camldebug-empty-value-buffer proc))
		   ((eq kind ?p)	; Print.
		    (camldebug-handle-print proc
					    (substring string 3 (1+ end))))
		   ((eq kind ?m)	; Match.
		    (camldebug-handle-match proc
					    (substring string 3 (1+ end))))
		   ((eq kind ?e)	; End of print or match.
		    (camldebug-handle-received-value proc)))
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
	  (if camldebug-hidden
	      nil
	    (goto-char (process-mark proc))
	    (setq start (point))
	    (insert-before-markers string)
	    (set-marker (process-mark proc) (point))
	    (camldebug-maybe-delete-prompt)
	    ;; Check for a filename-and-character number.
	    (camldebug-display-frame
	     ;; Don't display the specified file unless (1) point is
	     ;; at or after the position where output appears and (2)
	     ;; this buffer is on the screen.
	     (or output-after-point
		 (not (get-buffer-window (current-buffer))))
	     ;; Display a file only when a new
	     ;; filename-and-character-number appears.
	     t)
	    (and camldebug-expected-pattern
		 (not (equal string ""))
		 (display-buffer (current-buffer))))
	  (if camldebug-duplicate
	      (camldebug-insert-in-value-buffer proc string)))
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
	 (camldebug-empty-value-buffer proc)
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
       (progn (camldebug-display-line (car camldebug-last-frame)
				      (car (cdr camldebug-last-frame))
				      (cdr (cdr camldebug-last-frame)))
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

;;; Source buffers.

(defvar camldebug-source-map nil)

(if camldebug-source-map
    nil
  (setq camldebug-source-map (make-sparse-keymap))
  (define-key camldebug-source-map "\M-e"
    'camldebug-variable-list-other-window)
  (define-key camldebug-source-map "d" 'camldebug-variable-list-other-window)
  (define-key camldebug-source-map "s" 'camldebug-switch-to-debugger)
  (define-key camldebug-source-map "f" 'camldebug-view-variable)
  (define-key camldebug-source-map "\C-m" 'camldebug-view-variable)
  (define-key camldebug-source-map "b" 'camldebug-break)
  (if camldebug-emacs-19
      (if camldebug-xemacs
	  (progn
	    (define-key camldebug-source-map [button2]
	      'camldebug-mouse-view-variable)
	    (define-key camldebug-source-map [(shift button2)]
	      'camldebug-mouse-break))
	(define-key camldebug-source-map [mouse-2]
	  'camldebug-mouse-view-variable)
	(define-key camldebug-source-map [S-mouse-2]
	  'camldebug-mouse-break))))

(defun camldebug-mouse-view-variable (event)
  (interactive "@e")
  (let ((pos (if (fboundp 'event-point)
		 (event-point event)
	       (posn-point (event-start event)))))
    (if pos
	(save-excursion
	  (goto-char pos)
	  (camldebug-view-variable)))))

(defun camldebug-view-variable ()
  (interactive)
  (or (eobp)
      (save-excursion
	(let ((letter (concat camldebug-extended-letter "\\|_")))
	  (while (looking-at letter)
	    (forward-char -1))
	  (forward-char 1)
	  (if (looking-at (concat "\\(" camldebug-identifier "__" "\\)?"
				  camldebug-identifier))
	      (camldebug-select-value (cons (buffer-substring
					     (match-beginning 0)
					     (match-end 0))
					    nil)
				      (progn
					(set-buffer current-camldebug-buffer)
					camldebug-value-buffer)))))))

(defun camldebug-switch-to-debugger ()
  (interactive)
  (if camldebug-process-buffer
      (switch-to-buffer camldebug-process-buffer)
    (switch-to-buffer-other-window current-camldebug-buffer)))

(if (and camldebug-xemacs window-system)
    (require 'annotations))

(defvar camldebug-current-event nil)

(defun camldebug-remove-events ()
  (camldebug-remove-current-event))

(defun camldebug-remove-current-event ()
  (condition-case nil
      (if camldebug-current-event
	  (if (and camldebug-xemacs window-system)
	      (let ((buffer (extent-buffer (car camldebug-current-event))))
		(if (buffer-name buffer)
		    (save-excursion
		      (set-buffer buffer)
		      (delete-annotation (nth 0 camldebug-current-event))
		      (use-local-map (nth 1 camldebug-current-event))
		      (setq buffer-read-only (nth 2
						  camldebug-current-event)))))
	    (save-excursion
	      (widen)
	      (set-buffer (nth 0 camldebug-current-event))
	      (goto-char (nth 1 camldebug-current-event))
	      (let (buffer-read-only)
		(delete-char 5))
	      (setq buffer-read-only (nth 2 camldebug-current-event)
		    buffer-undo-list (nth 3 camldebug-current-event)
		    buffer-auto-save-file-name (nth 4 camldebug-current-event))
	      (set-buffer-modified-p (nth 5 camldebug-current-event))
	      (use-local-map (nth 6 camldebug-current-event)))))
    (error nil))
  (setq camldebug-current-event nil))

(defun camldebug-set-current-event (pos buffer kind)
  (camldebug-remove-current-event)
  (save-excursion
    (set-buffer buffer)
    (if (and camldebug-xemacs window-system)
	(progn
	  (setq camldebug-current-event
		(list (make-annotation (if kind "<|b|>" "<|a|>")
				       pos 'text buffer)
		      (current-local-map)
		      buffer-read-only))
	  (setq buffer-read-only t)
	  (set-annotation-face (car camldebug-current-event)
			       'camldebug-current-event))
      (widen)
      (goto-char pos)
      (setq camldebug-current-event
	    (list buffer
		  pos
		  buffer-read-only
		  buffer-undo-list
		  buffer-auto-save-file-name
		  (buffer-modified-p)
		  (current-local-map)))
      (setq buffer-read-only t
	    buffer-auto-save-file-name nil)
      (let (buffer-read-only)
	(insert (if kind "<|b|>" "<|a|>"))
	(if (and camldebug-emacs-19 window-system)
	    (overlay-put (make-overlay pos (point))
			 'face 'camldebug-current-event)))
      (set-buffer-modified-p nil))
    (use-local-map camldebug-source-map)))

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
		    (process-mark
		     (get-buffer-process current-camldebug-buffer))))
	    (progn
	      (set-marker (car camldebug-delete-prompt-marker) nil)
	      (setq camldebug-delete-prompt-marker nil))))))

(defun camldebug-break ()
  "Set CDB breakpoint at this source point."
  (interactive)
  (let ((file-name (file-name-nondirectory buffer-file-name))
	(pos (1- (point))))
    (process-send-string (get-buffer-process current-camldebug-buffer)
			 (concat "break @ "
				 (camldebug-module-name file-name)
				 " # "
				 pos
				 "\n"))))

(defun camldebug-mouse-break (event)
  (interactive "@e")
  (let ((pos (if (fboundp 'event-point)
		 (event-point event)
	       (posn-point (event-start event)))))
    (if pos
	(save-excursion
	  (goto-char pos)
	  (camldebug-break)))))

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
