;;; caml-search.el --- Searching for caml values in a library
;;; according to their types.

;; Jerome Vouillon, april 1994

;; USAGE:

;; Add the following lines to your .emacs file:

;; (autoload 'caml-search-equality "caml-search"
;;           "Search the list of the function whose type is equal to TYPE." t)
;; (autoload 'caml-search-more-general "caml-search"
;;           "Search the list of the function whose type is more general than TYPE." t)
;; (autoload 'caml-search-less-general "caml-search"
;;           "Search the list of the function whose type is less general TYPE." t)


(require 'caml)

;;{{{ Which emacs ?

(defvar caml-search-emacs-19
  (not (string-lessp emacs-version "19")))

(defvar caml-search-xemacs (string-match "\\(Lucid\\|Xemacs\\)" emacs-version))

;;}}}
;;{{{ Variables

(defvar caml-search-files-alist
  '((""
     "."
     "/usr/local/lib/caml-light"))
  "*A list which determines which files should be used for a given
buffer.  This is not really an association list, in that all elements
are checked.  The CAR of each element of this list is a pattern
against which the buffer's file name is compared; if it matches, then
the CDR of the list should be the list of files to use.  If more than
one element of this list matches the buffer's file name, then all of
the associated files will be used.  Earlier ones will be searched
first.

If the CAR of elements of this list are strings, then they are treated
as regular-expressions against which the file is compared (like the
auto-mode-alist).  If they are not strings, then they are evaluated.
If they evaluate to non-nil, then the current buffer is considered to
match.")

(defvar caml-search-prog-name "camlsearch"
  "*Name of the search program")

(defvar caml-search-write-module-names t
  "*Non-nil means precise the module name for globale names")

(defvar caml-search-no-unit t
  "*Non-nil means use the -nounit option")

(defvar caml-search-mode-hook nil
  "Call upon entry in caml-search-mode")

(defvar caml-search-insert-line-hook nil
  "Call after each insertion of a line in then caml-search buffer")

(defvar caml-search-errors-buffer "*camlsearch-errors*"
  "Name of the buffer where the errors are displayed")

(defvar caml-search-buffer "*camlsearch*"
  "Name of the search buffer")

;;}}}
;;{{{ Keymap

(defvar caml-search-mode-map nil
  "Keymap for caml-search-mode.")

(if caml-search-mode-map
    ()
  (setq caml-search-mode-map (make-sparse-keymap))
  (define-key caml-search-mode-map "t" 'caml-search-toggle-truncate-lines)
  (define-key caml-search-mode-map "f" 'caml-search-find-interface)
  (define-key caml-search-mode-map "i" 'caml-search-find-implementation)
  (define-key caml-search-mode-map "a" 'caml-search-interrupt)
  (define-key caml-search-mode-map "e" 'caml-search-equality)
  (define-key caml-search-mode-map "m" 'caml-search-more-general)
  (define-key caml-search-mode-map "l" 'caml-search-less-general)
  (define-key caml-search-mode-map "q" 'bury-buffer)
  (if caml-search-emacs-19
      (if caml-search-xemacs
	  (define-key caml-search-mode-map 
	    [button2] 'caml-search-select-with-mouse)
	(define-key caml-search-mode-map
	  [mouse-2] 'caml-search-select-with-mouse))))

(define-key caml-mode-map "\C-c\C-se" 'caml-search-equality)
(define-key caml-mode-map "\C-c\C-sm" 'caml-search-more-general)
(define-key caml-mode-map "\C-c\C-sl" 'caml-search-less-general)

;;}}}
;;{{{ Mode

(defun caml-search-mode ()
  "Major mode for searching Caml functions in a library according to their type.

Commands:
\\{caml-search-mode-map}

Variables:
  caml-search-files-alist
    A list which determines which files should be used for a given
    buffer.  This is not really an association list, in that all elements
    are checked.  The CAR of each element of this list is a pattern
    against which the buffer's file name is compared; if it matches, then
    the CDR of the list should be the list of files to use.  If more than
    one element of this list matches the buffer's file name, then all of
    the associated files will be used.  Earlier ones will be searched
    first.
    If the CAR of elements of this list are strings, then they are treated
    as regular-expressions against which the file is compared (like the
    auto-mode-alist).  If they are not strings, then they are evaluated.
    If they evaluate to non-nil, then the current buffer is considered to
    match.

  caml-search-prog-name
    Name of the search program

  caml-search-write-module-names
    Non-nil means precise the module name for globale names

  caml-search-no-unit
    Non-nil means use the -nounit option
"
  (setq major-mode 'caml-search-mode)
  (setq mode-name "Camlsearch")
  (use-local-map caml-search-mode-map)
  (condition-case nil
      (progn
	(require 'mode-motion)
	(setq mode-motion-hook 'mode-motion-highlight-line))
    (error nil))
  (run-hooks 'caml-search-mode-hook))

;;}}}
;;{{{ Internal variables

(defvar caml-search-string "")

(defvar caml-search-files nil)

;;}}}
;;{{{ Mouse and highlight

(if (and caml-search-emacs-19 window-system)
    (progn
      ;; Select faces
      (make-face 'caml-search-name-face)
      (or (face-differs-from-default-p 'caml-search-name-face)
	  (copy-face 'bold 'caml-search-name-face))
      (make-face 'caml-search-depth-face)
      (or (face-differs-from-default-p 'caml-search-depth-face)
	  (copy-face 'italic 'caml-search-depth-face))
      ;; Install highlight
      (if caml-search-xemacs
	  (progn
	    (fset 'caml-search-make-extent (symbol-function 'make-extent))
	    (fset 'caml-search-set-extent-property
		  (symbol-function 'set-extent-property)))
	(fset 'caml-search-make-extent (symbol-function 'make-overlay))
	(fset 'caml-search-set-extent-property
	      (symbol-function 'overlay-put)))
      (add-hook 'caml-search-insert-line-hook 'caml-search-highlight-line)))

(defun caml-search-highlight-line ()
  (save-excursion
    (let ((data (match-data)))
      (beginning-of-line 0)
      (if (looking-at "\\([^ ]*\\) .*:")
	  (caml-search-set-extent-property
	   (caml-search-make-extent (match-beginning 1)
				    (match-end 1))
	   'face
	   'caml-search-name-face)
	(caml-search-set-extent-property
	 (caml-search-make-extent (point)
				  (progn (end-of-line) (point)))
	 'face
	 'caml-search-depth-face))
      (store-match-data data))))

(defun caml-search-select-with-mouse (event)
  (interactive "@e")
  (let ((pos (if (fboundp 'event-point)
		 (event-point event)
	       (posn-point (event-start event)))))
    (if pos
	(save-excursion
	  (goto-char pos)
	  (caml-search-find-interface nil)))))

;;}}}
;;{{{ Search functions

(defun caml-search-equality (type)
  "Search the list of the function whose type is equal to TYPE."
  (interactive "sType : ")
  (caml-search-internal "-e" type))

(defun caml-search-more-general (type)
  "Search the list of the function whose type is more general than TYPE."
  (interactive "sType : ")
  (caml-search-internal "-m" type))

(defun caml-search-less-general (type)
  "Search the list of the function whose type is less general TYPE."
  (interactive "sType : ")
  (caml-search-internal "-l" type))

;;}}}
;;{{{ Interface with the process

(defun caml-search-internal (kind type)
  (let ((process-connection-type nil)
	(thisdir default-directory)
	(buffer (get-buffer-create caml-search-buffer))
	(args (append (list kind type)
		      (caml-search-buffer-files-list))))
    (if (get-process "camlsearch")
	(progn
	  (set-process-sentinel (get-process "camlsearch") nil)
	  (delete-process "camlsearch")))
    (setq caml-search-string "")
    (setq caml-search-files nil)
    (caml-search-erase-errors-buffer)
    (set-buffer buffer)
    (setq default-directory thisdir)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert (concat "Type : " type "\n\n")))
    (caml-search-mode)
    (setq buffer-read-only t)
    (setq mode-name "Camlsearch : running")
    (if (not (equal buffer (window-buffer)))
	(switch-to-buffer-other-window buffer))
    (let ((proc  (apply 'start-process
			(append (list "camlsearch"
				      buffer
				      caml-search-prog-name
				      "-x")
				(if caml-search-write-module-names
				    nil
				  '("-s"))
				(if caml-search-no-unit
				    '("-nounit")
				  nil)
				args))))
      (process-mark proc)
      (set-process-filter proc 'caml-search-filter)
      (set-process-sentinel proc 'caml-search-sentinel))))

(defun caml-search-sentinel (proc string)
  (let ((data (match-data)))
    (save-excursion
      (set-buffer (process-buffer proc))
      (or (string-match "^\\(finished\\|interrupt\\)\n$" string)
	  (progn (caml-search-write-error string)
		 (error "Error during search.")))
      (let ((buffer-read-only nil))
	(goto-char (point-max))
	(insert "\nDone.\n"))
      (setq mode-name "Camlsearch"))
    (store-match-data data)))

(defun caml-search-insert-line (proc string)
  (save-excursion
    (set-buffer (process-buffer proc))
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (insert string)
      (run-hooks 'caml-search-insert-line-hook))))

(defun caml-search-filter (proc string)
  (let ((string (concat caml-search-string string))
	(data (match-data)))
    (while (string-match "^(\\([^)]*\\)) \\([^\n]*\n\\)\\|\\(^Depth .*\n\\)\\|\\(^.*\n\\)" string)
      (cond ((match-beginning 4)
	     (caml-search-write-error (substring string
						 (match-beginning 4)
						 (match-end 4))))
	    ((match-beginning 3)
	     (caml-search-insert-line proc (substring string
						      (match-beginning 3)
						      (match-end 3)))
	     (setq caml-search-files
		   (nconc caml-search-files (list nil))))
	    (t
	     (caml-search-insert-line proc (substring string
						      (match-beginning 2)
						      (match-end 2)))
	     (setq caml-search-files
		   (nconc caml-search-files
			  (list (substring string
					   (match-beginning 1)
					   (match-end 1)))))))
      (setq string (substring string (match-end 0))))
    (setq caml-search-string string)
    (store-match-data data)))

(defun caml-search-interrupt ()
  "Interrupt the search."
  (interactive)
  (interrupt-process))

;;}}}
;;{{{ Errors

(defun caml-search-erase-errors-buffer ()
  (let ((buffer (get-buffer caml-search-errors-buffer)))
    (if buffer
	(save-excursion
	  (set-buffer buffer)
	  (set-buffer-modified-p nil)
	  (kill-buffer buffer)))))

(defun caml-search-write-error (string)
  (switch-to-buffer (get-buffer-create caml-search-errors-buffer))
  (goto-char (point-max))
  (insert string))

;;}}}
;;{{{ Find file

(defun caml-search-find-interface (arg)
  "Load the file where the function is declared."
  (interactive "P")
  (let* ((base (caml-search-get-filename))
	 (interface (concat base ".mli"))
	 (implementation (concat base ".ml")))
    (caml-search-search-value (cond ((file-exists-p interface) interface)
				    ((file-exists-p implementation)
				     implementation)
				    (t (error "File not found")))
			      arg)))

(defun caml-search-find-implementation (arg)
  "Load the file where the function is defined."
  (interactive "P")
  (let* ((base (caml-search-get-filename))
	 (implementation (concat base ".ml")))
    (caml-search-search-value (if (file-exists-p implementation)
				  implementation
				(error "File not found"))
			      arg)))

(defun caml-search-get-filename ()
  (let ((line (caml-search-current-line))
	filename)
    (if (or (< line 0)
	    (>= line (length caml-search-files)))
	(error "No value on this line"))
    (if (setq filename (nth line caml-search-files))
	(substring filename 0 -3)
      (error "No value on this line"))))

(defun caml-search-search-value (file arg)
  (let ((name (caml-search-get-value-name)))
    (if arg
	(find-file file)
      (find-file-other-window file))
    (goto-char (point-min))
    (if (re-search-forward (concat "^\\(value\\|let\\([ \t]+rec\\)?\\|\\(  \\)?and\\)[ \t]+"
				   "\\(prefix[ \t]+\\)?"
				   name
				   "[^a-zA-Z0-9_]")
			   nil
			   t)
	(beginning-of-line))))

;;}}}
;;{{{ Various functions

(defun caml-search-toggle-truncate-lines (&optional p)
  "Toggles the values of truncate-lines."
  (interactive "P")
  (setq truncate-lines
	(cond ((null p) (not truncate-lines))
	      ((= 0 (prefix-numeric-value p)) nil)
	      (t t)))
  (recenter))

(defun caml-search-current-line ()
  "Return the vertical position of point in the selected buffer.  
          Top line is -2.  Counts each text line only once, even if it wraps."
  (+ (count-lines (point-min) (point))
     (if (= (current-column) 0) 1 0)
     -3))

(defun caml-search-get-value-name ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at "\\([^_]+_\\)+_\\([^ ]+\\)")
	(buffer-substring (match-beginning 2) (match-end 2))
      (if (looking-at "\\([^ ]+\\)")
	  (buffer-substring (match-beginning 1) (match-end 1))
	""))))

(defun caml-search-buffer-files-list ()
  "Returns a list of the interface files  which should be used for 
the current buffer."
  (let (result expression)
    (let ((key (or buffer-file-name
		   (concat default-directory (buffer-name))))
	  (alist caml-search-files-alist))
      (while alist
	(setq expression (car (car alist)))
	;; If the car of the alist item is a string, apply it as a regexp
	;; to the buffer-file-name.  Otherwise, evaluate it.  If the
	;; regexp matches, or the expression evaluates non-nil, then this
	;; item in tag-table-alist applies to this buffer.
	(if (if (stringp expression)
		(string-match expression key)
	      (condition-case nil
		  (eval expression)
		(error nil)))
	    ;; Now evaluate the cdr of the alist item to get the name of
	    ;; the tag table file.
	      (setq result (append (cdr (car alist)) result)))
	(setq alist (cdr alist))))
    result))

;;}}}

(provide 'caml-search)

;; Local variables:
;; folded-file: t
;; end:
