;;; caml.el --- Caml code editing commands for Emacs

;; Xavier Leroy, july 1993.

(defvar caml-mode-map nil
  "Keymap used in Caml mode.")
(if caml-mode-map
    ()
  (setq caml-mode-map (make-sparse-keymap))
  (define-key caml-mode-map "\t" 'caml-indent-command)
  (define-key caml-mode-map "\M-\t" 'caml-unindent-command)
  (define-key caml-mode-map "\M-\C-h" 'caml-mark-phrase)
  (define-key caml-mode-map "\C-x`" 'caml-next-error)
  (define-key caml-mode-map "\177" 'backward-delete-char-untabify))

(defvar caml-mode-syntax-table nil
  "Syntax table in use in Caml mode buffers.")
(if caml-mode-syntax-table
    ()
  (setq caml-mode-syntax-table (make-syntax-table))
  ; backslash is an escape sequence
  (modify-syntax-entry ?\\ "\\" caml-mode-syntax-table)
  ; ( is first character of comment start
  (modify-syntax-entry ?\( "()1" caml-mode-syntax-table)
  ; * is second character of comment start,
  ; and first character of comment end
  (modify-syntax-entry ?*  ". 23" caml-mode-syntax-table)
  ; ) is last character of comment end
  (modify-syntax-entry ?\) ")(4" caml-mode-syntax-table)
  ; backquote is a string-like delimiter (for character literals)
  (modify-syntax-entry ?` "\"" caml-mode-syntax-table)
  ; quote is part of words
  (modify-syntax-entry ?' "w" caml-mode-syntax-table)
  ; ISO-latin accented letters are part of words
  (let ((i 192))
    (while (< i 256)
      (modify-syntax-entry i "w" caml-mode-syntax-table)
      (setq i (1+ i)))))

(defvar caml-mode-indentation 2
  "*Indentation for each extra tab in Caml mode.")

;;; The major mode

(defun caml-mode ()
  "Major mode for editing Caml code.
Tab at the beginning of a line indents this line like the line above.
Extra tabs increase the indentation level.
\\{caml-mode-map}
The variable caml-mode-indentation indicates how many spaces are
inserted for each indentation level."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'caml-mode)
  (setq mode-name "caml")
  (use-local-map caml-mode-map)
  (set-syntax-table caml-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "(* ")
  (make-local-variable 'comment-end)
  (setq comment-end " *)")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+ *")
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'caml-indent-command)
  (run-hooks 'caml-mode-hook))

;;; Indentation stuff

(defun caml-in-indentation ()
  "Tests whether all characters between beginning of line and point
are blanks."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

(defun caml-indent-command ()
  "Indent the current line in Caml mode.
When the point is at the beginning of an empty line, indent this line like
the line above.
When the point is at the beginning of an indented line
\(i.e. all characters between beginning of line and point are blanks\),
increase the indentation by one level.
The indentation size is given by the variable caml-mode-indentation.
In all other cases, insert a tabulation (using insert-tab)."
  (interactive)
  (let* ((begline
          (save-excursion
            (beginning-of-line)
            (point)))
         (current-offset
          (- (point) begline))
         (previous-indentation
          (save-excursion
            (if (eq (forward-line -1) 0) (current-indentation) 0))))
    (cond ((and (bolp)
                (looking-at "[ \t]*$")
                (> previous-indentation 0))
           (indent-to previous-indentation))
          ((caml-in-indentation)
           (indent-to (+ current-offset caml-mode-indentation)))
          (t
           (insert-tab)))))

(defun caml-unindent-command ()
  "Decrease indentation by one level in Caml mode.
Works only if the point is at the beginning of an indented line
\(i.e. all characters between beginning of line and point are blanks\).
Does nothing otherwise."
  (interactive)
  (let* ((begline
          (save-excursion
            (beginning-of-line)
            (point)))
         (current-offset
          (- (point) begline)))
    (if (and (>= current-offset caml-mode-indentation)
             (caml-in-indentation))
        (backward-delete-char-untabify caml-mode-indentation))))

;;; Error processing

;; In Emacs 18, the regexp compilation-error-regexp matches the error 
;; messages produced by camlc. In Emacs 19, none of the regexps in
;; compilation-error-regexp-alist matches. Hence we add one.

(defconst caml-error-regexp
  "\"\\([^,\" \n\t]+\\)\", lines? \\([0-9]+\\)[-:,]"
  "Regular expression matching the error messages produced by camlc.")

(if (boundp 'compilation-error-regexp-alist)
    (or (assoc caml-error-regexp
               compilation-error-regexp-alist)
        (setq compilation-error-regexp-alist
              (cons (list caml-error-regexp 1 2)
               compilation-error-regexp-alist))))

;; A regexp to extract the range info

(defconst caml-error-chars-regexp
  "File.*, line.*, characters \\([0-9]+\\)-\\([0-9]+\\):"
  "Regular expression extracting the character numbers
from an error message produced by camlc.")

;; Wrapper around next-error.

(defvar caml-error-overlay nil)

(defun caml-next-error (&optional arg)
  "Visit next compilation error message and corresponding source code.
This operates on the output from the M-x compile command.
This function works exactly as the next-error function (\\[next-error]),
except that it reads the extra positional information provided by the Caml
compiler in the error message, and puts the point and the mark exactly
around the erroneous program fragment. The erroneous fragment is also
temporarily highlighted if possible.
A prefix arg means ignore the warnings and skip to the next fatal error.
Just C-u as a prefix means reparse the error message buffer and start
at the first error (or warning)."
  (interactive "P")
  (next-error (if (consp arg) arg nil))
  (if (consp arg) (setq arg nil))
  (let ((beg nil) (end nil) (ignore-message nil))
    (save-excursion
      (set-buffer
       (if (boundp 'compilation-last-buffer) 
           compilation-last-buffer      ;Emacs 19
         "*compilation*"))              ;Emacs 18
      (save-excursion
        (goto-char (window-point (get-buffer-window (current-buffer))))
        (if (looking-at caml-error-chars-regexp)
            (setq beg
                  (string-to-int
                   (buffer-substring (match-beginning 1) (match-end 1)))
                  end
                  (string-to-int
                   (buffer-substring (match-beginning 2) (match-end 2)))))
        (if arg
            (progn
              (forward-line 1)
              (setq ignore-message
                    (catch 'exit
                      (while (looking-at "^#")
                        (if (looking-at "^# Warning: ") (throw 'exit t))
                        (forward-line 1))))))))
    (cond (ignore-message
           (caml-next-error arg))
          (beg
           (setq beg (+ (point) beg)
                 end (+ (point) end))
           (goto-char beg)
           (push-mark end t)
           (cond ((fboundp 'make-overlay)
                  (if caml-error-overlay ()
                    (setq caml-error-overlay (make-overlay 1 1))
                    (overlay-put caml-error-overlay 'face 'region))
                  (unwind-protect
                      (progn
                        (move-overlay caml-error-overlay
                                      beg end (current-buffer))
                        (sit-for 60))
                    (delete-overlay caml-error-overlay))))))))

;;; Phrases

(defun caml-mark-phrase ()
  "Put mark at end of this Caml phrase, point at beginning.
The Caml phrase is the one containing the point, if the point is inside
a phrase, or the phrase just before the point, if the point is outside
a phrase.
Phrases are delimited by the final \";;\". It must be at the end of
a line, or followed by at least one blank. \";;\" inside strings
and comments are not properly ignored, and will confuse this function."
  (interactive)
  (let (beg end)
    ; if we're on a blank or on a closing comment,
    ; see if we're between two phrases
    (if (looking-at "[ \t\n)]")
        (progn
          ; skip backward blanks and comments
          (skip-chars-backward " \t\n")
          (while (looking-back "*)")
            (backward-sexp 1)
            (skip-chars-backward " \t\n"))
          ; if we've reached ;;, move inside the previous phrase
          (if (looking-back ";;")
              (backward-char 2))))
    ; find the end of the previous phrase
    (if (re-search-backward ";;\\($\\|[ \t]\\)" nil 'move)
        (forward-char 2))               ;skip the final ;;
    ; skip blanks and comments
    (skip-chars-forward " \t\n")
    (while (looking-at "(\\*")
      (forward-sexp 1)
      (skip-chars-forward " \t\n"))
    ; we've reached the beginning of the current phrase
    (setq beg (point))
    ; skip forward to the end of the phrase
    (or (re-search-forward ";;\\($\\|[ \t]\\)" nil t)
        (error "Unterminated phrase"))
    ; the end of the current phrase is the character following the ;;
    (setq end (match-beginning 1))
    ; now set mark and point
    (push-mark end)
    (goto-char beg)))

(defun looking-back (str)
  "Test whether the string argument appears just before the point"
  (let ((len (length str)))
    (and (> (point) len)
         (string-equal str (buffer-substring (- (point) len) (point))))))

;;; caml.el ends here

(provide 'caml)
