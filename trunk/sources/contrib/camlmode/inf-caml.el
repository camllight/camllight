;;; inf-caml.el --- run the Caml toplevel in an Emacs buffer

;;itz 04-05-96 changed this to use derived.el. That probably means
;;it no longer works with Emacs 18. Tough.
;; Xavier Leroy, july 1993.

(require 'comint)
(require 'derived)
(require 'caml)

(defvar inferior-caml-program "camllight"
  "*Program name for invoking an inferior Caml from Emacs.")

(defvar inferior-caml-mode-syntax-table
  (make-syntax-table caml-mode-syntax-table)
  "Syntax table for inferior caml mode.

A separate copy of caml-mode-syntax-table is used, so that
inferior-caml-mode buffers aren't affected by the tricks we must
occasionally play with caml-mode-syntax-table.")

(define-derived-mode inferior-caml-mode comint-mode "Inferior Caml"

  "Major mode for interacting with an inferior Caml process.

Runs a Caml toplevel as a subprocess of Emacs, with I/O through an
Emacs buffer. A history of input phrases is maintained. Phrases can
be sent from another buffer in Caml mode.

\\{inferior-caml-mode-map}"

  (setq comint-prompt-regexp "^#")
  (setq comint-get-old-input 'inferior-caml-get-old-input)
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
  (setq parse-sexp-ignore-comments nil))

(defun run-caml (cmd)
  "Run an inferior Caml process.
Input and output via buffer `*inferior-caml*'."
  (interactive (list (read-from-minibuffer "Caml command to run: "
                                           inferior-caml-program)))
  (setq inferior-caml-program cmd)
  (if (not (comint-check-proc "*inferior-caml*"))
      (let ((cmdlist (inferior-caml-args-to-list cmd))
            (process-connection-type nil))
	(set-buffer (apply (function make-comint)
			   "inferior-caml" (car cmdlist) nil (cdr cmdlist)))
	(inferior-caml-mode)))
  (switch-to-buffer "*inferior-caml*"))

(defun inferior-caml-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (inferior-caml-args-to-list (substring string (+ 1 where)
							(length string)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if (null pos)
		   nil
		 (inferior-caml-args-to-list (substring string pos
							(length string)))))))))

(defun caml-eval-region (start end)
  "Send the current region to the inferior Caml process."
  (interactive"r")
  (comint-send-region "*inferior-caml*" start end)
  (comint-send-string "*inferior-caml*" "\n"))

(defun caml-eval-phrase ()
  "Send the current Caml phrase to the inferior Caml process."
  (interactive)
  (save-excursion
    (caml-mark-phrase)
    (caml-eval-region (point) (mark))))

;;itz 04-05-96
(defun inferior-caml-get-old-input ()
  (re-search-forward comint-prompt-regexp nil t)
  (re-search-backward ";;")
  (re-search-backward (concat comint-prompt-regexp "\\(\\(.\\|\n\\)*\\)\\="))
  (concat (match-string 1) ";;"))

(define-key caml-mode-map "\M-\C-x" 'caml-eval-phrase)
(define-key caml-mode-map "\C-x\C-e" 'caml-eval-phrase)
(define-key caml-mode-map "\C-c\C-r" 'caml-eval-region)

;;; inf-caml.el ends here

(provide 'inf-caml)
