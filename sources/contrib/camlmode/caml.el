;;; caml.el --- Caml code editing commands for Emacs

;; Xavier Leroy, july 1993.

;;indentation code is Copyright (C) 1996 by Ian T Zimmerman <itz@rahul.net>
;;copying: covered by the current FSF General Public License.

(or (and (fboundp 'indent-line-to) (fboundp 'match-string))
    (require 'caml-compat))

;;user customizable variables
(defvar caml-mode-indentation 4
  "*Used for \\[caml-unindent-command].")

(defvar caml-lookback-limit 2000
  "*How far to look back for syntax things in caml mode.")

(defvar caml-max-indent-priority 7
  "*Bounds priority of operators permitted to affect caml indentation.

Priorities are assigned to `interesting' caml operators as follows:

	::		6
	@		5
	,		4
	:= <-		3
	if		2
	;		1
	keywords 	0.")

(defvar caml-begin-indent	2
  "*How many spaces to indent from a begin keyword in caml mode.")
(make-variable-buffer-local 'caml-begin-indent)

(defvar caml-exception-indent	2
  "*How many spaces to indent from a exception keyword in caml mode.")
(make-variable-buffer-local 'caml-exception-indent)

(defvar caml-for-indent	2
  "*How many spaces to indent from a for keyword in caml mode.")
(make-variable-buffer-local 'caml-for-indent)

(defvar caml-fun-indent	4
  "*How many spaces to indent from a fun keyword in caml mode.")
(make-variable-buffer-local 'caml-fun-indent)

(defvar caml-function-indent	9
  "*How many spaces to indent from a function keyword in caml mode.")
(make-variable-buffer-local 'caml-function-indent)

(defvar caml-if-indent	2
  "*How many spaces to indent from a if keyword in caml mode.")
(make-variable-buffer-local 'caml-if-indent)

(defvar caml-let-indent	4
  "*How many spaces to indent from a let keyword in caml mode.")
(make-variable-buffer-local 'caml-let-indent)

(defvar caml-match-indent	6
  "*How many spaces to indent from a match keyword in caml mode.")
(make-variable-buffer-local 'caml-match-indent)

(defvar caml-try-indent	4
  "*How many spaces to indent from a try keyword in caml mode.")
(make-variable-buffer-local 'caml-try-indent)

(defvar caml-type-indent	2
  "*How many spaces to indent from a type keyword in caml mode.")
(make-variable-buffer-local 'caml-type-indent)

(defvar caml-value-indent	2
  "*How many spaces to indent from a value keyword in caml mode.")
(make-variable-buffer-local 'caml-value-indent)

(defvar caml-while-indent	2
  "*How many spaces to indent from a while keyword in caml mode.")
(make-variable-buffer-local 'caml-while-indent)

(defvar caml-::-indent	2
  "*How many spaces to indent from a :: operator in caml mode.")
(make-variable-buffer-local 'caml-::-indent)

(defvar caml-@-indent	1
  "*How many spaces to indent from a @ operator in caml mode.")
(make-variable-buffer-local 'caml-@-indent)

(defvar caml-,-indent	1
  "*How many spaces to indent from a , operator in caml mode.")
(make-variable-buffer-local 'caml-,-indent)

(defvar caml-:=-indent  2
  "*How many spaces to indent from a := operator in caml mode.")
(make-variable-buffer-local 'caml-:=-indent)

(defvar caml-<--indent	2
  "*How many spaces to indent from a <- operator in caml mode.")
(make-variable-buffer-local 'caml-<--indent)

(defvar caml-semi-indent	2
  "*How many spaces to indent from a \; operator in caml mode.")
(make-variable-buffer-local 'caml-semi-indent)

(defvar caml-lb-indent	1
  "*How many spaces to indent from a \[ operator in caml mode.")
(make-variable-buffer-local 'caml-lb-indent)

(defvar caml-lc-indent	1
  "*How many spaces to indent from a \{ operator in caml mode.")
(make-variable-buffer-local 'caml-lc-indent)

(defvar caml-lp-indent	1
  "*How many spaces to indent from a \( operator in caml mode.")
(make-variable-buffer-local 'caml-lp-indent)

(defvar caml-and-extra-indent -4
  "*Extra indent for caml lines starting with the and keyword.
Usually negative.")
(make-variable-buffer-local 'caml-and-extra-indent)

(defvar caml-do-extra-indent -2
  "*Extra indent for caml lines starting with the do keyword.
Usually negative.")
(make-variable-buffer-local 'caml-do-extra-indent)

(defvar caml-done-extra-indent -2
  "*Extra indent for caml lines starting with the done keyword.
Usually negative.")
(make-variable-buffer-local 'caml-done-extra-indent)

(defvar caml-else-extra-indent -2
  "*Extra indent for caml lines starting with the else keyword.
Usually negative.")
(make-variable-buffer-local 'caml-else-extra-indent)

(defvar caml-end-extra-indent -2
  "*Extra indent for caml lines starting with the end keyword.
Usually negative.")
(make-variable-buffer-local 'caml-end-extra-indent)

(defvar caml-in-extra-indent -4
  "*Extra indent for caml lines starting with the in keyword.
Usually negative.")
(make-variable-buffer-local 'caml-in-extra-indent)

(defvar caml-then-extra-indent -2
  "*Extra indent for caml lines starting with the then keyword.
Usually negative.")
(make-variable-buffer-local 'caml-then-extra-indent)

(defvar caml-with-extra-indent -4
  "*Extra indent for caml lines starting with the with keyword.
Usually negative.")
(make-variable-buffer-local 'caml-with-extra-indent)

(defvar caml-|-extra-indent -2
  "*Extra indent for caml lines starting with the | operator.
Usually negative.")
(make-variable-buffer-local 'caml-|-extra-indent)

(defvar caml-rb-extra-indent -1
  "*Extra indent for caml lines statring with ].
Usually negative.")

(defvar caml-rc-extra-indent -1
  "*Extra indent for caml lines starting with }.
Usually negative.")

(defvar caml-electric-indent t
  "*Non-nil means electrically indent lines starting with |, ] or }.

Many people find eletric keys irritating, so you can disable them if
you are one.")

(defvar caml-electric-close-vector t
  "*Non-nil means electrically insert a | before a vector-closing ].

Many people find eletric keys irritating, so you can disable them if
you are one. You should probably have this on, though, if you also
have caml-electric-indent on, which see.")

(defvar caml-font-lock-keywords 
  (list
   ;; Pierre Boulet: Jan 31 1996
   '("[ \t]*\\<:\\>[ \t]*\\(\\(\\sw\\|_\\)+\\)?"
     1 font-lock-type-face)
   '("[^_]\\<\\(let\\( rec\\)?\\|type\\|and\\|value\\)\\>[ \t]*\\(\\(\\sw\\|_\\)+\\|(.*)\\|{.*}\\)"
     3 font-lock-function-name-face)
   '("[^_]\\<\\(a\\(nd\\|s\\)\\|begin\\|close\\|do\\(ne\\|wnto\\)*\\|e\\(lse\\|nd\\|xception\\)\\|f\\(or\\|un\\(ction\\)*\\)\\|i[fn]\\|let\\|m\\(atch\\|utable\\)\\|not\\|o\\(f\\|pen\\|r\\)\\|prefix\\|rec\\|then\\|t\\(o\\|ry\\|ype\\)\\|value\\|w\\(h\\(n\\|ile\\)\\|ith\\)\\)\\>[^_]"
     1 font-lock-keyword-face)
   '("\\(\".*\"\\)" 1 font-lock-string-face)
   '("(\\*\\(.*\\)\\*)" 1 font-lock-comment-face)
   )
  "Additional expressions to highlight in caml mode.")

;;code
(defvar caml-mode-map nil
  "Keymap used in Caml mode.")
(if caml-mode-map
    ()
  (setq caml-mode-map (make-sparse-keymap))
  (define-key caml-mode-map "|" 'caml-electric-pipe)
  (define-key caml-mode-map "}" 'caml-electric-pipe)
  (define-key caml-mode-map "]" 'caml-electric-rb)
  (define-key caml-mode-map "\t" 'caml-indent-command)
  (define-key caml-mode-map [backtab] 'caml-unindent-command)
  (define-key caml-mode-map "\M-\C-h" 'caml-mark-phrase)

;itz 04-21-96 instead of defining a new function, use defadvice
;that way we get out effect even when we do \C-x` in compilation buffer  
;  (define-key caml-mode-map "\C-x`" 'caml-next-error)

  (define-key caml-mode-map "\177" 'backward-delete-char-untabify)
  (define-key caml-mode-map "\C-cb" 'caml-insert-begin-form)
  (define-key caml-mode-map "\C-cf" 'caml-insert-for-form)
  (define-key caml-mode-map "\C-ci" 'caml-insert-if-form)
  (define-key caml-mode-map "\C-cl" 'caml-insert-let-form)
  (define-key caml-mode-map "\C-cm" 'caml-insert-match-form)
  (define-key caml-mode-map "\C-ct" 'caml-insert-try-form)
  (define-key caml-mode-map "\C-cw" 'caml-insert-while-form)
  (define-key caml-mode-map "\C-c\C-\[" 'caml-backward-to-less-indent)
  (define-key caml-mode-map "\C-c\C-\]" 'caml-forward-to-less-indent)
  (define-key caml-mode-map "\C-c\C-q" 'caml-indent-phrase))

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

(defvar caml-mode-abbrev-table nil
  "Abbrev table used for Caml mode buffers.")
(if caml-mode-abbrev-table nil
  (setq caml-mode-abbrev-table (make-abbrev-table))
  (define-abbrev caml-mode-abbrev-table "and" "and" 'caml-abbrev-hook)
  (define-abbrev caml-mode-abbrev-table "do" "do" 'caml-abbrev-hook)
  (define-abbrev caml-mode-abbrev-table "done" "done" 'caml-abbrev-hook)
  (define-abbrev caml-mode-abbrev-table "else" "else" 'caml-abbrev-hook)
  (define-abbrev caml-mode-abbrev-table "end" "end" 'caml-abbrev-hook)
  (define-abbrev caml-mode-abbrev-table "in" "in" 'caml-abbrev-hook)
  (define-abbrev caml-mode-abbrev-table "then" "then" 'caml-abbrev-hook)
  (define-abbrev caml-mode-abbrev-table "with" "with" 'caml-abbrev-hook))

;;; The major mode

(defun caml-mode ()
  "Major mode for editing Caml code.

\\{caml-mode-map}"

  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'caml-mode)
  (setq mode-name "caml")
  (use-local-map caml-mode-map)
  (set-syntax-table caml-mode-syntax-table)
  (setq local-abbrev-table caml-mode-abbrev-table)
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
  ;itz 03-25-96
  (setq before-change-function 'caml-before-change-function)
  (setq caml-last-noncomment-pos nil)
  (setq caml-last-comment-start (make-marker))
  (setq caml-last-comment-end (make-marker))
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

When the point is in the line indentation, compute new indentation
based on caml syntax. Otherwise, indent the line all the way to where
point is."

  (interactive "*")
  (indent-line-to
   (if (caml-in-indentation)
       (caml-compute-final-indent)
     (current-column))))
   
(defun caml-unindent-command ()

  "Decrease indentation by one level in Caml mode.  

Works only if the point is at the beginning of an indented line
\(i.e. all characters between beginning of line and point are
blanks\).  Does nothing otherwise. The unindent size is given by the
variable caml-mode-indentation."

  (interactive "*")
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

(require 'compile)

;; In Emacs 19, the regexps in compilation-error-regexp-alist do not
;; match the error messages when the language is not English.
;; Hence we add a regexp.

(defconst caml-error-regexp
  "^[A-\377]+ \"\\([^\"\n]+\\)\", [A-\377]+ \\([0-9]+\\)[-,:]"
  "Regular expression matching the error messages produced by camlc.")

(if (boundp 'compilation-error-regexp-alist)
    (or (assoc caml-error-regexp
               compilation-error-regexp-alist)
        (setq compilation-error-regexp-alist
              (cons (list caml-error-regexp 1 2)
               compilation-error-regexp-alist))))

;; A regexp to extract the range info

(defconst caml-error-chars-regexp
  ".*, .*, [A-\377]+ \\([0-9]+\\)-\\([0-9]+\\):"
  "Regular expression extracting the character numbers
from an error message produced by camlc.")

;; Wrapper around next-error.

(defvar caml-error-overlay nil)

;;itz 04-21-96 somebody didn't get the documetation for next-error
;;right. When the optional argument is a number n, it should move
;;forward n errors, not reparse.

;itz 04-21-96 instead of defining a new function, use defadvice
;that way we get our effect even when we do \C-x` in compilation buffer  

(defadvice next-error (after caml-next-error activate)
 "Reads the extra positional information provided by the Caml compiler.

Puts the point and the mark exactly around the erroneous program
fragment. The erroneous fragment is also temporarily highlighted if
possible."

 (if (eq major-mode 'caml-mode)
     (let ((beg nil) (end nil))
       (save-excursion
	 (set-buffer
	  (if (boundp 'compilation-last-buffer) 
	      compilation-last-buffer	;Emacs 19
	    "*compilation*"))		;Emacs 18
	 (save-excursion
	   (goto-char (window-point (get-buffer-window (current-buffer))))
	   (if (looking-at caml-error-chars-regexp)
	       (setq beg
		     (string-to-int
		      (buffer-substring (match-beginning 1) (match-end 1)))
		     end
		     (string-to-int
		      (buffer-substring (match-beginning 2) (match-end 2)))))))
       (cond (beg
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
		       (delete-overlay caml-error-overlay)))))))))

;;; Phrases

;itz the heuristics used to see if we're `between two phrases'
;didn't seem right to me.

(defun caml-mark-phrase ()
  "Put mark at end of this Caml phrase, point at beginning.

The Caml phrase is the phrase just before the point.
Modified by itz 03-26-96 to make it more robust - comments and strings
are now handled almost correctly - taking advantage of the parsing
stuff written for indentation."

  (interactive)
  (if (not (caml-find-kwop "[^ \t\n\(]+"))
      (error "No phrase preceding point"))
  (if ( not (caml-find-kwop ";;"))
      (goto-char 1)
    (forward-char 2))
  ;skip blanks and comments
  (skip-chars-forward " \t\n")
  (while (looking-at comment-start-skip)
    (forward-sexp 1)
    (skip-chars-forward " \t\n"))
  (push-mark)
  (let ((done nil))
    (while (not done)
      (if (not (re-search-forward ";;" nil t))
	  (error "Unterminated phrase"))
      (if (caml-in-comment-p)
	  (up-list 1)
	(if (caml-in-literal-p)
	    (re-search-forward "[\"\`]")
	  (setq done t)))))
  (exchange-point-and-mark))

(defvar caml-last-noncomment-pos nil
  "Caches last buffer position determined not inside a caml comment.")
(make-variable-buffer-local 'caml-last-noncomment-pos)

;;last-noncomment-pos can be a simple position, because we nil it
;;anyway whenever buffer changes upstream. last-comment-start and -end
;;have to be markers, because we preserve them when the changes' end
;;doesn't overlap with the comment's start.

(defvar caml-last-comment-start nil
  "A marker caching last determined caml comment start.")
(make-variable-buffer-local 'caml-last-comment-start)

(defvar caml-last-comment-end nil
  "A marker caching last determined caml comment end.")
(make-variable-buffer-local 'caml-last-comment-end)

(make-variable-buffer-local 'before-change-function)

(defun caml-overlap (b1 e1 b2 e2)
  (<= (max b1 b2) (min e1 e2)))

;this clears the last comment cache if necessary
(defun caml-before-change-function (begin end)
  (if (and caml-last-noncomment-pos
	   (> caml-last-noncomment-pos begin))
      (setq caml-last-noncomment-pos nil))
  (if (and (marker-position caml-last-comment-start)
	   (marker-position caml-last-comment-end)
	   (caml-overlap begin end
			 caml-last-comment-start
			 caml-last-comment-end))
      (prog2
	  (set-marker caml-last-comment-start nil)
	  (set-marker caml-last-comment-end nil)))
  (let ((orig-function (default-value 'before-change-function)))
    (if orig-function (funcall orig-function begin end))))

(defun caml-in-literal-p ()
  "Returns non-nil if point is inside a caml literal."
  (let* ((bol (save-excursion (beginning-of-line 1) (point)))
	 (state (parse-partial-sexp bol (point))))
    (nth 3 state)))

(defun caml-forward-list-safe ()
  (condition-case nil
      (forward-list 1)
    (error (goto-char (point-max)))))



(defun caml-in-comment-p ()
  "Returns non-nil if point is inside a caml comment."
  ;;we look for comments differently than literals. there are two
  ;;reasons for this. first, caml has nested comments and it is not so
  ;;clear that parse-partial-sexp supports them; second, if proper
  ;;style is used, literals are never split across lines, so we don't
  ;;have to worry about bogus phrase breaks inside literals, while we
  ;;have to account for that possibility in comments.
  (save-excursion
    (let* ((cached-pos caml-last-noncomment-pos)
	   (cached-begin (marker-position caml-last-comment-start))
	   (cached-end (marker-position caml-last-comment-end)))
      (cond
       ((and cached-begin cached-end
	     (< cached-begin (point)) (< (point) cached-end)) t)
       ((and cached-pos (= cached-pos (point))) nil)
       ((and cached-pos (< cached-pos (point)))
	(let ((inside) (done nil) (here (point)))
	  (goto-char cached-pos)
	  (while (not done)
	    (setq inside nil)
	    (if (not   (re-search-forward comment-start-skip (1+ here) 'move))
		(setq here 0)
	      (goto-char (match-beginning 0))
	      (setq inside (point))
	      (caml-forward-list-safe))
	    (setq done (< here (point))))
	  (if (not inside)
	      (setq caml-last-noncomment-pos here)
	    (set-marker caml-last-comment-start inside)
	    (set-marker caml-last-comment-end (point)))
	  inside))
       (cached-pos
	(let ((inside) (done nil) (here (point)))
	  (goto-char cached-pos)
	  (while (not done)
	    (setq inside nil)
	    (if (not (re-search-backward "\\*)" (1- here) 'move))
		(setq here (point-max))
	      (goto-char (match-end 0))
	      (setq inside (point))
	      (backward-list 1))
	    (setq done (> here (point))))
	  (if (not inside)
	      (setq caml-last-noncomment-pos here)
	    (set-marker caml-last-comment-start (point))
	    (set-marker caml-last-comment-end inside))
	  inside))	    
       (t   
	(let* ((here (point)) (done nil)
	       (limit (- (point) caml-lookback-limit))
	       (begin (condition-case nil
			  (prog2
			      (while (and (not done) (< limit (point)))
				(up-list -1)
				(setq done (looking-at comment-start-skip)))
			      (if done (point)))
			(error nil))))
	  (if (not begin) (setq caml-last-noncomment-pos here) 
	    (goto-char begin)
	    (caml-forward-list-safe)
	    (set-marker caml-last-comment-start begin)
	    (set-marker caml-last-comment-end (point)))
	  begin))))))


;;this could obviously be simplified with looking-at, but it's needed
;;at each iteration of caml-find-kwop, which is already slow enough
;;:-(
(defun caml-at-phrase-break-p ()
  (and (char-equal ?\; (following-char))
       (or (and (not (eobp))
		(char-equal ?\; (char-after (1+ (point)))))
	   (char-equal ?\; (preceding-char)))))

(defun caml-at-sexp-close-p ()
  (or (char-equal ?\) (following-char))
      (char-equal ?\] (following-char))
      (char-equal ?} (following-char))))

(defun caml-find-kwop (kwop-regexp)
  "Look back for a caml keyword or operator matching KWOP-REGEXP.

Ignore occurences inside literals. If found, return a list of two
values: the actual text of the keyword or operator, and a boolean
indicating whether the keyword was one we looked for explicitly
{non-nil}, or on the other hand one of the block-terminating
keywords."
  
  (let ((found nil) (old-syntax (char-syntax ?_)) (kwop))
    (unwind-protect
	(progn
	  (modify-syntax-entry ?_ "w" caml-mode-syntax-table) ;awful hack
	  (while (and (not found)
		      (re-search-backward kwop-regexp
					  (- (point) caml-lookback-limit) t))
	    (setq kwop (match-string 0))
	    (cond
	     ((caml-in-comment-p)
	      (up-list -1))
	     ((caml-in-literal-p)
	      (re-search-backward "[\"\`]"))	;ugly hack, but there is no clean way
	     ((setq found t)))))
      (modify-syntax-entry ?_ (char-to-string old-syntax)
			   caml-mode-syntax-table))
    (if found
	(if (not (string-match "\\`[^|[]|[^]|]?\\'" kwop)) ;arrrrgh!!
	    kwop
	  (forward-char 1) "|") nil)))

;  Association list of indentation values based on governing keywords.
;
;Each element is of the form (KEYWORD OP-TYPE PRIO INDENT). OP-TYPE is
;non-nil for operator-type nodes, which affect indentation in a
;different way from keywords: subsequent lines are indented to the
;actual occurrence of an operator, but relative to the indentation of
;the line where the governing keyword occurs.

(defconst caml-kwop-alist
  '(("begin" 		nil	-1	caml-begin-indent)
    ("exception" 	nil	-1	caml-exception-indent)
    ("for"		nil	-1	caml-for-indent)
    ("fun"		nil	0	caml-fun-indent)
    ("function"		nil	0	caml-function-indent)
    ("if"		nil	2	caml-if-indent)
    ("let"		nil	0	caml-let-indent)
    ("match"		nil	0	caml-match-indent)
    ("try"		nil	0	caml-try-indent)
    ("type"		nil	-1	caml-type-indent)
    ("value"		nil	-1	caml-value-indent)
    ("while"		nil	-1	caml-while-indent)
    ("::"		t	6	caml-::-indent)
    ("@"		t	5	caml-@-indent)
    (","		t	4	caml-,-indent)
    (":="		t	3	caml-:=-indent)
    ("<-"		t	3	caml-<--indent)
    (";"		t	1	caml-semi-indent)
    ("\["		t	6	caml-lb-indent)
    ("{"		t	6	caml-lc-indent)
    ("\("		t	6	caml-lp-indent))

"Association list of indentation values based on governing keywords.

Each element is of the form (KEYWORD OP-TYPE PRIO INDENT). OP-TYPE is
non-nil for operator-type nodes, which affect indentation in a
different way from keywords: subsequent lines are indented to the
actual occurrence of an operator, but relative to the indentation of
the line where the governing keyword occurs.")

;;Originally, we had caml-kwop-regexp create these at runtime, from an
;;additional field in caml-kwop-alist. That proved way too slow,
;;although I still can't understand why. itz

(defconst caml-kwop-regexps (make-vector 8 nil)
  "Array of regexps representing caml keywords of different priorities.")

(aset caml-kwop-regexps 0
      "\\<\\(and\\|begin\\|e\\(xception\\|nd\\)\\|for\\|type\\|value\\|while\\|done\\)\\>\\|;;\\|[][(){}]\\|[^|[]|\\([^]|]\\|\\=\\)")
(aset caml-kwop-regexps 1
      (concat (aref caml-kwop-regexps 0)
	      "\\|\\<\\(with\\|in\\|fun\\(ction\\)?\\|let\\|match\\|try\\)\\>"))
(aset caml-kwop-regexps 2
      (concat (aref caml-kwop-regexps 1) "\\|;"))
(aset caml-kwop-regexps 3
      (concat (aref caml-kwop-regexps 2) "\\|\\<if\\|then\\|else\\>"))
(aset caml-kwop-regexps 4
      (concat (aref caml-kwop-regexps 3) "\\|:=\\|<-"))
(aset caml-kwop-regexps 5
      (concat (aref caml-kwop-regexps 4) "\\|,"))
(aset caml-kwop-regexps 6
      (concat (aref caml-kwop-regexps 5) "\\|@"))
(aset caml-kwop-regexps 7
      (concat (aref caml-kwop-regexps 6) "\\|::"))


(defun caml-find-done-match ()
  (let ((unbalanced 1) (kwop t))
    (while (and (not (= 0 unbalanced)) kwop)
      (setq kwop (caml-find-kwop "\\<\\(done\\|for\\|while\\)\\>"))
      (cond
       ((not kwop))
       ((string= kwop "done") (setq unbalanced (1+ unbalanced)))
       (t (setq unbalanced (1- unbalanced)))))
    kwop))
      
(defun caml-find-end-match ()
  (let ((unbalanced 1) (kwop t))
    (while (and (not (= 0 unbalanced)) kwop)
      (setq kwop (caml-find-kwop "\\<\\(end\\|begin\\)\\>"))
      (cond
       ((not kwop))
       ((string= kwop "end") (setq unbalanced (1+ unbalanced)))
       ( t (setq unbalanced (1- unbalanced)))))
    kwop))

(defun caml-find-in-match ()
  (let ((unbalanced 1) (kwop t))
    (while (and (not (= 0 unbalanced)) kwop)
      (setq kwop (caml-find-kwop "\\<\\(in\\|let\\)\\>"))
      (cond
       ((not kwop))
       ((string= kwop "in") (setq unbalanced (1+ unbalanced)))
       ( t (setq unbalanced (1- unbalanced)))))
    kwop))
  
(defun caml-find-with-match ()
  (let ((unbalanced 1) (kwop t))
    (while (and (not (= 0 unbalanced)) kwop)
      (setq kwop (caml-find-kwop "\\<\\(with\\|try\\|match\\)\\>"))
      (cond
       ((not kwop))
       ((string= kwop "with") (setq unbalanced (1+ unbalanced)))
       ( t (setq unbalanced (1- unbalanced)))))
    kwop))

(defun caml-find-then-match (&optional from-else)
  (let ((unbalanced 1) (kwop t) (last-else from-else))
    (while (and (not (= 0 unbalanced)) kwop)
      (setq kwop (caml-find-kwop "\\<\\(e\\(nd\\|lse\\)\\|done\\|then\\|if\\)\\>\\|[])}]"))
      (cond
       ((not kwop))
       ((caml-at-sexp-close-p)
	(forward-char 1)
	(backward-list 1))
       ((string= kwop "done")
	(setq kwop (caml-find-done-match)))
       ((string= kwop "end")
	(setq kwop (caml-find-end-match)))
       ((string= kwop "else")
	(setq last-else t)
	(setq unbalanced (1+ unbalanced)))
       ((string= kwop "then")
	(if (not last-else) (setq unbalanced (1+ unbalanced)))
	(setq last-else nil))
       (t (setq last-else nil) (setq unbalanced (1- unbalanced)))))
    kwop))


(defun caml-find-and-or-pipe-match (re)
  (let ((done nil) (kwop))
    (while (not done)
      (setq kwop (caml-find-kwop re))
      (cond
       ((not kwop) (setq done t))
       ((caml-at-sexp-close-p)
	(forward-char 1)
	(backward-list 1))
       ((string= kwop "done")
	(caml-find-done-match))
       ((string= kwop "end")
	(caml-find-end-match))
       (t (setq done t))))
    kwop))

(defun caml-find-and-match ()
  (caml-find-and-or-pipe-match
   "\\<\\(e\\(nd\\|xception\\)\\|done\\|let\\|value\\|type\\)\\>\\|[])}]"))

(defun caml-find-pipe-match ()
  (caml-find-and-or-pipe-match
   "\\<\\(end\\|done\\|try\\|match\\|fun\\(ction\\)?\\|type\\)\\>\\|[])}]"))

(defun caml-find-else-match ()
  (caml-find-then-match t))

(defconst caml-matching-kw-alist
  '(("|" . caml-find-pipe-match)
    ("end" . caml-find-end-match)
    ("done" . caml-find-done-match)
    ("in"  . caml-find-in-match)
    ("with" . caml-find-with-match)
    ("else" . caml-find-else-match)
    ("then" . caml-find-then-match)
    ("do" . caml-find-done-match)
    ("and" . caml-find-and-match))

  "Association list used in caml mode for skipping back over nested blocks.")

(defun caml-looking-at-word (w)
  (let ((old-syntax (char-syntax ?_)) (p))
    (unwind-protect
	(progn
	  (modify-syntax-entry ?_ "w" caml-mode-syntax-table)
	  (setq p (looking-at w)))
      (modify-syntax-entry ?_ (char-to-string old-syntax)
			   caml-mode-syntax-table)) p))

(defun caml-find-kwop-skipping-blocks (prio)
  "Look back for a caml keyword matching caml-kwop-regexps [PRIO].

 Skip nested blocks."

  (let ((done nil) (kwop nil) (matching-fun)
	(kwop-list (aref caml-kwop-regexps prio)))
    (while (not done)
      (setq kwop (caml-find-kwop kwop-list))
      (cond
       ((not kwop) (setq done t))
       ((caml-at-phrase-break-p)
	(skip-chars-backward ";")
	(setq kwop ";;")
	(setq done t))
       ((caml-at-sexp-close-p)
	(forward-char 1)
	(backward-list 1))
       ((string= kwop "end") (caml-find-end-match))
       ((string= kwop "done") (caml-find-done-match))
       ((setq matching-fun (cdr-safe (assoc kwop caml-matching-kw-alist)))
	(setq kwop (funcall matching-fun)) (setq done t))
       (t (let* ((kwop-info (assoc kwop caml-kwop-alist))
		 (is-op (nth 1 kwop-info)))
	    (if (and is-op (looking-at 
			    (concat (regexp-quote kwop)
				    "|?[ \t]*\\(\n\\|(\\*\\)")))
		(setq kwop-list (aref caml-kwop-regexps (nth 2 kwop-info)))
	      (setq done t))))))
    kwop))

(defun caml-compute-basic-indent (prio)
  "Compute indent of current caml line, ignoring leading keywords.

Find the `governing node' for current line. Compute desired
indentation based on the node and the indentation alists.
Assumes point is exactly at line indentation.
Does not preserve point."
  
  (let* ((kwop (cond
		((looking-at ";;") ";;")
		((looking-at "|\\([^]|]\\|\\'\\)")
		 (caml-find-pipe-match))
		((caml-looking-at-word
		"\\(and\\|do\\(ne\\)?\\|in\\|with\\|then\\|e\\(lse\\|nd\\)\\)\\>")
		 (funcall (cdr (assoc (match-string 1)
				      caml-matching-kw-alist))))
		((caml-looking-at-word (aref caml-kwop-regexps
					     caml-max-indent-priority))
		 (let* ((kwop (match-string 0))
			(kwop-info (assoc kwop caml-kwop-alist))
			(is-op (nth 1 kwop-info))
			(prio (if is-op (nth 2 kwop-info)
				caml-max-indent-priority)))
		   (caml-find-kwop-skipping-blocks prio)))
		(t (caml-find-kwop-skipping-blocks prio))))
	 (kwop-info (assoc kwop caml-kwop-alist))
	 (indent-diff
	  (cond
	   ((not kwop-info) (beginning-of-line 1) 0)
	   ((nth 1 kwop-info) (symbol-value (nth 3 kwop-info)))
	   (t (back-to-indentation) (symbol-value (nth 3 kwop-info))))))
  (+ indent-diff (current-column))))

(defconst caml-leading-kwops-regexp
  "\\(and\\|do\\(ne\\)?\\|e\\(lse\\|nd\\)\\|in\\|then\\|with\\)\\>\\||\\|]\\|}"

  "Regexp matching caml keywords which need special indentation.")

(defconst caml-leading-kwops-alist
  '(("and" caml-and-extra-indent 1)
    ("do" caml-do-extra-indent 0)
    ("done" caml-done-extra-indent 0)
    ("else" caml-else-extra-indent 3)
    ("end" caml-end-extra-indent 0)
    ("in" caml-in-extra-indent 1)
    ("then" caml-then-extra-indent 3)
    ("with" caml-with-extra-indent 1)
    ("|" caml-|-extra-indent 1)
    ("]" caml-rb-extra-indent 7)
    ("}" caml-rc-extra-indent 7))

  "Association list of special caml keyword indent values.

Each member is of the form (KEYWORD EXTRA-INDENT PRIO) where
EXTRA-INDENT is the variable holding extra indentation amount for
KEYWORD (usually negative) and PRIO is upper bound on priority of
matching nodes to determine KEYWORD's final indentation.")

(defun caml-compute-final-indent ()
  (save-excursion
    (back-to-indentation)
    (cond
     ((caml-in-comment-p)
      (let ((done nil))
	(while (not done)
	  (up-list -1)
	  (setq done (not (caml-in-comment-p))))
	(looking-at comment-start-skip)
	(goto-char (match-end 0))
	(current-column)))
     (t (let* ((leading (caml-looking-at-word caml-leading-kwops-regexp))
	       (assoc-val (if leading (assoc (match-string 0)
					     caml-leading-kwops-alist)))
	       (extra (if leading (symbol-value (nth 1 assoc-val)) 0))
	       (prio (if leading (nth 2 assoc-val)
		       caml-max-indent-priority)))
	  (max 0 (+ extra
		    (caml-compute-basic-indent prio))))))))


(defun caml-split-string ()
  "Called whenever a line is broken inside a caml string literal."
  (insert-before-markers "\"^\"")
  (backward-char 1))

(defadvice indent-new-comment-line (around
				    caml-indent-new-comment-line
				    activate)
  
  "Handle multi-line strings in caml mode."

;this advice doesn't make sense in other modes. I wish there were a
;cleaner way to do this: I haven't found one.

  (let ((hooked (and (eq major-mode 'caml-mode) (caml-in-literal-p)))
	(split-mark))
    (if (not hooked) nil
      (setq split-mark (set-marker (make-marker) (point)))
      (caml-split-string))
    ad-do-it
    (if (not hooked) nil
      (goto-char split-mark)
      (set-marker split-mark nil))))  
  
(defadvice newline-and-indent (around
			       caml-newline-and-indent
			       activate)

  "Handle multi-line strings in caml mode."

    (let ((hooked (and (eq major-mode 'caml-mode) (caml-in-literal-p)))
	(split-mark))
    (if (not hooked) nil
      (setq split-mark (set-marker (make-marker) (point)))
      (caml-split-string))
    ad-do-it
    (if (not hooked) nil
      (goto-char split-mark)
      (set-marker split-mark nil))))

(defun caml-electric-pipe ()
  "If inserting a | or } operator at beginning of line, reindent the line.

Unfortunately there is a situation where this mechanism gets
confused. It's when | is the first character of a |] sequence. This is
a misfeature of caml syntax and cannot be fixed, however, as a
workaround, the electric ] inserts | itself if the matching [ is
followed by |."
  
  (interactive "*")
  (let ((electric (and caml-electric-indent
		       (caml-in-indentation)
		       (not (caml-in-comment-p)))))
    (self-insert-command 1)
    (if electric
	(let ((indent
	       (save-excursion
		 (backward-char 1)
		 (caml-indent-command)
		 (current-column))))
	  (indent-to (- indent
			(symbol-value
			 (nth 1 (assoc
				 (char-to-string last-command-char)
				 caml-leading-kwops-alist)))))))))

(defun caml-electric-rb ()
  "If inserting a ] operator at beginning of line, reindent the line.

Also, if the matching [ is followed by a | and this ] is not preceded
by |, insert one."

  (interactive "*")
  (let* ((prec (preceding-char))
	 (look-pipe (and caml-electric-close-vector
			 (not (caml-in-comment-p))
			 (not (caml-in-literal-p))
			 (or (not (numberp prec))
			     (not (char-equal ?| prec)))
			 (set-marker (make-marker) (point))))
	 (electric (and caml-electric-indent
			(caml-in-indentation)
			(not (caml-in-comment-p)))))
    (self-insert-command 1)
    (if electric
	(let ((indent
	       (save-excursion
		 (backward-char 1)
		 (caml-indent-command)
		 (current-column))))
	  (indent-to (- indent
			(symbol-value
			 (nth 1 (assoc
				 (char-to-string last-command-char)
				 caml-leading-kwops-alist)))))))
    (if look-pipe
	(save-excursion
	  (let ((insert-pipe
		 (condition-case nil
		     (prog2
		       (backward-list 1)
		       (if (looking-at "\\[|") "|" ""))
		   (error ""))))
	    (goto-char look-pipe)
	    (insert insert-pipe))
	  (set-marker look-pipe nil)))))		 

(defun caml-abbrev-hook ()
  "If inserting a leading keyword at beginning of line, reindent the line."
  ;itz unfortunately we need a special case 
  (if (and (not (caml-in-comment-p)) (not (= last-command-char ?_)))
      (let* ((bol (save-excursion (beginning-of-line) (point)))
	     (kw (save-excursion
		   (and (re-search-backward "^[ \t]*\\(\\sw+\\)\\=" bol t)
			(match-string 1)))))
	(if kw
	    (let ((indent (save-excursion
			    (goto-char (match-beginning 1))
			    (caml-indent-command)
			    (current-column)))
		  (abbrev-correct (if (= last-command-char ?\ ) 1 0)))
	      (indent-to (- indent
			    (symbol-value
			     (nth 1 (assoc kw caml-leading-kwops-alist)))
			    abbrev-correct)))))))

(fset 'caml-indent-phrase
   "\C-[\C-h\C-[\C-\\")

(defun caml-backward-to-less-indent (&optional n)
  "Move cursor back  N lines with less or same indentation."
  (interactive "p")
  (beginning-of-line 1)
  (if (< n 0) (caml-forward-to-less-indent (- n))
    (while (> n 0)
      (let ((i (current-indentation)))
	(forward-line -1)
	(while (or (> (current-indentation) i)
		   (caml-in-comment-p)
		   (looking-at
		    (concat "[ \t]*\\(\n\\|" comment-start-skip "\\)")))
	  (forward-line -1)))
      (setq n (1- n))))
  (back-to-indentation))

(defun caml-forward-to-less-indent (&optional n)
  "Move cursor back N lines with less or same indentation."
  (interactive "p")
  (beginning-of-line 1)
  (if (< n 0) (caml-backward-to-less-indent (- n))
    (while (> n 0)
      (let ((i (current-indentation)))
	(forward-line 1)
	(while (or (> (current-indentation) i)
		   (caml-in-comment-p)
		   (looking-at
		    (concat "[ \t]*\\(\n\\|" comment-start-skip "\\)")))
	  (forward-line 1)))
      (setq n (1- n))))
  (back-to-indentation))  

(defun caml-insert-begin-form ()
  "Inserts a nicely formatted begin-end form, leaving a mark after end."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let* ((c (current-indentation)) (i (+ caml-begin-indent c)))
    (insert "begin\n\nend\n")
    (indent-line-to c)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)))

(defun caml-insert-for-form ()
  "Inserts a nicely formatted for-do-done form, leaving a mark after do(ne)."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let* ((c (current-indentation)) (i (+ caml-for-indent c)))
    (insert "for  do\n\ndone\n")
    (indent-line-to c)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)
    (push-mark)
    (beginning-of-line 1)
    (backward-char 4)))
  
(defun caml-insert-if-form ()
  "Insert nicely formatted if-then-else form leaving mark after then, else."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let* ((c (current-indentation)) (i (+ caml-if-indent c)))
    (insert "if\n\nthen\n\nelse\n")
    (indent-line-to i)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)))

(defun caml-insert-match-form ()
  "Insert nicely formatted match-with form leaving mark after with."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let* ((c (current-indentation)) (i (+ caml-match-indent c)))
    (insert "match\n\nwith\n")
    (indent-line-to i)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)))

(defun caml-insert-let-form ()
  "Insert nicely formatted let-in form leaving mark after in."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let* ((c (current-indentation)) (i (+ caml-let-indent c)))
    (insert "let\n\nin\n")
    (indent-line-to i)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)))

(defun caml-insert-try-form ()
  "Insert nicely formatted try-with form leaving mark after with."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let* ((c (current-indentation)) (i (+ caml-try-indent c)))
    (insert "try\n\nwith\n")
    (indent-line-to i)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)))

(defun caml-insert-while-form ()
  "Insert nicely formatted while-do-done form leaving mark after do, done."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let* ((c (current-indentation)) (i (+ caml-if-indent c)))
    (insert "while  do\n\ndone\n")
    (indent-line-to c)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)
    (push-mark)
    (beginning-of-line 1)
    (backward-char 4)))

;;; caml.el ends here

(provide 'caml)
