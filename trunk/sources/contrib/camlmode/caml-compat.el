;;Functions missing from older 19.xx Emacsen.

(if (not (fboundp 'indent-line-to))
    (defun indent-line-to (column)
      "Indent current line to COLUMN.

This function removes or adds spaces and tabs at beginning of line
only if necessary.  It leaves point at end of indentation."

      (beginning-of-line 1)
      (delete-horizontal-space)
      (indent-to column)))

(if (not (fboundp 'match-string))
    (defun match-string (num &optional string)

      "Return string of text matched by last search.

NUM specifies which parenthesized expression in the last regexp.
Value is nil if NUMth pair didn't match, or there were less than NUM
pairs.  Zero means the entire text matched by the whole regexp or
whole string.  STRING should be given if the last search was by
`string-match' on STRING."

      (let* ((data (match-data))
	     (begin (nth (* 2 num) data))
	     (end (nth (1+ (* 2 num)) data)))
	(if string (substring string begin end)
	  (buffer-substring begin end)))))

(provide 'caml-compat)
