;; set frame title manually
(defun set-frame-title (title)
  "Set current frame title"
  (interactive "sNew frame title: ")
  (setq frame-title-format title)
  )

;; refresh file 
(defun refresh-file ()
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t)
  )

;; jump to matching parenthesis
(defun goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))

(defun recompile-modules ()
  "Byte-compile all modules"
  (interactive)
  (byte-recompile-directory local-modules-dir 0)
  )

(provide 'module-util)
;; module-util.el ends here