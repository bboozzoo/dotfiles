;yasnippet
(require 'yasnippet)

(defcustom bozo-snippet-dirs '()
  "A list of directories containing snippets")

;; create user snippets directory if not present
(bozo-make-dir "~/.emacs.d/snippets")

; add custom snippet dirs to the list
(dolist (sd bozo-snippet-dirs)
  (add-to-list 'yas-snippet-dirs sd))

(yas-global-mode 1)

(provide 'module-snippets)
;; module-snippets.el ends here
