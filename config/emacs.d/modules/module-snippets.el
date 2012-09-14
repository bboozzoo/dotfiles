;yasnippet
(require 'yasnippet)

(setq snippet-dirs nil)
;; load snippets that came with package
;;(let ((default-snippets-dir (bozo-find-package-dir 'yasnippet)))
;;  (add-to-list 'snippet-dirs (concat default-snippets-dir "/snippets")))

;; create user snippets directory if not present
(bozo-make-dir "~/.emacs.d/snippets")

;;(add-to-list 'snippet-dirs "~/.emacs.d/snippets")

(yas-global-mode 1)

(provide 'module-snippets)
;; module-snippets.el ends here
