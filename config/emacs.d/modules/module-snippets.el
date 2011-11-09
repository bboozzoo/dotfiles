;yasnippet
(require 'yasnippet)

(setq snippet-dirs nil)
;; load snippets that came with package
(let ((default-snippets-dir (package--dir "yasnippet" "0.6.1")))
  (add-to-list 'snippet-dirs (concat default-snippets-dir "/snippets")))

;; create user snippets directory if not present
(bozo-make-dir "~/.emacs.d/snippets")

(add-to-list 'snippet-dirs "~/.emacs.d/snippets")

(yas/initialize)
;; load all snippets
(dolist (sd snippet-dirs)
  (yas/load-directory sd))

(provide 'module-snippets)
;; module-snippets.el ends here
