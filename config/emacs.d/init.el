(setq config-file "~/.emacs.d/maciek.org")

(add-to-list 'load-path "~/.emacs.d/elpa/")

(require 'package)

;; enable marmalade and MELPA repositories
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(require 'org)
(require 'ob-tangle)
(when (file-exists-p config-file)
    (org-babel-load-file config-file))
