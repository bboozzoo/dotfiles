;; From Emacs Starter Kit
;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq local-modules-dir "~/.emacs.d/modules")
(setq local-cache-dir "~/.emacs.d/cache/")
(setq local-plugins-dir "~/.emacs.d/plugins")

(add-to-list 'load-path local-modules-dir)
(add-to-list 'load-path "~/.emacs.d/plugins/")
(add-to-list 'load-path "~/.emacs.d/elpa/")

(require 'module-util)

;; local cache path (for recentf, IDO, etc.)
; Create cache dir if one does not exist
(bozo-make-dir local-cache-dir)

(setq bozo-color-theme-x 'color-theme-tty-dark)
(setq bozo-color-theme-terminal 'color-theme-tty-dark)

(require 'module-lang)
(require 'module-elpa)
(require 'module-ui)
(require 'module-usability)
(require 'module-keys)