;; need some utility functions
(require 'module-util)
(message "Module util loading")
;; disable startup screen
(setq inhibit-startup-screen t) 

;; set font
;;(set-default-font "ProggyCleanTT-12")
;;(setq default-frame-alist '((font . "Inconsolata-10")))
;;(setq default-frame-alist '((font . "ProggyCleanTT-12")))
;; (setq default-frame-alist '((font . "Ubuntu Mono-11")))
(setq default-frame-alist '((font . "Monaco-10")))

;; get rid of toolbar
(when (fboundp 'tool-bar-mode) 
  (tool-bar-mode -1))

;; get rid of menu bar
(menu-bar-mode -1)

;; syntax coloring
(require 'font-lock)
(global-font-lock-mode t)
(setq font-lock-verbose nil)

;; disable blinking cursor
(blink-cursor-mode -1)

;; highlight query
(setq query-replace-highlight t)

;; highling incremental search
(setq search-highlight t)

;; search for open-paren till point-min
(setq blink-matching-paren-distance nil)

;; show selected region
(setq transient-mark-mode t)

;; highlight current line
;; (global-hl-line-mode t)

 ;show line numbers
(global-linum-mode 1)

;; line highlight
(require 'hl-line+)

;; show line number in status bar
(line-number-mode t)

;; show column number in status bar
(column-number-mode t)

;; show file size
(size-indication-mode t)

;; delete region by typing
(delete-selection-mode t)

;; highlight matching parenthesis
(setq show-paren-delay 0)
(setq show-paren-syle 'mixed)
(show-paren-mode t)

;; setup scrolling
(setq
 scroll-margin 0                        ;; do smooth scrolling, ...
 scroll-conservatively 100000           ;; ... the defaults ...
 scroll-up-aggressively 0               ;; ... are very ...
 scroll-down-aggressively 0             ;; ... annoying
 scroll-preserve-screen-position t)     ;; preserve screen pos with C-v/M-v 

;; move mouse if cursor is too close
(mouse-avoidance-mode 'jump)

;; scroll bar on the right
(set-scroll-bar-mode nil)

;; color themes

;; (require 'color-theme-tangotango)
;; (color-theme-tangotango)
;; (load-theme 'adwaita)
;; set path to custom location of themes
(when (>= emacs-major-version 24)
  (add-to-list 'custom-theme-load-path local-themes-custom-dir)
  (if (and (boundp 'bozo-color-theme-x)
           (symbol-value 'bozo-color-theme-x))
      (bozo-set-color-theme bozo-color-theme-x)))

;; (when (= emacs-major-version 23)
;;   (add-to-list 'load-path local-themes-custom-dir)
;;   (require 'naquadah-theme))

;; mark at 80 columns
(require 'fill-column-indicator)

(run-hooks 'bozo-module-ui-hooks)
(provide 'module-ui)
;; module-ui.el ends here
