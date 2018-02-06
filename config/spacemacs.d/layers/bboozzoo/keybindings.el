;;; keybindings.el --- bboozzoo customization layer
;;
;;; License: MIT

;; disable background mode
(global-set-key (kbd "C-Z") nil)

;; bind file refresh to F5
(global-set-key (kbd "<f5>") 'bboozzoo/refresh-file)

;; bring up recent files with F12
(global-set-key (kbd "<f12>") 'helm-recentf)

(spacemacs/set-leader-keys-for-major-mode 'go-mode
  "gb" 'pop-tag-mark)
