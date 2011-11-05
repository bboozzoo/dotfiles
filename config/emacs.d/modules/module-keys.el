
; key bindings
(global-set-key (kbd "C-<tab>") 'other-frame)
(global-set-key (kbd "C-<prior>") 'bozo-select-previous-window) ; ctr+pg_up
(global-set-key (kbd "C-<next>") 'bozo-select-next-window) ; ctr+pg_down

(global-set-key (kbd "s-o") 'jl-jump-backward)
(global-set-key (kbd "s-i") 'jl-jump-forward)
(global-set-key (kbd "C-5") 'bozo-goto-match-paren)
(global-set-key (kbd "C-Z") nil) ;disable ctrl + z background mode
(global-set-key (kbd "C-<f5>") 'bozo-refresh-file)
(global-set-key (kbd "C-<f8>") 'comment-or-uncomment-region) ; (un)comment
(when (fboundp 'sr-speedbar-toggle)
  (global-set-key (kbd "C-<f9>") 'sr-speedbar-toggle)
  (global-set-key (kbd "C-<f10>") 'sr-speedbar-select-window)) ; speedbar
(global-set-key (kbd "C-<f11>") 'bozo-load-module)
(global-set-key (kbd "C-<f12>") 'recentf-open-files)

(provide 'module-keys)
;; module-keys.el ends here