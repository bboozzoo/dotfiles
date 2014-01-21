;; default dir where auto-complete is installed
(setq auto-complete-install-dir (expand-file-name 
                                 "auto-complete" local-plugins-dir))

(add-to-list 'load-path auto-complete-install-dir)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (expand-file-name
                                         "ac-dict"
                                         auto-complete-install-dir))
(ac-config-default)

(setq ac-delay 0)
(setq ac-auto-show-menu t)
(global-auto-complete-mode t)

(add-to-list 'ac-modes 'makefile-gmake-mode)
(add-to-list 'ac-modes 'nxml-mode)
(add-to-list 'ac-modes 'octave-mode)

(provide 'module-autocompletion)
;; module-autocompletion.el ends here
