(require 'auto-complete-config)

(let ((default-dict-dir (package--dir "auto-complete" "1.4.20110207")))
  (add-to-list 'ac-dictionary-directories (concat default-dict-dir "/dict")))

(ac-config-default)
(setq ac-delay 0)
(setq ac-auto-show-menu t)
(global-auto-complete-mode t)

(add-to-list 'ac-modes 'makefile-gmake-mode)


(provide 'module-autocompletion)