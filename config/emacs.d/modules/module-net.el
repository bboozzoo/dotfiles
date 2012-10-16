;; network related stuff

;; use TRAMP mode
(require 'tramp)                    ;enable TRAMP (edit through network)
(setq tramp-default-method "ssh")   ;TRAMP use ssh

(run-hooks 'bozo-module-net-hooks)
(provide 'module-net)
;; module-net.el ends here
