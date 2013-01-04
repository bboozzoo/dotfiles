
(bozo-make-dir local-site-custom-dir)

(message "Load custom files from %s" local-site-custom-dir)

(dolist (local-file (directory-files local-site-custom-dir t "^[^#]*.el$"))
  (progn (message "Loading custom file %s" local-file)
         (load-file local-file)))

(provide 'module-local)
;; module-local.el ends here
