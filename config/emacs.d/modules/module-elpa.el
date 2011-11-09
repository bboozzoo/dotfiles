(require 'package)

;; enable marmalade repository
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(provide 'module-elpa)