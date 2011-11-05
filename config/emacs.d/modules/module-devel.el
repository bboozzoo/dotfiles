;; development related settings

;; use cscope
(require 'xcscope)

;; style for C
(setq c-default-style "linux"
      c-basic-offset 4)

;; javascript mode
;; enable for all *.js files
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode)) 
; load javascript when javascipt-mode is needed
(autoload 'javascript-mode "javascript" nil t) 

; gnu global
;; (require 'gtags)
; prevent auto tags update in blacklisted directories
;(setq disallowed-gtags-dirs '("pBS"))

;; (defun gtags-create-or-update ()
;;   "create or update the gnu global tag file"
;;   (interactive)
;;   (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
;;     (let ((olddir default-directory)
;;           (topdir (read-directory-name  
;;                     "gtags: top of source tree:" default-directory)))
;;       (cd topdir)
;;       (shell-command "gtags && echo 'created tagfile'")
;;       (cd olddir)) ; restore   
;;     ;;  tagfile already exists; update it
;;     (shell-command "global -u && echo 'updated tagfile'")))

;; (defun allowed-gtags-dir (dir)
;;   "check if directory can be indexed"
;;   (interactive)
;;   (if (boundp 'disallowed-gtags-dirs)
;;       ;; check if directory name matches any of blacklisted dirs
;;       (let (retlist)
;;         (dolist (elt disallowed-gtags-dirs)
;;           (let ((case-fold-search nil))
;;             (if (string-match elt dir)
;;                 (progn ;; true
;;                   (setq retlist (cons elt retlist))
;;                   elt)
;;               )
;;             ))
;;         (not retlist))
;;     t))

;; (add-hook 'c-mode-common-hook
;;   (lambda ()
;;     (require 'gtags)
;;     (gtags-mode t)
;;     (let ((dir (expand-file-name default-directory)))
;;       (if (allowed-gtags-dir dir)
;;           (gtags-create-or-update)
;;         (message "directory %s is blacklisted for gtags" dir)))))

; additional gtags bindings
;; (add-hook 'gtags-mode-hook 
;;   (lambda()
;;     (local-set-key (kbd "M-.") 'gtags-find-tag)   ; find a tag, also M-.
;;     (local-set-key (kbd "M-,") 'gtags-find-rtag)
;;     (local-set-key (kbd "C-S-<f5>") 'gtags-create-or-update)
;;     (local-set-key (kbd "C-c s s") 'gtags-find-tag-from-here)
;;     (local-set-key (kbd "C-c s e") 'gtags-find-with-grep)
;;     (local-set-key (kbd "C-c s f") 'gtags-find-file)))  ; reverse tag

(require 'module-snippets)
(require 'module-net)

(provide 'module-devel)
;; module-devel.el ends here