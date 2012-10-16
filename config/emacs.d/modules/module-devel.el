;; development related settings

;; use cscope
(require 'xcscope)

;;;;;;;;;;;;;;;;;;;;
;; makefile
;;;;;;;;;;;;;;;;;;;;
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))

;;;;;;;;;;;;;;;;;;;;
;; C
;;;;;;;;;;;;;;;;;;;;

;; common for all modes derived from cc-mode
(setq c-default-style "linux"
      c-basic-offset 4)

(defun bozo-c-mode-defaults ()
  (electric-indent-mode t))

;(add-hook 'c-mode-common-hook 'bozo-c-mode-common-defaults)
(add-hook 'c-mode-hook 'bozo-c-mode-defaults)


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

;;;;;;;;;;;;;;;;;
;; python
;;;;;;;;;;;;;;;;;

;; load ropemacs on demand
;; (defun bozo-devel-python-load-ropemacs ()
;;   (if (not (fboundp 'ropemacs-mode))
;;       (pymacs-load "ropemacs" "rope-"))
;;   )
;; ;; to be called from python-mode-hook
;; (defun bozo-devel-python-ropemacs-ac ()
;;   (bozo-devel-python-load-ropemacs)
;;   (ac-ropemacs-initialize)
;;   (add-to-list 'ac-sources 'ac-source-ropemacs)
;;   )
;; load pymacs and setup hooks
;;(require 'pymacs)
;;(bozo-devel-python-ropemacs-ac)

;; ipython as python shell
;; (require 'ipython)

;; simple completion for python
;; (defvar ac-source-python '((candidates .
;;                                        (lambda ()
;;                                          (mapcar '(lambda (completion)
;;                                                     (first (last (split-string completion "\\." t))))
;;                                                  (python-symbol-completions (python-completion-at-point)))))))
;; (add-hook 'python-mode-hook
;;           (lambda() (setq ac-sources '(ac-source-python))))
(require 'ac-python)
;; eldoc mode
(add-hook 'python-mode-hook 'turn-on-eldoc-mode)

;; pdb
(setq gud-pdb-command-name "python -m pdb")

;;;;;;;;;;;;;;;;;;;
;; *LISP
;;;;;;;;;;;;;;;;;;;

(defun bozo-lisp-defaults ()
  (paredit-mode t)
  (rainbow-delimiters-mode t))

(defun bozo-lisp-repl-defaults ()
  (bozo-lisp-defaults))

;;
;; emacs lisp
;;
(defun bozo-elisp-defaults ()
  (bozo-lisp-defaults)
  (turn-on-eldoc-mode))

(defun bozo-ielm-defaults ()
  (bozo-lisp-repl-defaults)
  (turn-on-eldoc-mode))

(add-hook 'emacs-lisp-mode-hook 'bozo-elisp-defaults)
(add-hook 'ielm-mode-hook 'bozo-ielm-defaults)

;;
;; slime
;;
(setq slime-lisp-implementations
      '((ecl ("ecl"))
        (clisp ("clisp" "-q"))
        (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))
;; use clisp by default
(setq slime-default-lisp 'clisp)

(eval-after-load "slime"
  '(progn
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))

(defun bozo-common-lisp-defaults ()
  (bozo-lisp-defaults)
  (slime-mode))

(add-hook 'lisp-mode-hook 'bozo-common-lisp-defaults)
(add-hook 'slime-repl-mode-map 'bozo-lisp-repl-defaults)


;;;;;;;;;;;;;;;;;;;
;; *XML
;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist 
             (cons (concat "\\." 
                           (regexp-opt '("xml" "xsd" "sch" 
                                         "rng" "xslt" 
                                         "svg" "rss") 
                                       t) 
                           "\\'")
                   'nxml-mode))
(push '("<\\?xml" . nxml-mode) magic-mode-alist)



;;;;;;;;;;;;;;;;;;;

(require 'module-snippets)
(require 'module-net)

(run-hooks 'bozo-module-devel-hooks)
(provide 'module-devel)
;; module-devel.el ends here
