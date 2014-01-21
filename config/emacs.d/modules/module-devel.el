;; development related settings

;; use cscope
;;(require 'xcscope)
;;(require 'xgtags)
(require 'ggtags)

;; show function name in status bar
(which-function-mode 1)


;;;;;;;;;;;;;;;;;;;;
;; makefile
;;;;;;;;;;;;;;;;;;;;
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))

;;;;;;;;;;;;;;;;;;;;
;; cmake
;;;;;;;;;;;;;;;;;;;;
(require 'cmake-mode)

(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))

;;;;;;;;;;;;;;;;;;;
;; cc-mode common
;;;;;;;;;;;;;;;;;;
; common for all modes derived from cc-mode

(setq c-default-style '((other . "linux")))

(setq c-basic-offset 4)

(setq hide-ifdef-initially t
      hide-ifdef-shadow t)

(defun bozo-enable-electric ()
  (electric-indent-mode t))

(defun bozo-enable-subword ()
  (subword-mode 1))

(defun bozo-enable-hide-ifdef ()
  (hide-ifdef-mode 1))
;;;;;;;;;;;;;;;;;;;;
;; C
;;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-common-hook 'bozo-enable-electric)
(add-hook 'c-mode-common-hook 'bozo-add-whitespace-cleanup-on-save)
(add-hook 'c-mode-common-hook (lambda ()
                                (add-to-list 'ac-sources 'ac-source-gtags)))
(add-hook 'c-mode-common-hook 'bozo-enable-subword)
(add-hook 'c-mode-common-hook 'bozo-enable-hide-ifdef)
(add-hook 'c-mode-common-hook (lambda ()
                                (ggtags-mode 1)))

;;;;;;;;;;;;;;;;;;;;
;; C++
;;;;;;;;;;;;;;;;;;;;

;;(add-hook 'c++-mode-hook 'bozo-enable-subword)

;;;;;;;;;;;;;;;;;;;
;; Java
;;;;;;;;;;;;;;;;;;
; add default style
(push '(java-mode . "java") c-default-style)


(add-hook 'java-mode-hook 'bozo-enable-subword)

(add-hook 'java-mode-hook (function cscope:hook))

;;;;;;;;;;;;;;
;; GNU global
;;;;;;;;;;;;;;;;
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
;; Python
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
;;(require 'ac-python)
;; eldoc mode
(add-hook 'python-mode-hook 'turn-on-eldoc-mode)

;; pdb
(setq gud-pdb-command-name "python -m pdb")

(add-hook 'python-mode-hook 'bozo-add-whitespace-cleanup-on-save)

;;;;;;;;;;;;;;;;;;;
;; *LISP
;;;;;;;;;;;;;;;;;;;

(defun bozo-lisp-defaults ()
  (paredit-mode t)
  (rainbow-delimiters-mode t))

(defun bozo-lisp-repl-defaults ()
  (bozo-lisp-defaults))

;;;;;;;;;;;;;;;;;
;; emacs lisp
;;;;;;;;;;;;;;;;;
(defun bozo-elisp-defaults ()
  (bozo-lisp-defaults)
  (turn-on-eldoc-mode))

(defun bozo-ielm-defaults ()
  (bozo-lisp-repl-defaults)
  (turn-on-eldoc-mode))

(add-hook 'emacs-lisp-mode-hook 'bozo-elisp-defaults)
(add-hook 'ielm-mode-hook 'bozo-ielm-defaults)
(add-hook 'lisp-mode-hook 'bozo-add-whitespace-cleanup-on-save)

;;;;;;;;;;;;;;;;
;; slime
;;;;;;;;;;;;;;;;
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
;; sh
;;;;;;;;;;;;;;;;;;;
(add-hook 'sh-mode-hook 'bozo-add-whitespace-cleanup-on-save)

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
(require 'auto-complete-nxml)

;;;;;;;;;;;;;;;;;;;;
;; octave
;;;;;;;;;;;;;;;;;;;;
(setq auto-mode-alist (cons '("\\.m$" . octave-mode)
                            auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;
;; windows batch files
;;;;;;;;;;;;;;;;;;;;;;;
(require 'batch-mode)

(setq auto-mode-alist
	  (append
	   '(("\\.bat\\'" . batch-mode))
	   auto-mode-alist))

;;;;;;;;;;;;;;;;;;;
;; erlang
;;;;;;;;;;;;;;;;;;;
(speedbar-add-supported-extension '(".erl" ".hrl"))
(add-to-list 'speedbar-fetch-etags-parse-list
             '("\\.[eh]rl" . speedbar-parse-c-or-c++tag))

(setq bozo:erlang-locations '("/usr/lib64/erlang"
                              "/usr/lib/erlang"))

(defun bozo-find-erlang-root (roots)
  "Return erlang root dir by looking through list of possible locations"
  (find-if (lambda (dir)
             (file-exists-p dir))
           roots))

(setq erlang-root-dir (bozo-find-erlang-root bozo:erlang-locations))

;; (setq-default bozo-distel-dir "~/code/distel")
(setq-default bozo-edts-dir nil)
(setq bozo-edts-dir "~/code/edts")

(setq inferior-erlang-machine-options '("-sname" "emacs"))
;; distel
;; (defun ac-distel-setup ()
;;   (setq ac-sources '(ac-source-distel)))

;; (if (file-directory-p bozo-distel-dir)
;;     (progn
;;       (add-to-list 'load-path (concat (file-name-as-directory bozo-distel-dir)
;;                                       "elisp"))
;;       (require 'distel)
;;       (distel-setup)
;;       (add-hook 'erlang-mode-hook 'ac-distel-setup)
;;       (add-hook 'erlang-shell-mode-hook 'ac-distel-setup)
;;      ))
(if (and bozo-edts-dir (file-directory-p bozo-edts-dir))
    (progn
      (add-to-list 'load-path bozo-edts-dir)
      (setq edts-man-root erlang-root-dir)
      ;(setq edts-log-level 'debug)
      (require 'edts-start)))

;; (add-hook 'erlang-mode-hook
;;           (lambda ()
;;             (define-key erlang-mode-map (kbd "C-c C-f") 'erlang-man-function)))

;; (add-hook 'edts-mode-hook
;;           (lambda ()
;;             (auto-higlight-symbol-mode -1)))
;; use ac for erlang anyway
;;(add-to-list 'ac-modes 'erlang-mode)

;;;;;;;;;;;;;;;;;;;
;; protobuf
;;;;;;;;;;;;;;;;;;;
(require 'protobuf-mode)
(setq auto-mode-alist (cons '("\\.proto$" . protobuf-mode)
                            auto-mode-alist))

;;;;;;;;;;;;;;;;;;;
;; (ma)git
;;;;;;;;;;;;;;;;;;;
(require 'magit)
(add-hook 'magit-status-mode-hook
          (lambda ()
            (linum-mode -1)))

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;;;;;;;;;;;;;;;;;;;;
;; text
;;;;;;;;;;;;;;;;;;;
(add-hook 'text-mode-hook 'bozo-add-whitespace-cleanup-on-save)


;; load other modules
(require 'module-snippets)
(require 'module-net)

;; finally run user's hooks
(run-hooks 'bozo-module-devel-hooks)

(provide 'module-devel)
;; module-devel.el ends here
