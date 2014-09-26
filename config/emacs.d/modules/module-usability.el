;; usability enhancements, tabs, ido, uniquify etc..
;; winner mode
(winner-mode 1)

;; dired
(put 'dired-find-alternate-file 'disabled nil)
; taken from Xah Lee's Ergo emacs site
(setq dired-dwim-target t)
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))
(defun bozo-remap-dired-keys ()
  (define-key dired-mode-map (kbd "<return>")
    'dired-find-alternate-file) ; was dired-advertised-find-file
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))
                                        ; was dired-up-directory
  )
(add-hook 'dired-mode-hook 'bozo-remap-dired-keys)
(add-hook 'dired-mode-hook 
          (lambda ()
            (setq truncate-lines t)))


;; indentation
(setq-default indent-tabs-mode nil) ;use spaces instead of tabs
(setq tab-width 4)
(setq stardard-indent 4)

;recentf - recent files
(require 'recentf)
(setq 
 ;; save list to ~/.emacs.d/cache/recentf
 recentf-save-file (expand-file-name"recentf" local-cache-dir)
 recentf-max-saved-items 100
 recentf-max-menu-items 15)
;; enable
(recentf-mode t)

;; uniquify: unique buffer names
(require 'uniquify)
(setq 
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":"
 ;; regenerate buffer names after killing a buffer
 uniquify-after-kill-buffer-p t 
 ;; ignore buffers with *, *ielm*, *cscope* etc.
 uniquify-ignore-buffers-re "^\\*")

;IDO
(require 'ido)
;; enable for buffers and files
(ido-mode 'both)
(setq  ;; save state to ~/.emacs.d/cache/ido.last
 ido-save-directory-list-file (expand-file-name"ido.last" local-cache-dir)
 ;; ignore these guys
 ido-ignore-buffers 
 '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
   "^\*compilation" "^\*GTAGS" "^session\.*")
 ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~/code")
 ido-case-fold  t                 ; be case-insensitive
 ido-enable-last-directory-history t ; remember last used dirs
 ido-max-work-directory-list 30   ; should be enough
 ido-max-work-file-list      50   ; remember many
 ido-use-filename-at-point nil    ; don't use filename at point (annoying)
 ido-use-url-at-point nil         ; don't use url at point (annoying)
 ido-enable-flex-matching nil     ; don't try to be too smart
 ido-max-prospects 8              ; don't spam my minibuffer
 ido-confirm-unique-completion t  ; wait for RET, even with unique completion
 ido-default-buffer-method 'select-window) ; show buffer in frame that I want it to be
;; resize minibuf with ido completions to at most 1 line
(add-hook 'ido-minibuffer-setup-hook 
          (function
           (lambda ()
             (make-local-variable 'resize-minibuffer-window-max-height)
             (setq resize-minibuffer-window-max-height 1))))

;; helm
;; (setq helm-ff-auto-update-initial-value nil
;;       helm-ff-maximum-candidate-to-decorate 200)
;; (require 'helm-config)
;; (helm-mode 1)

;; icomplete - ido like complete in minibuffer
(icomplete-mode t)

;; jumplist
;;(require 'jumplist)

;speedbar in frame
(require 'sr-speedbar)
(setq speedbar-show-unknown-files t
      speedbar-use-images nil
      ;; speedbar-fetch-etags-command "global"
      ;; speedbar-fetch-etags-arguments '("-f" "-t")
      speedbar-use-imenu-flag nil
      speedbar-dynamic-tags-function-list '(
                                            (speedbar-fetch-dynamic-etags
                                             .
                                             speedbar-insert-etags-list)
                                            (speedbar-fetch-dynamic-imenu
                                             .
                                             speedbar-insert-imenu-list)))


;; autocompletions
(require 'module-autocompletion)

;; terminal
(require 'multi-term)

(require 'org)
;; org mode
(setq org-log-done 'time
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-on-weekday 1
      org-agenda-show-all-dates t
      org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil
      org-refile-targets '((org-agenda-files . (:maxlevel . 4))))
(defun bozo-org-mode-hook()
  (progn (auto-fill-mode 1)))
(add-hook 'org-mode-hook 'bozo-org-mode-hook)
;;
;(org-remember-insinuate) ; was broken
;; default task cycling
;; if other is needed override with buffer local settings
;; see: http://orgmode.org/manual/Tracking-TODO-state-changes.html
(setq org-todo-keywords
      '((sequence "TODO(t!)" "HOLD(h@/!)" "STARTED(s!)" "|" "DONE(d!)" "CANCELLED(c@)")))

(defun wl-org-column-view-uses-fixed-width-face ()
  ;; copy from org-faces.el
  (when (fboundp 'set-face-attribute)
    ;; Make sure that a fixed-width face is used when we have a column table.
    (set-face-attribute 'org-column nil
			:height (face-attribute 'default :height)
			:family (face-attribute 'default :family))))

(when (and (fboundp 'daemonp) (daemonp))
  (add-hook 'org-mode-hook 'wl-org-column-view-uses-fixed-width-face))

;; hi-lock for highligting entries in the buffer
(global-hi-lock-mode 1)

;; windmove - for easy switching between windows
;; default keybindings shift + up/down/left/right are fine
(windmove-default-keybindings)

;; rainbow-mode for colors whenever a pattern such as #ababab is found
(require 'rainbow-mode)
(rainbow-turn-on)

;; electricity
(electric-pair-mode t)

;; move-text
(require 'move-text)
(move-text-default-bindings)

;; ack support
(require 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)

;; zeitgeis support
;; disabled - zeitgeist causes massive slowdown when opening remote files
;; (require 'zeitgeist)

;; back button mode
;; (require 'back-button)
;; (back-button-mode 1)

;; undo tree
(require 'undo-tree)
(global-undo-tree-mode 1)

;; message mode
(add-hook 'message-mode-hook (lambda ()
                               (linum-mode -1)))

(setq auto-mode-alist
	  (append
	   '(("/tmp/mutt-.*" . message-mode))
	   auto-mode-alist))

;; ibuffer
(defalias 'list-buffers 'ibuffer)

;; run hooks
(run-hooks 'bozo-module-usability-hooks)

(provide 'module-usability)
;; module-usability.el ends here

