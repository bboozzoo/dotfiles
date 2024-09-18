;;; bboozzoo/tweaks/config.el -*- lexical-binding: t; -*-

(defun bboozzoo/frame-decorate-toggle()
  (interactive)
  (let ((decorate (frame-parameter nil 'undecorate)))
    (message "decorate %s, not %s" decorate (not decorate))
    (set-frame-parameter nil 'undecorated (not decorate))))

(bboozzoo/frame-decorate-toggle)

;;(load-theme 'naquadah t)
;; (setq doom-theme 'doom-dracula)

;; from https://www.reddit.com/r/DoomEmacs/comments/jl6p9x/whitespacemode/
(defun bboozzoo/see-all-whitespace () (interactive)
       (setq whitespace-style (default-value 'whitespace-style))
       (setq whitespace-display-mappings (default-value 'whitespace-display-mappings))
       (whitespace-mode 'toggle))

(global-set-key (kbd "C-<f4>") 'bboozzoo/see-all-whitespace)

(defun bboozzoo/refresh-file ()
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t))

;; bind file refresh to F5
(global-set-key (kbd "C-<f5>") 'bboozzoo/refresh-file)

;; enable subword mode for moving around
(global-subword-mode +1)

(windmove-default-keybindings)
