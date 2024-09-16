;;; bboozzoo/tweaks/config.el -*- lexical-binding: t; -*-

(defun bboozzoo/frame-decorate-toggle()
  (interactive)
  (let ((decorate (frame-parameter nil 'undecorate)))
    (message "decorate %s, not %s" decorate (not decorate))
    (set-frame-parameter nil 'undecorated (not decorate))))

(bboozzoo/frame-decorate-toggle)

(load-theme 'naquadah t)
