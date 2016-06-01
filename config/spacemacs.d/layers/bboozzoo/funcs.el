;;; funcs.el --- bboozzoo customization layer
;;
;;; License: MIT

(defun bboozzoo/refresh-file ()
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t))

(defun bboozzoo/set-frame-title (title)
  "Set current frame title"
  (interactive "sNew frame title: ")
  (setq frame-title-format title))

(defun bboozzoo/server-shutdown ()
  "Kill server and save buffers"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defalias 'bozo-server-shutdown 'bboozzoo/server-shutdown)

(defun bozo-terminal-keymap-fix ()
  (mapc (lambda (key-seq)
          (progn
            (message "mapping key: %s as %s" (car key-seq) (cdr key-seq))
            (define-key input-decode-map (car key-seq) (cdr key-seq))))
        '(("\e[1;2D" . [S-left])
          ("\e[1;2C" . [S-right])
          ("\e[1;2A" . [S-up])
          ("\e[1;2B" . [S-down])
          ("\e[1;3A" . [M-up])
          ("\e[1;3B" . [M-down])
          ("\e[1;3C" . [M-right])
          ("\e[1;3D" . [M-left])
          ("\e[1;5B" . [C-down])
          ("\e[1;5D" . [C-left])
          ("\e[1;5A" . [C-up])
          ("\e[1;5C" . [C-right]))))

(defadvice terminal-init-xterm (after fix-terminal-keymap activate)
  (bozo-terminal-keymap-fix))

(defadvice terminal-init-screen (after fix-terminal-keymap activate)
  (bozo-terminal-keymap-fix))

(defun bboozzoo/scroll-init()
  (setq
    scroll-margin 0
    scroll-conservatively 100000
    scroll-up-aggressively 0.0
    scroll-down-aggressively 0.0
    scroll-preserve-screen-position t))

(defun bboozzoo/init()
  (bboozzoo/scroll-init))
