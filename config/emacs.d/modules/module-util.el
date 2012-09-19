(require 'cl)

;; set frame title manually
(defun bozo-set-frame-title (title)
  "Set current frame title"
  (interactive "sNew frame title: ")
  (setq frame-title-format title)
  )

;; refresh file 
(defun bozo-refresh-file ()
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t)
  )

;; jump to matching parenthesis
(defun bozo-goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))

;; rebuild all modules in local-modules-dir
(defun bozo-recompile-modules ()
  "Byte-compile all modules"
  (interactive)
  (byte-recompile-directory local-modules-dir 0)
  )

;; taken from http://nex-3.com/posts/45-efficient-window-switching-in-emacs
(defun bozo-select-next-window ()
  "Switch to the next window" 
  (interactive)
  (select-window (next-window)))

(defun bozo-select-previous-window ()
  "Switch to the previous window" 
  (interactive)
  (select-window (previous-window)))

;; select function for completing read
(defun bozo-pick-completing-read ()
  "Pick a function for completing read"
  (if (fboundp 'ido-completing-read)
      'ido-completing-read
    'completing-read)
  )

;; load modules interactively
(defun bozo-load-module ()
  "Load specific module"
  (interactive)
  (let (modules-list module-name module-file)
    (setq modules-list
          (mapcar (lambda (md) 
                    (cadr (split-string md "[-\.]")))
                  (directory-files local-modules-dir 
                                   nil 
                                   "module-[A-Za-z0-9]+\.el$")))
    (setq module-name (funcall (bozo-pick-completing-read) 
                               "Load module: " 
                               modules-list))
    (message "Loading module %s" module-name)
    (setq module-file (concat "module-" module-name))
    (message "module file %s" module-file)
    ;; require wants a symbol, make one
    (if (require (intern module-file) nil 0)
        (message "Module %s loaded" module-name)
        (message "Failed to load module %s (file %s)" module-name module-file))
    )
  )

;; check if running in terminal
(defun bozo-in-terminal-p ()
  "Return t if running in terminal"
  (not (display-graphic-p))
  )

;; set color theme from color-theme package
(defun bozo-set-color-theme (sym)
  "Set color theme to value stored in symbol sym"
  (if (and (boundp sym)
           (symbol-value sym))
      (load-theme (symbol-value sym) t)))

;; create directory if not present
(defun bozo-make-dir (f)
  (if (not (file-exists-p f))
      (progn
        (message "creating directory at %s" f)
        (make-directory f)))
  )

(defun bozo-find-package-dir (pkg)
  "Find directory corresponding to package symbol pkg."
  (let (pkg-desc)
    (setf pkg-desc (car (remove-if-not 
                         (lambda (x) (eq (car x) pkg)) 
                         package-alist)))
    (when pkg-desc
      (let ((pkg-vers (aref (cdr pkg-desc) 0)))
        (concat package-user-dir 
                "/"
                (symbol-name pkg) 
                "-" 
                (mapconcat #'int-to-string pkg-vers "."))))))

(defun bozo-list-missing-packages (required-packages)
  "List packages that are not installed but are required
 (i.e. passed in the list of required packages)."
  (remove-if 'package-installed-p
             required-packages))

(defun bozo-install-packages (packages)
  "Install desired packages. Packages is a list of symbols
corresponding to package names."
  (mapcar #'package-install packages))

(defun* bozo-refresh-package-list (&key (force nil))
  "Refresh list of packages if it's empty or :force is not
nil."
  (when (or force
            (not package-archive-contents))
    (message "Perform package list update...")
    (package-refresh-contents)))

(provide 'module-util)
;; module-util.el ends here
