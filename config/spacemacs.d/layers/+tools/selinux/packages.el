(defconst selinux-packages
  '(
    (selinux-mode :location local)))


(defun selinux/init-selinux-mode ()
  (use-package selinux-mode
    :mode ("\\.te\\'" . selinux-te-mode)
    :defer t))
