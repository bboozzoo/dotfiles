(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; do not use aspell
(setq ispell-really-aspell nil)
;; use hunspell instead
(setq ispell-really-hunspell t)
(setq ispell-program-name "hunspell"
      ispell-extra-args '("-i" "utf-8"))

(setq ispell-local-dictionary-alist
      '((nil
         "[[:alpha:]]" "[^[:alpha:]]" "[']"
         nil
         ("-d" "en_US")
         nil utf-8)
        ("polish" 
         "[A-Za-z\241\243\246\254\257\261\263\266\274\277\306\312\321\323\346\352\361\363]" "[^A-Za-z\241\243\246\254\257\261\263\266\274\277\306\312\321\323\346\352\361\363]" "[.]"
         nil
         ("-d" "pl_PL")
         nil utf-8)
        ("american"
         "[A-Za-z]" "[^A-Za-z]" "[']"
         nil
         ("-d" "en_US")
         nil utf-8)
        ("british"
         "[A-Za-z]" "[^A-Za-z]" "[']"
         nil
         ("-d" "en_EN")
         nil utf-8)))

;; trick to always report utf-8 as encoding
(eval-after-load "ispell"
  '(progn (defun ispell-get-coding-system () 'utf-8)
          (setq ispell-dictionary "american")))

(add-hook 'text-mode-hook
          (lambda ()
            (flyspell-mode t)))

(setq flyspell-issue-message-flag nil)

;; (setq ispell-dictionary "american"
;;       ispell-extra-args '("-a" "-i" "utf-8"))

(run-hooks 'bozo-module-lang-hooks)
(provide 'module-lang)
