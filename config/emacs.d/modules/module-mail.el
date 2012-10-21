(require 'cl)
(require 'smtpmail)

(defcustom bozo-load-mail nil
  "Set to t if mail support should be added")

(defcustom bozo-user-name "i-did-not-set-my-username"
  "User name used in emails")

(defcustom bozo-user-address "i-did-not-set@my-email.address"
  "User's email address")

(defcustom bozo-mail-addresses nil
  "List of mail addresses. Each mail address is a cons of string 'email@address' and plist of ")

(defcustom bozo-default-smtp-server nil
  "Default SMTP server to use")

(defcustom bozo-default-smtp-service 25
  "Default SMTP port/service name")

(defcustom bozo-default-smtp-starttls-credentials nil
  "Credentials to starttls, format same as smtpmail-starttls-credentials")

(defun bozo~set-smtp-defaults ()
  ; configure defaults
  (setq smtpmail-smtp-server bozo-default-smtp-server
        smtpmail-smtp-service bozo-default-smtp-service
        smtpmail-starttls-credentials bozo-default-smtp-starttls-credentials))

(defun bozo~mail-setup-defaults ()
  "Setup some mail related defaults"
  (bozo~set-smtp-defaults)
  ; do not save .authinfo
  (setq auth-source-save-behavior nil)
  ; default username and address
  (setq user-full-name bozo-user-name)
  (setq user-mail-address bozo-user-address)
  ; mail sending
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq send-mail-function 'smtpmail-send-it)
  ; don't keep message buffers around
  (setq message-kill-buffer-on-exit t))

(defun bozo~mail-setup ()
  "Basic configuration of mail client"
  ;(add-to-list 'load-path "~/code/mu/emacs")
  (require 'mu4e)
  (setq mu4e-maildir "~/Maildir"
        mu4e-get-mail-command "offlineimap"
        mu4e-date-format-long "%a %d/%m/%y %R"
        mu4e-headers-date-format "%a %d/%m/%y %R"
        mu4e-maildir-shortcuts '( ("/INBOX" . ?i))
        ;mu4e-html2text-command "w3m -dump -T text/html"
        ;mu4e-html2text-command "html2text -utf8 -width 72 -style pretty"
        mu4e-html2text-command "python /usr/lib/python2.7/site-packages/html2text.py -b 72"
        ;mu4e-html2text-command "pandoc -f html -t plain --normalize --columns 72"
        mu4e-view-prefer-html t
        mu4e-view-show-images t)
  ; add maildir name to mu4e headers view
  (add-to-list 'mu4e-headers-fields '(:maildir . 10))
  ; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  (run-hooks 'bozo-mail-setup-hooks))

(defun bozo~matching-mail-address-p (addr mail-address-entry)
  "Check if mail address `addr' matches `mail-address-entry'"
  (let ((mail-address (car mail-address-entry)))
    (string-match mail-address addr)))

(defun bozo~change-smtp ()
  "Change SMTP settings based on From field of current message"
  (save-excursion
    (save-restriction
      (let ((from-address (progn (message-narrow-to-headers)
                                 (message-fetch-field "from"))))
        ; find entry that matches current From address in
        ; bozo-mail-addresses
        (setq matching-entry (find-if
                              (lambda (entry)
                                (bozo~matching-mail-address-p
                                 from-address entry))
                              bozo-mail-addresses))
        ; set smtpmail config if we found something
        (when matching-entry
          (let ((account-settings (cdr matching-entry)))
            (bozo-set-smtp-defaults)
            (dolist (setting account-settings)
              (bozo~set-smtpmail setting))))))))

(defun bozo~set-smtpmail (setting)
  "Set smtpmail variables based on setting"
  (let ((sym (car setting))
        (val (cdr setting)))
    ; build symbol name from smtpmail-smtp- and symbol in car
    (set (intern (concat (symbol-name 'smtpmail-smtp-)
                         (symbol-name sym)))
         val)))

(defadvice smtpmail-via-smtp
  (before smtpmail-via-smtp-ad-change-smtp (recipient
                                            smtpmail-text-buffer
                                            &optional ask-for-password))
  "Call `change-smtp' before every `smtpmail-via-smtp'."
  (with-current-buffer smtpmail-text-buffer (bozo~change-smtp)))

(ad-activate 'smtpmail-via-smtp)

(when (and bozo-load-mail
           bozo-mail-addresses)
  (bozo~mail-setup-defaults)
  (bozo~mail-setup))



;; (load-file "~/code/offlineimap-el/offlineimap.el")
;; (defun bozo-get-pass (action &optional other)
;;   (when (string= "getpass" action)
;;     (message "ask for pass now"))
;;   )
;; (defun bozo-trigger-mail-check (action &optional other)
;;   (when (string= "acctdone" action)
;;     (message "Sync finished - trigger mail check")
;;     (if (fboundp 'mu4e~proc-index)
;;         (progn (message "Reindex mail database for mu4e")
;;                (mu4e~proc-index mu4e-maildir))))
;;   )
;; (add-hook 'offlineimap-event-hooks 'bozo-get-pass)
;; (add-hook 'offlineimap-event-hooks 'bozo-trigger-mail-check)

(provide 'module-mail)
