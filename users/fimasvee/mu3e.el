(setq mu4e-drafts-folder "/gmail/drafts")
(setq mu4e-refile-folder "/gmail/archive")
(setq user-mail-address "magnars@gmail.com")

;; Shortcuts
(setq mu4e-maildir-shortcuts
      '(("/gmail/inbox" . ?i)))

;; sending mail
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/local/bin/msmtp"
      user-full-name "Magnar Sveen")

;; Borrowed from http://ionrock.org/emacs-email-and-mu.html
;; Choose account label to feed msmtp -a option based on From header
;; in Message buffer; This function must be added to
;; message-send-mail-hook for on-the-fly change of From address before
;; sending message since message-send-mail-hook is processed right
;; before sending message.
(defun choose-msmtp-account ()
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (or (save-restriction
                         (message-narrow-to-headers)
                         (message-fetch-field "from")) ""))
             (cc (or (save-restriction
                       (message-narrow-to-headers)
                       (message-fetch-field "cc")) ""))
             (account
              (cond
               ((or (string-match "@kodemaker.no" from)
                    (string-match "@kodemaker.no" cc)) "kodemaker")
               ((or (string-match "@adventur.no" from)
                    (string-match "@adventur.no" cc)) "adventur")
               (t "gmail"))))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))
(setq message-sendmail-envelope-from 'header)
(add-hook 'message-send-mail-hook 'choose-msmtp-account)

;; Dynamic sender addresses
(add-hook 'mu4e-compose-pre-hook
          (defun my-set-from-address ()
            "Set the From address based on the To address of the original."
            (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
              (if msg
                  (setq user-mail-address
                        (cond
                         ((mu4e-message-contact-field-matches msg :to "@adventur")
                          "magnar@adventur.no")
                         ((mu4e-message-contact-field-matches msg :to "@kodemaker.no")
                          "magnars@kodemaker.no")
                         (t "magnars@gmail.com")))))))

(setq mu4e-user-mail-address-list
      (list "magnars@gmail.com" "magnar@adventur.no" "magnars@kodemaker.no"))
