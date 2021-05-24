(use-package simple
  :custom (read-mail-command #'gnus))

;; /usr/local/Cellar/dovecot/2.3.14/libexec/dovecot/imap

(use-package auth-source
  :config
  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))
  (setq user-full-name "Andrew McDermott")
  (setq user-mail-address "amcdermo@redhat.com"))

(use-package epa-file
  :straight (:type built-in)
  :config
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
  :init
  (epa-file-enable))

(use-package gnus
  :straight (:type built-in)
  :config
  (setq gnus-agent nil)
  (setq message-sendmail-f-is-evil 't)
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq gnus-always-read-dribble-file t)
  (setq gnus-article-save-directory "~/.config/gnus/news")
  (setq gnus-cache-directory "~/.config/gnus/news/cache")
  (setq gnus-check-new-newsgroups nil)
  (setq gnus-home-directory "~/.config/gnus")
  (setq gnus-kill-files-directory "~/.config/gnus/news")
  (setq gnus-novice-user nil)
  (setq gnus-read-active-file 'some)
  (setq gnus-registry-cache-file "~/.config/gnus/gnus.registry.eld")
  (setq gnus-select-method '(nnnil))
  (setq gnus-secondary-select-methods
	'((nnimap ""
		  (nnimap-stream shell)
		  (nnimap-shell-program "/usr/local/Cellar/dovecot/2.3.14/libexec/dovecot/imap -c ~/.config/gnus/dovecotrc-work-mbsync"))))
  (setq gnus-startup-file "~/.config/gnus/newsrc")
  (setq gnus-use-dribble-file t)
  (setq mail-source-directory "~/.config/gnus/mail")
  (setq mail-user-agent 'gnus-user-agent) ; also works with `sendmail-user-agent'
  (setq nndraft-directory "~/.config/gnus/drafts")
  (setq nnfolder-directory "~/.config/gnus/archive")
  (setq nnmh-directory "~/.config/gnus/drafts")
  (setq nnml-directory "~/.config/gnus/mail")
  (setq nntp-authinfo-file "~/.authinfo.gpg")
  (setq gnus-use-bbdb t))

(use-package gnus-group
  :straight (:type built-in)
  :after gnus
  :config
  (setq gnus-list-groups-with-ticked-articles nil)
  (setq gnus-group-sort-function
	'((gnus-group-sort-by-unread)
	  (gnus-group-sort-by-alphabet)
	  (gnus-group-sort-by-rank)))
  (setq gnus-group-line-format "%M%p%P%5y:%B%(%g%)\n")
  (setq gnus-group-mode-line-format "%%b")
  :hook ((gnus-group-mode-hook . hl-line-mode)
	 (gnus-select-group-hook . gnus-group-set-timestamp))
  :bind (:map gnus-group-mode-map
	      ("M-n" . gnus-topic-goto-next-topic)
	      ("M-p" . gnus-topic-goto-previous-topic)))

(use-package gnus-sum
  :straight (:type built-in)
  :after (gnus gnus-group)
  :config
  (setq gnus-auto-select-first nil)
  (setq gnus-summary-ignore-duplicates t)
  (setq gnus-suppress-duplicates t)
  (setq gnus-summary-goto-unread nil)
  (setq gnus-summary-make-false-root 'adopt)
  (setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
  (setq gnus-thread-sort-functions
	'((not gnus-thread-sort-by-number)
	  (not gnus-thread-sort-by-date)))
  (setq gnus-subthread-sort-functions
	'gnus-thread-sort-by-date)
  (setq gnus-thread-hide-subtree nil)
  (setq gnus-thread-ignore-subject t)
  (setq gnus-user-date-format-alist
	'(((gnus-seconds-today) . "Today at %R")
	  ((+ 86400 (gnus-seconds-today)) . "Yesterday, %R")
	  (t . "%Y-%m-%d %R")))
  (setq gnus-summary-line-format "%U%R%z %-16,16&user-date;  %-30,30f  %B%s\n")
  (setq gnus-summary-mode-line-format "Gnus: %p (%U)")
  (setq gnus-sum-thread-tree-false-root "")
  (setq gnus-sum-thread-tree-indent " ")
  (setq gnus-sum-thread-tree-leaf-with-other "├─➤ ")
  (setq gnus-sum-thread-tree-root "")
  (setq gnus-sum-thread-tree-single-leaf "└─➤ ")
  (setq gnus-sum-thread-tree-vertical "│")
  :hook
  (gnus-summary-exit-hook . gnus-topic-sort-groups-by-alphabet)
  (gnus-summary-exit-hook . gnus-group-sort-groups-by-rank))

(use-package gnus-art
  :straight (:type built-in)
  :after gnus
  :config
  (setq gnus-article-browse-delete-temp 'ask)
  (setq gnus-article-over-scroll nil)
  (setq gnus-article-show-cursor t)
  (setq gnus-article-sort-functions
	'((not gnus-article-sort-by-number)
	  (not gnus-article-sort-by-date)))
  (setq gnus-article-truncate-lines nil)
  (setq gnus-html-frame-width 80)
  (setq gnus-html-image-automatic-caching t)
  (setq gnus-inhibit-images t)
  (setq gnus-inhibit-expiry t)
  (setq gnus-max-image-proportion 0.7)
  (setq gnus-treat-display-smileys nil)
  (setq gnus-article-mode-line-format "%G %S %m")
  (setq gnus-visible-headers
	'("^From:" "^Subject:" "^To:" "^Cc:" "^Newsgroups:" "^Date:"
	  "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
	  "^X-Mailer:"))
  (setq gnus-sorted-header-list gnus-visible-headers)
  (setq gnus-extra-headers '(To Cc)
	nnmail-extra-headers gnus-extra-headers)
  :hook (gnus-article-mode-hook . (lambda ()
				    (setq-local fill-column 80)))
  :bind (:map gnus-article-mode-map
	      ("i" . gnus-article-show-images)
	      ("s" . gnus-mime-save-part)
	      ("o" . gnus-mime-copy-part)))

(use-package gnus-score
  :straight (:type built-in)
  :ensure gnus
  :config
  (setq
   ;; Gnus article scoring entries.
   gnus-home-score-file "~/.config/gnus/score"
   ;; Number of days to keep score.
   gnus-score-expiry-days 60
   ;; Adaptive score list.
   gnus-default-adaptive-score-alist
   '((gnus-unread-mark)
     (gnus-ticked-mark (from 4))
     (gnus-dormant-mark (from 5))
     (gnus-saved-mark (from 20) (subject 5))
     (gnus-del-mark (from -2) (subject -5))
     (gnus-read-mark (from 2) (subject 1))
     (gnus-killed-mark (from -1) (subject -3)))
   ;; Decay score over time.
   gnus-decay-scores t
   ;; Score decay rate.
   gnus-score-decay-constant 1
   gnus-score-decay-scale 0.03))

(eval-after-load "mm-decode"
  '(progn
     (add-to-list 'mm-discouraged-alternatives "text/html")
     (add-to-list 'mm-discouraged-alternatives "text/richtext")))

;; with Emacs 23.1, you have to set this explicitly (in MS Windows)
;; otherwise it tries to send through OS associated mail client
(setq message-send-mail-function 'message-send-mail-with-sendmail)

;; we substitute sendmail with msmtp
(setq sendmail-program "/run/current-system/sw/bin/msmtp")

;;need to tell msmtp which account we're using
(setq message-sendmail-extra-arguments '("-a" "gmail"))

;; you might want to set the following too
(setq user-full-name "Andrew McDermott")
(setq user-mail-address "amcdermo@redhat.com")
