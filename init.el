(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(canlock-password "922d24caa598a3ec5e6422d35faf8f4fa739ba71")
 '(custom-safe-themes t)
 '(gnus-boring-article-headers (quote (empty followup-to reply-to long-to many-to)))
 '(default-frame-alist
    (quote
     ((reverse . t)
      (vertical-scroll-bars)
      (tty-color-mode . true-color))))
 '(exec-path-from-shell-variables (quote ("PATH" "MANPATH" "GOPATH")))
 '(gnus-boring-article-headers (quote (empty followup-to reply-to long-to many-to)))
 '(mail-host-address "frobware.com")
 '(mm-text-html-renderer (quote shr))
 '(ns-command-modifier (quote meta))
 '(send-mail-function (quote smtpmail-send-it)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "yellow"))))
 '(diff-hunk-header ((t (:inherit diff-header))))
 '(font-lock-keyword-face ((t (:foreground "white" :weight bold))))
 '(fringe ((t (:background "grey10"))))
 '(highlight ((t (:background "grey10"))))
 '(hl-line ((t (:inherit highlight))))
 '(isearch-fail ((((class color)) (:background "red"))))
 '(linum ((t (:foreground "#656868" :background "black"))))
 '(mode-line ((t (:background "grey25" :foreground "green" :box nil))))
 '(region ((t (:background "#444" :foreground "#ffffff"))))
 '(widget-field ((t (:background "grey25")))))

;; (and (string-equal "darwin" system-type)
;;      (progn
;;        (set-default-font "-*-Source Code Pro-normal-normal-*-28-*-*-*-m-0-iso10646-1" nil nil)
;;        (menu-bar-mode)))

;;(set-default-font "-*-Source Code Pro-normal-normal-*-22-*-*-*-m-0-iso10646-1" nil nil)

(fset 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; When saving files, set execute permission if #! is in first line.
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

(defvar aim/is-darwin (eq system-type 'darwin))
(defvar aim/is-linux (eq system-type 'gnu/linux))

(defun aim/add-to-load-path (path)
  (add-to-list 'load-path (expand-file-name path user-emacs-directory)))

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message (getenv "USER"))
(setq initial-scratch-message nil)

;; Turn off 3d mode line
(set-face-attribute 'mode-line nil :box nil)

(setq vc-follow-symlinks t
      inhibit-startup-screen t
      ring-bell-function #'ignore
      mouse-yank-at-point t)

(mapc (lambda (mode)
	(when (fboundp mode)
	  (apply mode '(-1))))
      '(blink-cursor-mode
	column-number-mode
	global-linum-mode
	line-number-mode
	scroll-bar-mode
	menu-bar-mode
	tool-bar-mode))

(aim/add-to-load-path "vendor/use-package")

(require 'use-package)
(require 'package)

(mapc (lambda(p)
	(push p package-archives))
      '(("melpa" . "http://melpa.org/packages/")
	("gnu" . "http://elpa.gnu.org/packages/")
	("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'bind-key)

(use-package dired-x
  :init (progn
	  (global-set-key (kbd "C-x C-j") 'dired-jump)
	  (setq-default dired-omit-mode t)))

(use-package cmake-mode
  :defer t
  :mode ("\\.cmake$" . cmake-mode)
  :ensure t)

(use-package ag
  :ensure t
  :commands (ag ag-files ag-regexp ag-project ag-project-files ag-project-regexp)
  :config
  (progn
    (setq ag-highlight-search t
	  ag-reuse-buffers t)))

(use-package wgrep-ag
  :config
  (progn
    (setq wgrep-auto-save-buffer t))
  :ensure t)

(use-package magit
  :bind ("C-c i" . magit-status)
  :commands magit-status
  :ensure t
  :config
  (progn
    (setq magit-refresh-status-buffer nil
	  magit-auto-revert-mode nil
	  magit-diff-arguments (quote ("--function-context" "--no-ext-diff" "--stat"))
	  magit-pull-arguments nil)))

(use-package magit-gh-pulls
  :ensure t
  :commands turn-on-magit-gh-pulls
  :config
  (progn
    (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)))

(use-package markdown-mode
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode)
  :config)

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands))
  :commands smex
  :config
  (progn
    (smex-initialize)))

;; (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

(remove-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

(use-package cmake-mode
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)))

(use-package yaml-mode
  :ensure yaml-mode
  :mode "\\.ya?ml\\'")

(use-package browse-url
  :ensure t)

(use-package company
  :ensure company)

(use-package go-eldoc
  :ensure go-eldoc
  :commands go-eldoc-setup
  :init (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package golint
  :ensure golint)

(use-package flycheck
  :ensure t
  :config
  (progn
    (setq flycheck-highlighting-mode 'lines)
    (set-face-underline 'flycheck-error nil)
    (set-face-background 'flycheck-error nil)
    (set-face-underline 'flycheck-warning nil)))

(use-package ibuffer
  :config
  (progn
    (setq ibuffer-saved-filter-groups
	  (quote (("default"
		   ("dired" (mode . dired-mode))
		   ("perl" (mode . cperl-mode))
		   ("erc" (mode . erc-mode))
		   ("planner" (or
			       (name . "^\\*Calendar\\*$")
			       (name . "^diary$")
			       (mode . muse-mode)))
		   ("emacs" (or
			     (name . "^\\*scratch\\*$")
			     (name . "^\\*Messages\\*$")))
		   ("gnus" (or
			    (mode . message-mode)
			    (mode . bbdb-mode)
			    (mode . mail-mode)
			    (mode . gnus-group-mode)
			    (mode . gnus-summary-mode)
			    (mode . gnus-article-mode)
			    (name . "^\\.bbdb$")
			    (name . "^\\.newsrc-dribble")))))))
    (add-hook 'ibuffer-mode-hook
	      (lambda ()
		(ibuffer-switch-to-saved-filter-groups "default")))
    (bind-key "[::space::]" 'ibuffer-visit-buffer ibuffer-mode-map)
    (global-set-key (kbd "C-x C-b") 'electric-buffer-list)))
;;(global-set-key (kbd "C-x C-b") 'ibuffer)))

(aim/add-to-load-path "lisp")

(use-package iswitchb
  :init
  (iswitchb-mode 1))

(use-package server)

(use-package uniquify
  :init
  (progn
    (setq uniquify-buffer-name-style 'reverse)
    (setq uniquify-separator "|")
    (setq uniquify-after-kill-buffer-p t)
    (setq uniquify-ignore-buffers-re "^\\*")))

(use-package dockerfile-mode
  :ensure t)

(use-package lisp-mode
  :config
  (progn
    (bind-key "M-/" 'company-complete emacs-lisp-mode-map)
    (add-hook 'emacs-lisp-mode-hook 'company-mode t)))

(use-package ffap
  :config (ffap-bindings))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

(use-package fringe-helper
  :ensure t)

(and window-system
     (use-package git-gutter-fringe
       :ensure t))

(use-package markdown-mode
  :ensure markdown-mode)

(use-package ace-jump-mode
  :ensure ace-jump-mode
  :bind ("C-x SPC" . ace-jump-mode))

;; The platinum searcher
(use-package pt
  :ensure pt)

(use-package cc-mode
  :mode (("\\.h\\'"    . c-mode)
	 ("\\.c\\'"    . c-mode)
	 ("\\.cpp\\'"  . c++-mode)
	 ("\\.mm\\'"   . objc-mode)
	 ("\\.java\\'" . java-mode)))

(use-package "hippie-exp"
  :config
  (setq hippie-expand-try-functions-list
	'(try-expand-dabbrev
	  try-expand-dabbrev-all-buffers
	  try-expand-dabbrev-from-kill
	  try-complete-file-name-partially
	  try-complete-file-name
	  try-expand-all-abbrevs
	  try-expand-list
	  try-expand-line
	  try-complete-lisp-symbol-partially
	  try-complete-lisp-symbol))
  :bind ("M-/" . hippie-expand))

(use-package python-mode
  :init (progn
	  (set-variable 'py-indent-offset 4)
	  (set-variable 'indent-tabs-mode nil)))

(use-package company
  :ensure company)

(use-package company-go
  :ensure company-go
  :init (add-to-list 'company-backends 'company-go))

(use-package go-mode
  :ensure go-mode
  :mode "\\.go\\'"
  :commands (godoc gofmt gofmt-before-save go-remove-unused-imports)
  :init
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)
    ;; By default company-mode loads every backend it has. If you want
    ;; to only have company-mode enabled in go-mode add the following
    ;; to your emacs-config:
    (add-hook 'go-mode-hook (lambda ()
			      (set (make-local-variable 'company-backends) '(company-go))
			      (company-mode))))
  ;;(flycheck-mode)
  ;;#'go-guru-hl-identifier-mode)))
  :config
  (progn
    (bind-key "C-c C-P" 'aim/occur-go-public-functions)
    (bind-key "C-c C-f" 'gofmt go-mode-map)
    (bind-key "C-c C-g" 'go-goto-imports go-mode-map)
    (bind-key "C-c C-k" 'godoc go-mode-map)
    (bind-key "C-c C-r" 'go-remove-unused-imports go-mode-map)
    (bind-key "C-M-x" 'aim/run-go-buffer go-mode-map)
    (bind-key "M-." 'godef-jump go-mode-map)
    (bind-key "<tab>" 'company-complete go-mode-map)
    (bind-key "C-c C-r" 'go-remove-unused-imports go-mode-map)))

(use-package itail
  :ensure t)

;; (use-package tramp
;;   :init
;;   (setq tramp-ssh-controlmaster-options
;;         "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"))

(use-package tramp
  :defer nil
  :config
  (progn
    (set-default 'tramp-default-method "ssh")
    ;; /sudo:root@10.17.20.215:/var/log/juju/
    ;; /ssh:ubuntu@10.11.20.101|sudo:10.11.20.101:/var/log/juju/machine-0.log
    ;; /ssh:ubuntu@10.17.20.215|sudo:10.17.20.215:/var/log/juju/
    ;; /ssh:ubuntu@10.17.20.215|sudo:10.17.20.215:/
    ;; /ssh:ubuntu@10.17.20.215|sudo:10.17.20.215:/
    ;; http://irreal.org/blog/?p=895
    (add-to-list 'tramp-default-proxies-alist
		 '(nil "\\`root\\'" "/ssh:%h:"))
    (add-to-list 'tramp-default-proxies-alist
		 '((regexp-quote (system-name)) nil nil))
    ;;    (set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
    (setq tramp-ssh-controlmaster-options
	  (concat
	   "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
	   "-o ControlMaster=auto -o ControlPersist=no"))))

(use-package guide-key
  :ensure t
  :config (setq guide-key/guide-key-sequence '("C-c p" "C-x 4")))

(use-package wgrep
  :ensure t)

(use-package wgrep-ag
  :ensure t)

(use-package guide-key
  :ensure t
  :config (setq guide-key/guide-key-sequence '("C-c p" "C-x 4")))

(require 'aim-functions)
(require 'aim-global-keybindings)

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
	(backward-char 1)
	(if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(setq company-idle-delay 0.3)
(setq company-tooltip-limit 20)
(setq company-minimum-prefix-length 2)
(setq company-echo-delay 0)
(setq company-auto-complete nil)

(add-hook 'lisp-mode #'(complete-mode 1))

(defadvice kill-line (before check-position activate)
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
	     (just-one-space 0)
	     (backward-char 1))))

(electric-indent-mode 1)

(defun aim/run-go-buffer ()
  (interactive)
  (shell-command (format "go run %s" (buffer-file-name (current-buffer)))))

(defun isearch-face-settings ()
  (interactive)
  (set-face-foreground 'isearch "black")
  (set-face-background 'isearch "yellow")
  (set-face-foreground 'lazy-highlight "black")
  (set-face-background 'lazy-highlight "orange")
  (custom-set-faces '(isearch-fail ((((class color)) (:background "red"))))))

(eval-after-load "isearch"
  `(isearch-face-settings))

(defun aim/occur-go-public-functions ()
  (interactive)
  (occur "^func [A-Z]"))		;which is clearly broken

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
	      vc-ignore-dir-regexp
	      tramp-file-name-regexp))

(and (file-exists-p "~/repos/xml-rpc/xml-rpc.el")
     (add-to-list 'load-path "~/repos/xml-rpc"))

(and (file-exists-p "~/repos/lava-mode/lava-mode.el")
     (progn
       (use-package json-mode
	 :defer nil
	 :ensure t)
       (use-package log4j-mode
	 :defer nil
	 :ensure t)
       (use-package popup
	 :defer nil
	 :ensure t)
       (add-to-list 'load-path "~/repos/lava-mode")
       (require 'lava-mode)))

(defun aim/sj ()
  (interactive)
  (save-excursion
    (with-output-to-temp-buffer "*sj*"
      (shell-command (format "sj --show-job %s" (buffer-file-name (current-buffer))) "*sj*")
      (pop-to-buffer "*sj*")
      (lava-mode-submit-job nil))))

(defmacro with-x-environment (&rest body)
  `(let ((process-environment
	  (cons (concat "DISPLAY=" (getenv "DISPLAY" (selected-frame)))
		process-environment)))
     (if (getenv "XAUTHORITY" (selected-frame))
	 (setq process-environment
	       (cons (concat "XAUTHORITY=" (getenv "XAUTHORITY" (selected-frame)))
		     process-environment)))
     ,@body))

(defun x-terminal-copy (text)
  (with-temp-buffer
    (insert text)
    (with-x-environment
     (call-process-region (point-min) (point-max) "xsel" nil nil nil "-bi"))))

(defadvice x-select-text
    (before x-select-text-in-tty activate)
  "Use xsel to copy to the X clipboard when running in a terminal under X."
  (when (and (eq (framep (selected-frame)) t)
	     (getenv "DISPLAY" (selected-frame)))
    (x-terminal-copy text)))

(defun x-terminal-paste ()
  (with-temp-buffer
    (with-x-environment
     (call-process "xsel" nil t nil "-bo"))))

(defadvice x-cut-buffer-or-selection-value
    (before x-cut-buffer-or-selection-value-in-tty activate)
  "Use xsel to paste from the X clipboard when running in a terminal under X."
  (when (and (eq (framep (selected-frame)) t)
	     (getenv "DISPLAY" (selected-frame)))
    (x-terminal-paste)))

(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t)

(setq sentence-end-double-space nil)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(global-set-key (kbd "M-z") 'zap-up-to-char)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(put 'scroll-left 'disabled nil)

(load-library "python")

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; (setq interpreter-mode-alist
;;       (cons '("python" . python-mode) interpreter-mode-alist)
;;       python-mode-hook '(lambda () (progn
;;				     (set-variable 'py-indent-offset 4)
;;				     (set-variable 'indent-tabs-mode t))))

(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

(setq twittering-icon-mode t)

;;; IRC

(setq rcirc-server-alist
      '(("irc.freenode.net" :port 6697 :encryption tls
	 :channels ("#rcirc" "#emacs" "#emacswiki"))))

;; Identification for IRC server connections
(setq rcirc-default-user-name "frobware"
      rcirc-default-nick      "frobware"
      rcirc-default-full-name "Andrew McDermott")

;; Enable automatic authentication with rcirc-authinfo keys.
(setq rcirc-auto-authenticate-flag t)

;; Enable logging support by default.
(setq rcirc-log-flag      t
      rcirc-log-directory (expand-file-name ".rcirclogs" (getenv "HOME")))

;; Some UI options which I like better than the defaults.
(rcirc-track-minor-mode 1)

(setq rcirc-prompt      "»» "
      rcirc-time-format "%H:%M "
      rcirc-fill-flag   nil)

(global-set-key (kbd "C-c I") 'irc)

(use-package rcirc-notify
  :ensure t)

(eval-after-load 'rcirc '(require 'rcirc-notify))
(eval-after-load 'rcirc '(rcirc-notify-add-hooks))

(require 'auth-source)

(message (auth-source-search :port '("nickserv")
			     :require '(:port :user :password)))

(defadvice rcirc (before rcirc-read-from-authinfo activate)
  "Allow rcirc to read authinfo from ~/.authinfo.gpg via the auth-source API.
This doesn't support the chanserv auth method"
  (unless arg
    (dolist (p (auth-source-search :port '("nickserv")
				   :require '(:port :user :password)))
      (let ((secret (plist-get p :secret))
	    (method (intern (plist-get p :port))))
	(add-to-list 'rcirc-authinfo
		     (list (plist-get p :host)
			   method
			   (plist-get p :user)
			   (if (functionp secret)
			       (funcall secret)
			     secret)))))))

(require 'ansi-color)
(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))

(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defvar dark-background nil)

(defun toggle-dark-background ()
  (interactive)
  (let ((difficult-colors
	 '("red" "blue" "medium blue")))
    (mapc
     (lambda (face)
       (and (member (face-attribute face :foreground)  difficult-colors)
	    (set-face-bold-p face (not dark-background))))
     (face-list)))
  (setq dark-background (not dark-background)))

(require 'desktop)

(setq desktop-buffers-not-to-save
      (concat "\\("
	      "\\.go\\"
	      "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
	      "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
	      "\\)$"))

(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

;;(desktop-save-mode 1)

(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))

;;(add-hook 'auto-save-hook 'my-desktop-save)

;; (require 'recentf)
;; (recentf-mode 1)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")

(and (require 'notmuch nil 't)
     (progn
       (require 'notmuch-address)

       (aim/add-to-load-path "vendor/rbt")
       (require 'rbt)

       (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
       (require 'notmuch)

       (setq notmuch-message-headers '("To" "Subject")) ; The default list is '("Subject" "To" "Cc" "Date").
       (setq notmuch-show-indent-messages-width 1) ; The default is 1.

       ;; Show HTML mail by default, and keep the text/plain hidden.
       ;; (setq notmuch-multipart/alternative-discouraged '("text/plain" "text/html"))
       (setq notmuch-multipart/alternative-discouraged '("text/html" "text/plain"))

       ;; By default the "show hidden multipart" buttons are very bright (and distracting) in my color scheme.
       ;; Make them be the same color as the email's body text.
       (set-face-foreground 'message-mml (face-attribute 'default :foreground))))

;;(add-to-list 'default-frame-alist '(tty-color-mode  . -1))

(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
	  (progn
	    (goto-char start)
	    (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
	(replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(use-package peep-dired
  :ensure t
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
	      ("P" . peep-dired)))

(defun get-frame-name (&optional frame)
  (interactive)
  "Return the string that names FRAME (a frame).  Default is selected frame."
  (unless frame (setq frame (selected-frame)))
  (if (framep frame)
      (cdr (assq 'name (frame-parameters frame)))
    (error "Function `get-frame-name': Argument not a frame: `%s'" frame)))

(use-package dumb-jump
  :ensure t)

(defun open-var-log-juju (hostname)
  "Open Juju log directory"
  (interactive "shost: ")
  (let ((filename (format "/ssh:%s|sudo:%s:/var/log/juju/" hostname hostname)))
    (find-file filename)))

(defun visit-dir (hostname)
  "Open Juju log directory"
  (interactive "shost: ")
  (let ((filename (format "/ssh:%s|sudo:%s:/" hostname hostname)))
    (find-file filename)))

(defun ubuntu-visit-dir (hostname)
  "Open Juju log directory"
  (interactive "shost: ")
  (let ((filename (format "/ssh:%s:~" hostname hostname)))
    (find-file filename)))

(if (file-exists-p "/usr/local/go1.7.1/misc/go-guru.el")
     (use-package go-guru
       :load-path "/usr/local/go1.7.1/misc"
       :config (add-to-list 'exec-path "/usr/local/go1.7.1/bin")))

(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook 'wgrep-ag-setup)

(global-set-key (kbd "M-.") 'dumb-jump-go)

(unless (window-system)
  ;; use xclip to copy/paste in emacs-nox
  (when (getenv "DISPLAY")
    (defun xclip-cut-function (text &optional push)
      (with-temp-buffer
	(insert text)
	(call-process-region (point-min) (point-max) "xclip" nil 0 nil "-i" "-selection" "clipboard")))
    (defun xclip-paste-function()
      (let ((xclip-output (shell-command-to-string "xclip -o -selection clipboard")))
	(unless (string= (car kill-ring) xclip-output)
	  xclip-output )))
    (setq interprogram-cut-function 'xclip-cut-function)
    (setq interprogram-paste-function 'xclip-paste-function))
  (defun terminal-init-screen ()
    "Terminal initialization function for screen."
    ;; Use the xterm color initialization code.
    (load "term/xterm")
    (xterm-register-default-colors)
    (tty-set-up-initial-frame-faces))
  ;; Set color-theme if running in X or a high-color terminal
  (defun setup-color-theme-p ()
    "Returns true if it looks like the display can handle 24-bit colors"
    (or (display-graphic-p)
	(< 256 (display-color-cells))
	(getenv "KONSOLE_DBUS_SESSION")))
  (defun setup-color-theme ()
    "Set up my color theme"
    (when (setup-color-theme-p)
      (set-face-attribute 'default nil :background "#000000")))
  (terminal-init-screen)
  (add-hook 'window-setup-hook 'setup-color-theme)
  ;; xterm mouse support
  (require 'mouse)
  (xterm-mouse-mode t))

(unless (server-running-p)
  (server-start))

(message "Done")
