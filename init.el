;; -*- lexical-binding: t; -*-

(setq custom-file null-device)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Happiness delivered in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

;; From https://github.com/raxod502/straight.el/issues/757
(setq straight-disable-native-compile nil)
(setq native-comp-async-report-warnings-errors nil)

(unless (functionp 'json-serialize)
  (error "**** you don't have a json-serialize built-in function ****"))

(unless (functionp 'module-load)
  (error "**** you don't have modules enabled ****"))

(custom-set-variables '(straight-use-package-by-default t)
		      '(straight-repository-branch "develop")
		      ;; straight-check-for-modifications '(check-on-save))
		      '(straight-check-for-modifications nil))

(setq-default straight-vc-git-default-clone-depth 1)

(setq use-package-always-defer t
      use-package-verbose t
      use-package-always-ensure t
      use-package-ignore-unknown-keywords t)

(defun aim/straight-bootstrap nil
  "Bootstrap straight."
  (defvar bootstrap-version)		;dynamically bound
  (let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
	  (url-retrieve-synchronously
	   "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	   'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)
    (straight-use-package 'use-package)))

(require 'package)

(if (package-installed-p 'use-package)
    (eval-when-compile
      (require 'use-package))
  (aim/straight-bootstrap))

(require 'term)
;; prevent cursor blinking in remote terminal sessions.
(setq visible-cursor nil)

(use-package pinentry
  :config
  (setq epa-pinentry-mode 'loopback) ; prevent GUI input
  (pinentry-start))

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package clipetty
  ;; You can invoke Clipetty explicitly from a key binding to copy a
  ;; region to the clipboard rather than using either the local or
  ;; global minor modes. To that end, Clipetty has a function called
  ;; clipetty-kill-ring-save which I like to bind to M-w like so:
  ;; :bind ("M-w" . clipetty-kill-ring-save))
  :hook (after-init . global-clipetty-mode))

(require 'cc-mode)
(setq c-default-style '((java-mode . "java")
			(awk-mode . "awk")
			(other . "linux")))

(when (memq window-system '(mac ns))
  (require 'exec-path-from-shell)
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

;; avy gives us fluent jump-to-line commands mapped to the home row.
(use-package avy
  :bind ("C-c l" . avy-goto-line))

(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  :custom (which-key-idle-delay 1.2))

(require 'select)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator "|"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

(require 'ffap)
(setq ffap-machine-p-known 'reject)
(ffap-bindings)

(use-package emacs
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'set-goal-column 'disabled nil)
  :custom
  (auto-compression-mode t)
  (sentence-end-double-space nil)
  (blink-cursor-mode nil)
  (vc-follow-symlinks t)
  (inhibit-startup-screen t)
  (inhibit-splash-screen t)
  (inhibit-startup-message t)
  (inhibit-startup-echo-area-message (getenv "USER"))
  (initial-scratch-message nil)
  (ring-bell-function #'ignore)
  (mouse-yank-at-point t)
  ;; (default-frame-alist '((menu-bar-lines 0)
  ;;			 (tool-bar-lines 0)
  ;;			 (vertical-scroll-bars)))
  ;; (initial-frame-alist '((vertical-scroll-bars)))
  (inhibit-startup-screen t "Don't show splash screen")
  (use-dialog-box nil "Disable dialog boxes")
  (x-gtk-use-system-tooltips nil)
  (enable-recursive-minibuffers t "Allow minibuffer commands in the minibuffer")
  (debug-on-error nil))

;;; where can I put these? simple?
(defalias 'ttl 'toggle-truncate-lines)
(fset 'yes-or-no-p 'y-or-n-p)

(and (executable-find "direnv")
     (use-package direnv
       ;; :init
       ;; (add-hook 'prog-mode-hook #'direnv-update-environment)
       :custom
       (direnv-always-show-summary nil)
       :config
       (direnv-mode)))

(use-package dired-narrow
  :bind (:map dired-mode-map
	      ("/" . dired-narrow)))

;; (use-package hippie-exp
;;   :straight (:type built-in)
;;   :defer nil
;;   :custom
;;   (hippie-expand-try-functions-list
;;    '(try-expand-dabbrev
;;      try-expand-dabbrev-from-kill
;;      try-expand-dabbrev-all-buffers
;;      try-complete-file-name-partially
;;      try-complete-file-name
;;      try-expand-all-abbrevs
;;      try-expand-list
;;      try-expand-line
;;      try-complete-lisp-symbol-partially
;;      try-complete-lisp-symbol))
;;   :bind
;;   (("M-/" . hippie-expand)))

;; (use-package files
;;   :straight (:type built-in)
;;   :defer nil
;;   :custom
;;   (require-final-newline t)
;;   (backup-by-copying t)
;;   (backup-directory-alist
;;    `((".*" . ,(locate-user-emacs-file "backups"))))
;;   (auto-save-file-name-transforms
;;    `((".*" ,temporary-file-directory t)))
;;   (delete-old-versions t)
;;   (kept-new-versions 20)
;;   (kept-old-versions 10)
;;   (version-control t))

(require 'executable)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; https://github.com/hrs/dotfiles/blob/master/emacs/.emacs.d/configuration.org
;; thanks man!

(setq hrs/default-font-size 20)
(setq hrs/default-font "JetBrains Mono")
(setq hrs/current-font-size hrs/default-font-size)
(setq hrs/font-change-increment 1.1)

(defun hrs/font-code ()
  "Return a string representing the current font (like \"Inconsolata-14\")."
  (concat hrs/default-font "-" (number-to-string hrs/current-font-size)))

(defun hrs/set-font-size ()
  "Set the font to `hrs/default-font' at `hrs/current-font-size'.
Set that for the current frame, and also make it the default for
other, future frames."
  (let ((font-code (hrs/font-code)))
    (add-to-list 'default-frame-alist (cons 'font font-code))
    (set-frame-font font-code)))

(defun hrs/reset-font-size ()
  "Change font size back to `hrs/default-font-size'."
  (interactive)
  (setq hrs/current-font-size hrs/default-font-size)
  (hrs/set-font-size))

(defun hrs/increase-font-size ()
  "Increase current font size by a factor of `hrs/font-change-increment'."
  (interactive)
  (setq hrs/current-font-size
	(ceiling (* hrs/current-font-size hrs/font-change-increment)))
  (hrs/set-font-size))

(defun hrs/decrease-font-size ()
  "Decrease current font size by a factor `hrs/font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq hrs/current-font-size
	(max 1
	     (floor (/ hrs/current-font-size hrs/font-change-increment))))
  (hrs/set-font-size))

(define-key global-map (kbd "C-)") 'hrs/reset-font-size)
(define-key global-map (kbd "C-+") 'hrs/increase-font-size)
(define-key global-map (kbd "C--") 'hrs/decrease-font-size)

(use-package ag
  :custom
  (ag-highligh-search t)
  (ag-reuse-buffers t)
  (ag-reuse-window t)
  :bind
  ("M-s a" . ag-project))

(use-package wgrep
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(use-package wgrep-ag			;TODO
  :after ag)

(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands))
  :config
  (smex-initialize))

(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line."
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))

(use-package yaml-mode
  :bind
  ("C-c C-j" . aj-toggle-fold))

;; (use-package tramp
;;   :config
;;   (put 'temporary-file-directory 'standard-value `(,temporary-file-directory))
;;   :custom
;;   (tramp-backup-directory-alist backup-directory-alist)
;;   (tramp-default-method "ssh")
;;   (tramp-default-proxies-alist nil)
;;   ;; shell prompt additions for NixOS
;;   (tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*"))

(use-package magit
  ;; :config
  ;; (global-magit-file-mode -1)
  ;; Removed by https://github.com/magit/magit/pull/4237
  :custom
  (magit-diff-arguments (quote ("--function-context" "--no-ext-diff" "--stat")))
  :bind
  (("C-c i" . magit-status)
   ("C-c I" . magit-dispatch)))

(use-package git-commit			;TODO (spell)
  :hook (git-commit-setup . git-commit-turn-on-flyspell))

(use-package git-timemachine)

(use-package git-gutter
  :config
  (setq git-gutter:modified-sign " "
	git-gutter:added-sign " "
	git-gutter:deleted-sign " "
	git-gutter:lighter " GG")
  (set-face-background 'git-gutter:modified "DarkGoldenrod4")
  (set-face-foreground 'git-gutter:added "dark green")
  (set-face-foreground 'git-gutter:deleted "dark red")
  (global-git-gutter-mode 1))

(use-package copy-as-format
  :config
  (setq copy-as-format-default "slack")
  :bind
  (:map mode-specific-map
	:prefix-map copy-as-format-prefix-map
	:prefix "f"
	("f" . copy-as-format)
	("a" . copy-as-format-asciidoc)
	("b" . copy-as-format-bitbucket)
	("d" . copy-as-format-disqus)
	("g" . copy-as-format-github)
	("l" . copy-as-format-gitlab)
	("c" . copy-as-format-hipchat)
	("h" . copy-as-format-html)
	("j" . copy-as-format-jira)
	("m" . copy-as-format-markdown)
	("w" . copy-as-format-mediawiki)
	("o" . copy-as-format-org-mode)
	("p" . copy-as-format-pod)
	("r" . copy-as-format-rst)
	("s" . copy-as-format-slack)))

;;(use-package xref)

(use-package protobuf-mode)

(use-package nix-mode
  :mode "\\.nix\\'"
  :custom
  (nix-indent-function #'nix-indent-line)
  :bind (:map nix-mode-map
	      ("C-c C-j" . aj-toggle-fold)))

;; not sure if these two should be here
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker-compose-mode)

;; Making it easier to discover Emacs key presses.
(use-package which-key
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay most-positive-fixnum)
  (setq which-key-idle-secondary-delay 1e-100)
  (which-key-mode +1))

(defvar browse-url-mosaic-program nil)

(use-package browse-at-remote)
(require 'browse-url)
(use-package cmake-mode)
(use-package json-mode)
(use-package markdown-mode)
(use-package pass)
(use-package x509-mode)
(use-package xterm-color)

(use-package toml-mode)

(use-package cargo)

(use-package rust-mode
  :hook
  (rust-mode . yas-minor-mode))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

(use-package ws-butler
  :hook
  (prog-mode . ws-butler-mode))

(require 'ibuffer)

;; (use-package ibuffer
;;   :bind
;;   (:map ibuffer-mode-map
;;	("SPC" . ibuffer-visit-buffer)))

(use-package ibuffer-vc
  :after (ibuffer vc)
  :bind (:map ibuffer-mode-map
	      ("/ V" . ibuffer-vc-set-filter-groups-by-vc-root)
	      ("/ <deletechar>" . ibuffer-clear-filter-groups)))

(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

(use-package notmuch
  :init
  (setq notmuch-search-oldest-first nil
	mail-user-agent 'message-user-agent
	notmuch-wash-wrap-lines-length 80
	notmuch-tree-show-out t)
  :config
  (setq notmuch-search-oldest-first nil
	mail-user-agent 'message-user-agent
	notmuch-wash-wrap-lines-length 80
	notmuch-tree-show-out t)
  (setq notmuch-saved-searches
	'((:key "i" :name "inbox" :query "tag:inbox")
	  (:key "u" :name "unread" :query "tag:unread")
	  (:key "g" :name "github/mentions" :query "tag:github/mentions is:unread")
	  (:key "b" :name "bugs" :query "tag:bugs date:today")
	  (:key "T" :name "today" :query "date:today and not tag:trash")
	  (:key "U" :name "unread today" :query "date:today is:unread")
	  (:key "F" :name "flagged" :query "tag:flagged")
	  (:key "S" :name "sent" :query "tag:Sent Mail"))))

(use-package langtool
  :config
  (setq langtool-http-server-host "localhost"
	langtool-http-server-port 8081
	langtool-default-language "en-GB")
  :bind
  (:map git-commit-mode-map
	("C-x `" . langtool-correct-buffer)))

;; This is to speedup LSP.
;;
;; Increase the amount of data which Emacs reads from the process.
;; Again the emacs default is too low 4k considering that the some
;; of the language server responses are in 800k - 3M range.
;; (use-package process
;;   :straight (:type built-in)
;;   :custom
;;   (read-process-output-max (* 1 (* 1024 1024))))

(use-package company
  :custom
  (company-idle-delay 0)
  (company-tooltip-limit 20)
  (company-minimum-prefix-length 3)
  (company-echo-delay 0)
  (company-require-match nil)
  (company-tooltip-align-annotations t) ; Align annotation to the right side.
  (company-auto-complete nil)
  :bind
  (:map company-active-map
	("C-n" . company-select-next-or-abort)
	("C-p" . company-select-previous-or-abort))
  :hook
  (after-init . global-company-mode))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-select-previous)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

;; (use-package company-quickhelp
;;   :custom
;;   (company-quickhelp-delay 3)
;;   (company-quickhelp-mode 1))

(use-package company-shell
  :after company
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env)))

(use-package flycheck)

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
	lsp-enable-file-watchers nil
	lsp-enable-on-type-formatting nil
	lsp-enable-snippet nil
	lsp-prefer-capf t
	lsp-prefer-flymake nil)
  :config
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.direnv$")
  (lsp-enable-which-key-integration t)
  :bind (("C-c d" . lsp-describe-thing-at-point)
	 ("C-c e n" . flycheck-next-error)
	 ("C-c e p" . flycheck-previous-error)
	 ("C-c e l" . flycheck-list-errors)
	 ("C-c e r" . lsp-find-references)
	 ("C-c e R" . lsp-rename)
	 ("C-c e i" . lsp-find-implementation)
	 ("C-c e t" . lsp-find-type-definition))
  :commands
  (lsp lsp-deferred))

;;(use-package lsp-treemacs)		;

(defun aim/lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

;;(use-package go-test)

(use-package go-mode
  :mode "\\.go\\'"
  :custom
  (go-fontify-function-calls nil)
  (go-fontify-variables nil)
  :bind (:map go-mode-map
	      ("C-c C-n" . go-run)
	      ("C-c C-c" . go-coverage)
	      ("C-c ."   . go-test-current-test)
	      ("C-c f"   . go-test-current-file)
	      ("C-c a"   . go-test-current-project))
  :hook ((go-mode . lsp-deferred)
	 (before-save . aim/lsp-go-install-save-hooks)))

(use-package go-add-tags)

(require 'hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

(require 'recentf)
(setq recentf-max-menu-items 32
      recentf-max-saved-items 325)
(recentf-mode 1)
(global-key-binding (kbd "C-x C-r") 'recentf-open-files)

(defun aim/run-go-buffer ()
  "Run current buffer using go run."
  (interactive)
  (shell-command (format "go run %s" (buffer-file-name (current-buffer)))))

(defun aim/fullscreen ()
  "Toggle fullscreen."
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defun aim/revert-buffer-now ()
  "Revert-(current-buffer) asking no questions."
  (interactive)
  (revert-buffer nil t))

(defun aim/tramp-borked ()
  "Delete all tramp buffers and their connections."
  (interactive)
  (tramp-cleanup-all-connections)
  (tramp-cleanup-all-buffers))

(defun my-minibuffer-setup ()
  "Stop squinting."
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 1.25))))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

;;; Require confirmation before interactively evaluating code blocks
;;; in Org buffers. The default value of this variable is t, meaning
;;; confirmation is required for any code block evaluation.
(setq org-confirm-babel-evaluate nil)

(use-package lsp-ui
  :after lsp-mode
  :diminish
  ;; :custom-face
  ;; (lsp-ui-doc-background ((t (:background nil))))
  ;; (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
	      ;; ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	      ;; ([remap xref-find-references] . lsp-ui-peek-find-references)
	      ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-enable-symbol-highlighting nil)
  (lsp-ui-peek-fontify 'always)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-doc-use-webkit t)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil)
  (lsp-eldoc-hook nil))

;; The buffer *Flymake log* tends to fill up with things like:
;; > Warning [flymake init.el]: Disabling backend flymake-proc-legacy-flymake
;; > because (error Can’t find a suitable init function)
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

;; https://github.com/emacs-lsp/lsp-mode/issues/631#issuecomment-457866187
(add-hook 'c++-mode-hook
	  (lambda ()
	    (setq flymake-diagnostic-functions (list 'lsp--flymake-backend))))

(require 'simple)
(setq kill-ring-max 30000
      truncate-lines t)
(column-number-mode 1)

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server))

(if (package-installed-p 'vterm)
    (use-package vterm
      :init
      (setq vterm-ignore-blink-cursor t)))

(add-hook 'before-save-hook 'whitespace-cleanup)
;;(setq notmuch-command "remote-notmuch.sh")

(add-to-list 'load-path
	     (expand-file-name "~/src/github.com/frobware/emacs.d/modus-themes"))

(require 'modus-themes)

(load-theme 'modus-vivendi t t)		;dark
(load-theme 'modus-operandi t t)	;lightness

;; Add all your customizations prior to loading the themes
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-region '(bg-only no-extend))

;; Load the theme files before enabling a theme
(modus-themes-load-themes)

;; Load the theme of your choice:
(modus-themes-load-vivendi)

(when (eq system-type 'darwin)
  (progn
    (setq mac-command-modifier 'meta
	  mac-right-option-modifier 'none
	  mac-option-modifier 'super
	  shell-command-switch "-lc")
    (global-set-key "\M-`" 'other-frame)))

(mapcar #'(lambda (x)
	    (define-key global-map (kbd (car x)) (cdr x)))
	'(("<f11>" . aim/fullscreen)
	  ("<f1>" . gnus-slave)
	  ("<f2>" . aim/revert-buffer-now)
	  ("<f3>" . whitespace-cleanup)
	  ("<f5>" . modus-themes-toggle)
	  ("C-x C" . compile)
	  ("C-x C-g" . goto-line)
	  ("C-x C-j" . dired-jump)
	  ("C-x C-r" . recentf-open-files) ;overrides binding in ffap
	  ("C-x g" . goto-line)
	  ("C-x m" . gnus-msg-mail)
	  ("M-/" . hippie-expand)))
