;; -*- lexical-binding: t; -*-

(when (eq system-type 'darwin)
  (progn
    (setq mac-command-modifier 'meta
          mac-right-option-modifier 'none
          mac-option-modifier 'super
          shell-command-switch "-lc")
    (global-set-key "\M-`" 'other-frame)))

(defvar use-nix-epkgs (or (string= system-name "mba")
                          (string= system-name "x1c")))

(setq use-nix-epkgs t)
(setq debug-on-error nil)

(and (boundp 'use-package-report)
     (use-package-report))

(unless (functionp 'json-serialize)
  (error "**** you don't have a json-serialize built-in function ****"))

(unless (functionp 'module-load)
  (error "**** you don't have modules enabled ****"))

(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines nil)
(setq-default indent-tabs-mode nil)

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-by-copying t
      backup-directory-alist `((".*" . ,(locate-user-emacs-file "backups")))
      comp-deferred-compilation nil
      create-lockfiles nil
      delete-old-versions t
      display-time-24hr-format t
      display-time-default-load-average nil
      enable-recursive-minibuffers t
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      inhibit-compacting-font-caches t
      inhibit-startup-message t
      kept-new-versions 20
      kept-old-versions 10
      mouse-yank-at-point nil
      ns-pop-up-frames nil
      package-enable-at-startup nil
      require-final-newline t
      ring-bell-function #'ignore
      sentence-end-double-space nil
      show-paren-delay 0
      ;;site-run-file nil
      truncate-lines t
      use-dialog-box nil
      vc-follow-symlinks t
      version-control t
      visible-bell nil
      visible-cursor nil
      window-resize-pixelwise t)

(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t)

(setq uniquify-buffer-name-style 'reverse
      uniquify-separator "|"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

(setq gc-cons-threshold (* 100 1048576))

(setq use-package-always-defer nil
      use-package-always-ensure t
      use-package-ignore-unknown-keywords t
      use-package-verbose nil
      use-package-compute-statistics t)

(setq straight-use-package-by-default t
      straight-repository-branch "develop"
      straight-check-for-modifications nil
      straight-disable-native-compile t)

(customize-set-variable 'kill-ring-max 30000)

(auto-compression-mode t)
(blink-cursor-mode -1)
(column-number-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode)                ; Show current line
(menu-bar-mode (not (eq system-type 'darwin))) ; Disable the menu bar
(put 'narrow-to-region 'disabled nil)
(scroll-bar-mode -1)                 ; Disable scrollbar
(set-fringe-mode 4)                  ; Give us some space
(show-paren-mode -1)                 ; Enable show paren matching mode
(tool-bar-mode -1)                   ; Disable toolbar
(tooltip-mode -1)                    ; Disable tooltip

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

(defun aim/minibuffer-setup ()
  "Stop squinting."
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 1.25))))

(defun aim/straight-bootstrap nil
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

(if use-nix-epkgs
    (require 'use-package)
  (progn
    (setq-default straight-vc-git-default-clone-depth 1)
    (aim/straight-bootstrap)))

(setq warning-suppress-log-types '((comp) (use-package)))

;;; PACKAGES

(use-package gcmh
  :demand t
  :straight (:type built-in)
  :custom (gcmh-verbose t)
  :config
  (gcmh-mode))

(use-package desktop
  :demand t
  :custom ((desktop-restore-eager 8)
           (desktop-globals-to-save nil)
           (desktop-files-not-to-save
            (rx (or (seq bol "/" (zero-or-more (not (any "/" ":"))) ":")
                    (seq "(ftp)" eol)
                    (seq "*" (one-or-more not-newline) "*")))))
  :config
  (desktop-save-mode t))

(use-package savehist
  :demand t
  :custom ((history-delete-duplicates t)
           (savehist-save-minibuffer-history t)
           (savehist-additional-variables '(kill-ring
                                            compile-command
                                            search-ring))
           (savehist-ignored-variables '(yes-or-no-p-history)))
  :config
  (savehist-mode t))

(use-package saveplace
  :demand t
  :config
  (save-place-mode t))

(use-package hrs
  :ensure nil
  :straight (:type built-in)
  :load-path (lambda () (expand-file-name "hrs" user-emacs-directory))
  :commands (hrs/reset-font-size
             hrs/increase-font-size
             hrs/default-font-size)
  :bind (("C-)" . hrs/reset-font-size)
         ("C-+" . hrs/increase-font-size)
         ("C--" . hrs/decrease-font-size)))

(use-package modus-themes
  :ensure nil
  :defer nil
  :straight (:type built-in)
  ;;:load-path (lambda () (expand-file-name "modus-themes" user-emacs-directory))
  :commands (modus-themes-load-themes
             modus-themes-load-operandi
             modus-themes-load-vivendi
             modus-themes-toggle)
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))
  (modus-themes-load-themes)
  (modus-themes-load-vivendi)
  :bind (("<f5>" . modus-themes-toggle)))

(use-package pinentry
  :defer nil
  :commands (pinentry-start)
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
  :config (global-clipetty-mode))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure t
  :config
  (dolist (var '("GPG_AGENT_INFO"
                 "GNUPGHOME"
                 "LANG"
                 "LC_CTYPE"
                 "NIX_PATH"
                 "NIX_SSL_CERT_FILE"
                 "NO_COLOR"
                 "PASSWORD_STORE_DIR"
                 "SSH_AGENT_PID"
                 "SSH_AUTH_SOCK"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

(use-package avy
  ;; avy gives us fluent jump-to-line commands mapped to the home row.
  :bind ("C-c l" . avy-goto-line))

(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  :custom (which-key-idle-delay 1.2))

(use-package ffap
  :ensure nil
  :defer nil
  :config
  (setq ffap-machine-p-known 'reject)
  (ffap-bindings))

(defalias 'ttl 'toggle-truncate-lines)

(use-package dired-x
  :straight (:type built-in)
  :ensure nil
  :commands (dired-jump)
  :bind ("C-x C-j" . dired-jump))

(use-package dired-narrow
  :init
  (setq dired-use-ls-dired nil)
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package hippie-exp
  :straight (:type built-in)
  :defer nil
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-from-kill
          try-expand-dabbrev-all-buffers
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(use-package ag
  :custom
  (ag-highligh-search t)
  (ag-reuse-buffers t)
  (ag-reuse-window t)
  :bind ("M-s a" . ag-project))

(use-package wgrep
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(use-package wgrep-ag
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

(use-package tramp
  ;; Using the built-in version avoids; Symbol's function definition is void: "tramp-register-crypt-file-name-handler
  :straight (:type built-in)
  :ensure nil
  :config
  (put 'temporary-file-directory 'standard-value `(,temporary-file-directory))
  :custom
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-default-method "ssh")
  (tramp-default-proxies-alist nil)
  ;; shell prompt additions for NixOS
  (tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*"))

(use-package magit
  ;; :config
  ;; (global-magit-file-mode -1)
  ;; Removed by https://github.com/magit/magit/pull/4237
  :custom
  (magit-diff-arguments (quote ("--function-context" "--no-ext-diff" "--stat")))
  :bind
  (("C-c i" . magit-status)
   ("C-c I" . magit-dispatch)))

(use-package git-commit
  :hook (git-commit-setup . git-commit-turn-on-flyspell))

(use-package git-timemachine)

(use-package git-gutter
  :config
  (setq git-gutter:modified-sign " "
        git-gutter:added-sign " "
        git-gutter:deleted-sign " "
        git-gutter:lighter " GG")
  (set-face-background 'git-gutter:modified "systemOrangeColor")
  (set-face-foreground 'git-gutter:added "systemGreenColor")
  (set-face-foreground 'git-gutter:deleted "systemRedColor")
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

(use-package nixpkgs-fmt
  :demand t
  :custom
  (nixpkgs-fmt-command "nixfmt"))

(use-package nix-mode
  :mode "\\.nix\\'"
  :custom
  (nix-indent-function #'nix-indent-line)
  ;;:hook 'nix-mode #'nixpkgs-fmt-on-save-mode
  :bind (:map nix-mode-map
              ("C-c C-j" . aj-toggle-fold)))

;; not sure if these two should be here
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker-compose-mode)

(use-package notmuch
  :defer 5
  :init
  (setq notmuch-search-oldest-first nil
        mail-user-agent 'message-user-agent
        notmuch-wash-wrap-lines-length 80
        notmuch-tree-show-out t)
  :config
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

(use-package company
  :commands (company-select-next-or-abort
             company-select-previous-or-abort)
  :custom ((company-idle-delay 0)
           (company-tooltip-limit 20)
           (company-minimum-prefix-length 3)
           (company-echo-delay 0)
           (company-require-match nil)
           (company-tooltip-align-annotations t) ; Align annotation to the right side.
           (company-auto-complete nil))
  :bind (:map company-active-map
              ("C-n" . company-select-next-or-abort)
              ("C-p" . company-select-previous-or-abort))
  :hook (after-init . global-company-mode))

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
  (lsp lsp-deferred ls-rename lsp-find-references lsp-find-implementation lsp-find-type-definition))

(use-package gotest)

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
  :commands (go-run
             go-coverage
             go-test-current-test
             go-test-current-file
             go-test-current-project)
  :hook ((go-mode . lsp-deferred)))

(use-package go-add-tags)

(require 'hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

(use-package recentf
  :commands (recentf-open-files)
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 32
        recentf-max-saved-items 32)
  :bind ("C-x C-r" . recentf-open-files))

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

(use-package atomic-chrome
  :defer nil
  :commands (atomic-chrome-start-server)
  :config
  (atomic-chrome-start-server))

(use-package vterm
  :custom
  (setq vterm-ignore-blink-cursor t))

;;; Copied from somebody - can't recall whom, but thanks!
(defun my-vterm/split-horizontal ()
  "Create a new vterm window under of the current one."
  (interactive)
  (let* ((ignore-window-parameters t)
         (dedicated-p (window-dedicated-p)))
    (split-window-vertically)
    (other-window 1)
    (vterm default-directory)))

(use-package helm
  :commands (helm-buffers-list helm-mini)
  :config
  (require 'helm-config)
  (helm-mode -1)
  :init
  (setq helm-imenu-fuzzy-match t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-buffers-fuzzy-matching t)
  :bind (("C-c h d" . helm-browse-project)
         ("C-c h i" . helm-semantic-or-imenu)
         ("C-c h o" . helm-occur)
         ("C-c h p" . helm-projects-history)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-mini)
         ("M-y" . helm-show-kill-ring)))

(use-package helm-ls-git
  :commands (helm-ls-git)
  :bind
  (("C-c C-l" . helm-ls-git)))

(use-package whitespace
  :ensure nil
  :straight (:type built-in)
  :defer nil
  :commands (whitespace-cleanup)
  :bind ("<f3>" . whitespace-cleanup)
  :hook (before-save . whitespace-cleanup))

(use-package ws-butler
  :config
  (ws-butler-global-mode))

(use-package executable
  :ensure nil
  :straight (:type built-in)
  :defer nil
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package swift-mode)

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "Xcrun --find sourcekit-lsp"))))

;;; Require confirmation before interactively evaluating code blocks
;;; in Org buffers. The default value of this variable is t, meaning
;;; confirmation is required for any code block evaluation.
(setq org-confirm-babel-evaluate nil)

;; The buffer *Flymake log* tends to fill up with things like:
;; > Warning [flymake init.el]: Disabling backend flymake-proc-legacy-flymake
;; > because (error Can’t find a suitable init function)
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

;; https://github.com/emacs-lsp/lsp-mode/issues/631#issuecomment-457866187
(add-hook 'c++-mode-hook
          (lambda ()
            (setq flymake-diagnostic-functions (list 'lsp--flymake-backend))))

(add-hook 'minibuffer-setup-hook 'aim/minibuffer-setup)

(use-package direnv
  :if (executable-find "direnv")
  ;; :init
  ;; (add-hook 'prog-mode-hook #'direnv-update-environment)
  :custom
  (direnv-always-show-summary nil)
  :config
  (direnv-mode))

(when (boundp 'read-process-output-max)
  ;; This is to speedup LSP. Increase the amount of data which Emacs
  ;; reads from the process. Again the emacs default is too low 4k
  ;; considering that the some of the language server responses are in
  ;; 800k - 3M range.
  (setq-local read-process-output-max (* 4 (* 1024 1024))))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-select-previous)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Happiness delivered in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(mapcar #'(lambda (x)
            (define-key global-map (kbd (car x)) (cdr x)))
        '(("<f11>" . aim/fullscreen)
          ("<f1>" . gnus-slave)
          ("<f2>" . aim/revert-buffer-now)
          ("C-x C" . compile)
          ("C-x C-g" . goto-line)
          ("C-x C-r" . recentf-open-files) ;overrides binding in ffap
          ("C-x g" . goto-line)
          ("C-x m" . gnus-msg-mail)))
(require 'color)

;; https://ruzkuku.com/emacs.d.html#org08dc33e
(defun zge/reverse-face (face &optional frame)
  (interactive (list (read-face-name "Reverse face" (face-at-point t))))
  (let* ((fg (face-attribute face :foreground frame))
         (bg (face-attribute face :background frame)))
    (set-face-attribute
     face frame
     :foreground
     (color-complement-hex
      (if (eq fg 'unspecified)
          (face-attribute 'default :foreground frame)
        fg))
     :background
     (color-complement-hex
      (if (eq bg 'unspecified)
          (face-attribute 'default :background frame)
        bg))))
  face)

(defun zge/toggle-dark-mode ()
  (interactive)
  (dolist (face '(mode-line default))
    (zge/reverse-face face)))

(global-set-key (kbd "M-i") 'imenu)

(setq completion-styles `(basic partial-completion emacs22
                                initials ,(if (version<= emacs-version "27.0") 'helm-flex 'flex)))
