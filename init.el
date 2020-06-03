;; A big contributor to startup times is garbage collection. We up the
;; gc threshold to temporarily prevent it from running, and then reset
;; it later using a hook.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Keep a ref to the actual file-name-handler
(defvar default-file-name-handler-alist file-name-handler-alist)

;; Set to nil (because regexing is cpu intensive).
(setq file-name-handler-alist nil)

;; Reset file-name-handler-alist after initialization.
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (hrs/reset-font-size)
	    (message "Happiness delivered in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)
	    (setq gc-cons-threshold 100000000 ;100MB
		  gc-cons-percentage 0.1
		  file-name-handler-alist default-file-name-handler-alist)))

(setq straight-use-package-by-default t
  straight-repository-branch "develop"
  ;;straight-check-for-modifications '(watch-files find-when-checking))
  straight-check-for-modifications nil)

(setq-default straight-vc-git-default-clone-depth 1)

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
    (load bootstrap-file nil 'nomessage)))

(aim/straight-bootstrap)

;; Bootstrap use-package
(straight-use-package 'use-package)

(setq use-package-always-defer t)

;;(straight-use-package '(org :type built-in))

(use-package gcmh
  :defer nil
  :ensure t
  :config
  (gcmh-mode 1))

(use-package select
  :straight (:type built-in)
  :defer nil
  :custom
  (x-select-enable-clipboard t)
  (x-select-enable-primary t)
  (save-interprogram-paste-before-kill t))

(use-package cus-edit
  :straight (:type built-in)
  :defer nil
  :custom
  (custom-file null-device "Don't store customizations"))

(use-package uniquify
  :straight (:type built-in)
  :defer nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "|")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(use-package ffap
  :custom
  ;; Don't ping things that look like domain names.
  (ffap-machine-p-known 'reject)
  :config
  (ffap-bindings))

(use-package emacs
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'set-goal-column 'disabled nil)
  :custom
  (auto-compression-mode)
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
  (default-frame-alist '((menu-bar-lines 0)
			 (tool-bar-lines 0)
			 (vertical-scroll-bars)))
  (initial-frame-alist '((vertical-scroll-bars)))
  (inhibit-startup-screen t "Don't show splash screen")
  (use-dialog-box nil "Disable dialog boxes")
  (x-gtk-use-system-tooltips nil)
  (enable-recursive-minibuffers t "Allow minibuffer commands in the minibuffer")
  (debug-on-error nil))

;;; where can I put these? simple?
(defalias 'ttl 'toggle-truncate-lines)
(fset 'yes-or-no-p 'y-or-n-p)

(use-package direnv
  :init
  (add-hook 'prog-mode-hook #'direnv-update-environment)
  :custom
  (direnv-always-show-summary nil)
  :config
  (direnv-mode))

(use-package dired-x
  :straight (:type built-in)
  :defer nil
  :bind
  (("C-x C-j" . dired-jump)))

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
	      ("/" . dired-narrow)))

(use-package hippie-exp
  :straight (:type built-in)
  :defer nil
  :custom
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev
     try-expand-dabbrev-from-kill
     try-expand-dabbrev-all-buffers
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     try-expand-list
     try-expand-line
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol))
  :bind
  (("M-/" . hippie-expand)))

(use-package files
  :straight (:type built-in)
  :defer nil
  :custom
  (require-final-newline t)
  (backup-by-copying t)
  (backup-directory-alist
   `((".*" . ,(locate-user-emacs-file "backups"))))
  (auto-save-file-name-transforms
   `((".*" ,temporary-file-directory t)))
  (delete-old-versions t)
  (kept-new-versions 20)
  (kept-old-versions 10)
  (version-control t))

(use-package executable
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p))

;; https://github.com/hrs/dotfiles/blob/master/emacs/.emacs.d/configuration.org
;; thanks man!

(setq hrs/default-font "DejaVu Sans Mono")
(setq hrs/default-font-size 14)
(setq hrs/current-font-size hrs/default-font-size)
(setq hrs/font-change-increment 1.2)

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

(use-package tramp
  :config
  (put 'temporary-file-directory 'standard-value `(,temporary-file-directory))
  :custom
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-default-method "ssh")
  (tramp-default-proxies-alist nil)
  ;; shell prompt additions for NixOS
  (tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*"))

(use-package magit
  :config
  (global-magit-file-mode -1)
  :custom
  (magit-diff-arguments (quote ("--function-context" "--no-ext-diff" "--stat")))
  :bind
  ("C-c i" . magit-status))

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

(use-package forge
  :after magit)

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

(use-package helm)
(use-package helm-company)
(use-package helm-lsp) ;; Locate symbols using helm-lsp-workspace-symbol

(use-package helm-ls-git
  :ensure t
  :defer nil
  :bind
  (("C-c C-l" . helm-ls-git-ls)))

(use-package protobuf-mode)

(use-package nix-mode
  :custom
  (nix-indent-function #'nix-indent-line)
  :bind
  (:map nix-mode-map
	("C-c C-j" . aj-toggle-fold)))

(use-package deadgrep
  :bind
  (("<f5>" . deadgrep)))

(use-package docker
  :ensure t
  :bind
  (:map mode-specific-map
	("d" . docker)))

;; not sure if these two should be here
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker-compose-mode)

;; (use-package k8s-mode
;;   :after yasnippet
;;   :requires yasnippet
;;   :hook (k8s-mode . yas-minor-mode))

;;(use-package yasnippet)

(use-package kubernetes
  :commands (kubernetes-overview))

;; Making it easier to discover Emacs key presses.
(use-package which-key
  :demand t
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay most-positive-fixnum)
  (setq which-key-idle-secondary-delay 1e-100)
  (which-key-mode +1))

(use-package browse-at-remote)
(use-package browse-url)
(use-package cmake-mode)
(use-package docker-tramp)
(use-package epkg)
(use-package esup)
(use-package hl-line)
(use-package json-mode)
(use-package kubernetes-tramp)
(use-package markdown-mode)
(use-package modus-operandi-theme)
(use-package modus-vivendi-theme)
(use-package no-littering)
(use-package pass)
(use-package restart-emacs)
(use-package terraform-mode)
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
  :config
  (ws-butler-global-mode))

(use-package ibuffer
  :bind
  (:map ibuffer-mode-map
	("SPC" . ibuffer-visit-buffer)))

(use-package ibuffer-vc
  :after (ibuffer vc)
  :bind (:map ibuffer-mode-map
	      ("/ V" . ibuffer-vc-set-filter-groups-by-vc-root)
	      ("/ <deletechar>" . ibuffer-clear-filter-groups)))

(use-package heaven-and-hell
  :ensure t
  :init
  (setq heaven-and-hell-theme-type 'dark) ;; Omit to use light by default
  (setq heaven-and-hell-themes
	'((light . modus-operandi)
	  (dark . modus-vivendi))) ;; Themes can be the list: (dark . (tsdh-dark wombat))
  (setq heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
	 ("<f6>" . heaven-and-hell-toggle-theme)))

(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

(use-package langtool
  :config
  (setq langtool-http-server-host "localhost"
	langtool-http-server-port 8081
	langtool-default-language "en-GB")
  :bind
  (:map git-commit-mode-map
	("C-x `" . langtool-correct-buffer)))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
	 ("M-g j" . dumb-jump-go)
	 ("M-g b" . dumb-jump-back)
	 ("M-g i" . dumb-jump-go-prompt)
	 ("M-g x" . dumb-jump-go-prefer-external)
	 ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq ;;dumb-jump-selector 'helm
   dumb-jump-debug nil
   dumb-jump-prefer-searcher 'rg)
  :hook (prog-mode . dumb-jump-mode))

(defun my-dumb-jump-mode-hook ()
  "Remove keybindings I use elsewhere after dumb-jump has loaded."
  (define-key dumb-jump-mode-map (kbd "C-M-g") nil)
  (define-key dumb-jump-mode-map (kbd "C-M-p") nil)
  (define-key dumb-jump-mode-map (kbd "C-M-q") nil))

;; This is to speedup LSP.
;;
;; Increase the amount of data which Emacs reads from the process.
;; Again the emacs default is too low 4k considering that the some
;; of the language server responses are in 800k - 3M range.
(use-package process
  :straight (:type built-in)
  :custom
  (read-process-output-max (* 1 (* 1024 1024))))

(use-package company
  :after lsp-mode
  :custom
  (company-idle-delay 0)
  (company-tooltip-limit 20)
  (company-minimum-prefix-length 1)
  (company-echo-delay 0)
  (company-require-match nil)
  (company-tooltip-align-annotations t) ; Align annotation to the right side.
  (company-auto-complete nil)
  :hook
  (prog-mode . company-mode)
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
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-select-previous)
     (define-key company-active-map (kbd "<tab>") 'company-select-previous)
     (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
     (define-key company-active-map (kbd "<backtab>") 'company-select-previous)))

;; (use-package company-quickhelp
;;   :custom
;;   (company-quickhelp-delay 3)
;;   (company-quickhelp-mode 1))

(use-package company-shell
  :after company
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env)))

(use-package flycheck)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
	lsp-rust-server 'rust-analyzer
	lsp-prefer-capf t
	lsp-enable-file-watchers nil
	lsp-prefer-flymake nil)
  :hook ((prog-mode . direnv-update-environment)
	 (prog-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration)
	 ;; (before-save . lsp-format-buffer)
	 ;; (before-save . lsp-organize-imports)
	 )
  :config
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.direnv$")
  :bind (("C-c d" . lsp-describe-thing-at-point)
	 ("C-c e n" . flycheck-next-error)
	 ("C-c e p" . flycheck-previous-error)
	 ("C-c e l" . flycheck-list-errors)
	 ("C-c e r" . lsp-find-references)
	 ("C-c e R" . lsp-rename)
	 ("C-c e i" . lsp-find-implementation)
	 ("C-c e t" . lsp-find-type-definition))
  :commands lsp lsp-deferred)

(use-package lsp-treemacs)

(use-package go-mode
  :custom
  (gofmt-command "goimports")
  :bind (:map go-mode-map
	      ("C-c C-n" . go-run)
	      ("C-c ."   . go-test-current-test)
	      ("C-c f"   . go-test-current-file)
	      ("C-c a"   . go-test-current-project))
  :hook
  ((go-mode . lsp-deferred)
   (before-save . gofmt-before-save)))

(eval-after-load "dumb-jump"
  (add-hook 'go-mode 'my-dumb-jump-mode-hook))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  "Fix the slow LSP format buffer."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(remove-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'before-save-hook 'gofmt-before-save)

;;Prefer go-mode's gofmt over LSP sluggishness
(remove-hook 'before-save-hook 'lsp-organize-imports)
(remove-hook 'before-save-hook 'lsp-format-buffer)

(use-package hl-line
  :hook
  (prog-mode . hl-line-mode))

(use-package recentf
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 25)
  :config
  (recentf-mode 1)
  :bind
  (("C-x C-r" . recentf-open-files)))

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

(use-package simple
  :straight (:type built-in)
  :defer nil
  :custom
  (kill-ring-max 30000)
  (truncate-lines t)
  :config
  (column-number-mode 1)
  :bind
  (("C-x C-b" . ibuffer)
   ("C-x C-b" . helm-buffers-list)
   ("C-x C-j" . dired-jump)
   ("C-x m" . gnus-msg-mail)
   ([f1] . gnus-slave)
   ([f2] . aim/revert-buffer-now)
   ([f3] . whitespace-cleanup)
   ([f4] . heaven-and-hell-toggle-theme)
   ("C-x C" . compile)
   ("C-x g" . goto-line)
   ("C-x C-g" . goto-line)
   ([f11] . aim/fullscreen)))

(defun my-minibuffer-setup ()
  "Fix my eyes."
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 1.25))))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

(setq org-confirm-babel-evaluate nil)

(unless (fboundp 'json-serialize)
  (error "**** you don't have a json-serialize built-in function ****"))

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

;; direct copy from vdemeester
(use-package projectile
  :commands
  (projectile-ack
   projectile-ag
   projectile-compile-project
   projectile-dired
   projectile-find-dir
   projectile-find-file
   projectile-find-tag
   projectile-test-project
   projectile-grep
   projectile-invalidate-cache
   projectile-kill-buffers
   projectile-multi-occur
   projectile-project-p
   projectile-project-root
   projectile-recentf
   projectile-regenerate-tags
   projectile-replace
   projectile-replace-regexp
   projectile-run-async-shell-command-in-root
   projectile-run-shell-command-in-root
   projectile-switch-project
   projectile-switch-to-buffer
   projectile-vc)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq-default projectile-completion-system 'default
                ;; Do not track known projects automatically, instead call projectile-add-known-project
                projectile-track-known-projects-automatically nil)
  (projectile-mode)
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)
  (setq
   ;; Custom compilation buffer name function
   compilation-buffer-name-function (lambda (mode) (concat "*" (downcase mode) ": " (projectile-project-name) "*"))
   projectile-find-dir-includes-top-level t
   projectile-switch-project-action #'projectile-commander
   projectile-create-missing-test-files t
   projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
  (def-projectile-commander-method ?s
    "Open a *shell* buffer for the project"
    (projectile-run-eshell))
  (def-projectile-commander-method ?c
    "Run `compile' in the project"
    (projectile-compile-project nil)))

(setq projectile-completion-system 'helm)

(setq projectile-switch-project-action
      (lambda () (projectile-ibuffer nil)))

(use-package helm-projectile
  :config
  (helm-projectile-on))

(setq lsp-ui-peek-fontify 'always)
(setq projectile-switch-project-action 'helm-projectile)
(setq projectile-enable-caching t)
