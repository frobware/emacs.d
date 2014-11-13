(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ae8d0f1f36460f3705b583970188e4fbb145805b7accce0adb41031d99bd2580" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "90e0447c82dd161963f5d22408ad6b088b1bf613839a95080c232f9d5dfe4c6a" "8f7e1668dd3a097964e6016c26d36822ab2e48fc3e9a3a2e2634224a5ca728c8" default)))
 '(jira-url "https://cards.linaro.org/rpc/xmlrpc")
 '(ns-command-modifier (quote meta)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "yellow"))))
 '(font-lock-keyword-face ((t (:foreground "white" :weight bold))))
 '(fringe ((t (:background "grey10"))))
 '(highlight ((t (:background "grey10"))))
 '(hl-line ((t (:inherit highlight))) t)
 '(isearch-fail ((((class color)) (:background "red"))))
 '(mode-line ((t (:background "grey15" :foreground "green" :box nil)))))

(and (string-equal "darwin" system-type)
     (progn
       (set-default-font "-*-Source Code Pro-light-normal-normal-*-18-*-*-*-m-0-iso10646-1" nil nil)
       (menu-bar-mode)))

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
(setq inhibit-startup-echo-area-message t)
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
      '(("melpa" . "http://melpa.milkbox.net/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(use-package base16-theme
  :defer t)

(use-package dired-x
  :init (global-set-key (kbd "C-x C-j") 'dired-jump))

(use-package cmake-mode
  :defer t
  :mode ("\\.cmake$" . cmake-mode)
  :ensure t)

;; (use-package exec-path-from-shell
;;   :ensure t
;;   :defer nil
;;   :init
;;   (progn
;;     (dolist (var '("GOPATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
;;       (add-to-list 'exec-path-from-shell-variables var)))
;;   :config
;;   (exec-path-from-shell-initialize))

(use-package ag
  :ensure t
  :commands (ag ag-files ag-regexp ag-project ag-project-files ag-project-regexp)
  :config
  (progn
    (setq ag-highlight-search t
	  ag-reuse-buffers t)))

(use-package magit
  :bind ("C-c i" . magit-status)
  :commands magit-status
  :ensure t)

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

;; (use-package company
;;   :ensure company)

;; (use-package company-go
;;   :ensure company-go
;;   :init (add-to-list 'company-backends 'company-go))

(use-package cmake-mode
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)))

(use-package yaml-mode
  :ensure yaml-mode
  :mode "\\.ya?ml\\'")

;; (use-package flycheck
;;   :defer t
;;   :config
;;   (progn
;;     (set-face-underline 'flycheck-error nil)
;;     (set-face-background 'flycheck-error nil)
;;     (set-face-underline 'flycheck-warning nil)))

(use-package browse-url
  :ensure t)

(use-package company
  :ensure company)

;;; go

(use-package go-mode
  :ensure go-mode
  :mode "\\.go\\'"
  :commands (godoc gofmt gofmt-before-save go-remove-unused-imports)
  :init
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)
    (add-hook 'go-mode-hook 'company-mode))
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

(use-package go-eldoc
  :ensure go-eldoc
  :commands go-eldoc-setup
  :init (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package golint
  :ensure golint)

(use-package company-go
  :ensure company-go
  :init (add-to-list 'company-backends 'company-go))

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
    (global-set-key (kbd "C-x C-b") 'electric-buffer-list)))

(aim/add-to-load-path "lisp")

(require 'aim-functions)
(require 'aim-global-keybindings)

(use-package iswitchb
  :init
  (iswitchb-mode 1))

(use-package server
  :init
  (unless (server-running-p)
    (server-start)))

(use-package uniquify
  :init
  (progn
    (setq uniquify-buffer-name-style 'reverse)
    (setq uniquify-separator "|")
    (setq uniquify-after-kill-buffer-p t)
    (setq uniquify-ignore-buffers-re "^\\*")))

(use-package dockerfile-mode
  :ensure t)

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

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
	    (null (do-yas-expand)))
	(if (check-expansion)
	    (company-complete-common)
	  (indent-for-tab-command)))))

(setq company-idle-delay 0.3)
(setq company-tooltip-limit 20)
(setq company-minimum-prefix-length 2)
(setq company-echo-delay 0)
(setq company-auto-complete nil)

(add-hook 'lisp-mode #'(complete-mode 1))

(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
    (indent-according-to-mode)))

(defun complete-or-indent ()
  (interactive)
  (if (company-manual-begin)
      (company-complete-common)
    (indent-according-to-mode)))

(defadvice kill-line (before check-position activate)
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
	     (just-one-space 0)
	     (backward-char 1))))

(electric-indent-mode 1)

(use-package lisp-mode
  :config
  (progn
    (bind-key "M-/" 'company-complete emacs-lisp-mode-map)
    (add-hook 'emacs-lisp-mode-hook 'company-mode t)))

(use-package ffap
  :config (ffap-bindings))

(setq exec-path-from-shell-debug t)

(use-package exec-path-from-shell
  :ensure t
  :init
  (progn
    (dolist (var '("GOPATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
      (add-to-list 'exec-path-from-shell-variables var)))
  :idle (exec-path-from-shell-initialize))

;; (use-package exec-path-from-shell
;;   :ensure t
;;   :defer nil
;;   :init
;;   (progn
;;     (dolist (var '("GOPATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
;;       (add-to-list 'exec-path-from-shell-variables var)))
;;   :idle
;;   (exec-path-from-shell-initialize))

(defun aim/run-go-buffer ()
  (interactive)
  (shell-command (format "go run %s" (buffer-file-name (current-buffer)))))

(use-package git-gutter-fringe
  :ensure t)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

(use-package markdown-mode
  :ensure markdown-mode)

(use-package ace-jump-mode
  :ensure ace-jump-mode
  :bind ("C-x SPC" . ace-jump-mode))

;; The platinum searcher
(use-package pt
  :ensure pt)

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
  (occur "^func [A-Z]"))

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
  (with-output-to-temp-buffer "*sj*"
    (shell-command (format "sj --show-job %s" (buffer-file-name (current-buffer))) "*sj*")
    (save-excursion
      (pop-to-buffer "*sj*")
      (lava-mode-submit-job nil))))
