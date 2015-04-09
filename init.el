(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("9b402e9e8f62024b2e7f516465b63a4927028a7055392290600b776e4a5b9905" "56cb99174fad69feba8edd6663c592e77d77332fb4c4bb5be058ef459a426463" "a0fdc9976885513b03b000b57ddde04621d94c3a08f3042d1f6e2dbc336d25c7" "c56d90af9285708041991bbc780a073182cbe91721de17c6e7a8aac1380336b3" "968c8cf5763708bb86a3f82bb0f8b8d2fe885e693ac8644268738ac2584da292" "cbef37d6304f12fb789f5d80c2b75ea01465e41073c30341dc84c6c0d1eb611d" "ba9be9caf9aa91eb34cf11ad9e8c61e54db68d2d474f99a52ba7e87097fa27f5" "5141eecb3e1ddd520e931cc1b30549490e4f8ebd49e8ea55ae528efa9857ffe6" "405fda54905200f202dd2e6ccbf94c1b7cc1312671894bc8eca7e6ec9e8a41a2" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "ae8d0f1f36460f3705b583970188e4fbb145805b7accce0adb41031d99bd2580" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "90e0447c82dd161963f5d22408ad6b088b1bf613839a95080c232f9d5dfe4c6a" "8f7e1668dd3a097964e6016c26d36822ab2e48fc3e9a3a2e2634224a5ca728c8" default)))
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
 '(linum ((t (:foreground "#656868" :background "black"))))
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
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
	("melpa" . "http://melpa.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(use-package base16-theme
  :defer t)

(use-package dired-x
  :init (progn
	  (global-set-key (kbd "C-x C-j") 'dired-jump)
	  (setq-default dired-omit-mode t)))

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

(use-package magit-gerrit
  :ensure t
  :config
  (progn
    (setq-default magit-gerrit-remote "gerrit")))

;;; An example of making magit-gerrit work.

;; ((magit-mode
;;   (magit-gerrit-remote . "ssh://frobware@review.openstack.org:29418/stackforge/rally.git"))
;;  ("indent-tabs-mode"
;;   (indent-tabs-mode)))

(use-package magit
  :bind ("C-c i" . magit-status)
  :commands magit-status
  :ensure t
  :config
  (progn
    (setq magit-auto-revert-mode nil)))

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

(and (file-exists-p "/usr/local/go1.4.2/misc/oracle.el")
     (load-file "/usr/local/go1.4.2/misc/oracle.el"))

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
    ;;(global-set-key (kbd "C-x C-b") 'ibuffer)))

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

;; (defun tab-indent-or-complete ()
;;   (interactive)
;;   (if (minibufferp)
;;       (minibuffer-complete)
;;     (if (or (not yas/minor-mode)
;; 	    (null (do-yas-expand)))
;; 	(if (check-expansion)
;; 	    (company-complete-common)
;; 	  (indent-for-tab-command)))))

(setq company-idle-delay 0.3)
(setq company-tooltip-limit 20)
(setq company-minimum-prefix-length 2)
(setq company-echo-delay 0)
(setq company-auto-complete nil)

(add-hook 'lisp-mode #'(complete-mode 1))

;; (defun indent-or-complete ()
;;   (interactive)
;;   (if (looking-at "\\_>")
;;       (company-complete-common)
;;     (indent-according-to-mode)))

;; (defun complete-or-indent ()
;;   (interactive)
;;   (if (company-manual-begin)
;;       (company-complete-common)
;;     (indent-according-to-mode)))

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

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

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

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
	      vc-ignore-dir-regexp
	      tramp-file-name-regexp))

(use-package tramp
  :defer nil
  :config
  (progn
    (set-default 'tramp-default-method "ssh")
    (set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
    (setq tramp-ssh-controlmaster-options
	  (concat
	   "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
	   "-o ControlMaster=auto -o ControlPersist=yes"))))

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

(use-package badger-theme
  :ensure t
  :config
  (progn
    (load-theme 'badger t)
    (set-face-attribute 'region nil :background "#555" :foreground "#ffffff")))

(put 'scroll-left 'disabled nil)

(load-library "python")

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist)
      python-mode-hook '(lambda () (progn
				     (set-variable 'py-indent-offset 4)
				     (set-variable 'indent-tabs-mode nil))))

;; (eval-after-load 'company
;;   (progn
;;     (define-key company-active-map (kbd "TAB") nil)
;;     (define-key company-active-map [tab] nil)))

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
			      (company-mode)
			      (flycheck-mode))))
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

(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

(setq twittering-icon-mode t)

;;; IRC

(setq rcirc-server-alist
      '(("ircproxy.linaro.org" :port 6667 :encryption tls
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
