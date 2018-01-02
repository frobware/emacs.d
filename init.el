(require 'package)

(setq package-enable-at-startup nil)

(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
	("melpa" . "http://melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")
	("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)

;; Mitigate Bug#28350 (security) in Emacs 25.2 and earlier.
;; http://seclists.org/oss-sec/2017/q3/422
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments (quote ("--smart-case" "--stats" "--follow")))
 '(c-default-style
   (quote
    ((c-mode . "linux")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(custom-safe-themes
   (quote
    ("cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "cab317d0125d7aab145bc7ee03a1e16804d5abdfa2aa8738198ac30dc5f7b569" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "4cbec5d41c8ca9742e7c31cc13d8d4d5a18bd3a0961c18eb56d69972bbcf3071" "6952b5d43bbd4f1c6727ff61bc9bf5677d385e101433b78ada9c3f0e3787af06" "b9cbfb43711effa2e0a7fbc99d5e7522d8d8c1c151a3194a4b176ec17c9a8215" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "ed317c0a3387be628a48c4bbdb316b4fa645a414838149069210b66dd521733f" "5acb6002127f5d212e2d31ba2ab5503df9cd1baa1200fbb5f57cc49f6da3056d" "5cd0afd0ca01648e1fff95a7a7f8abec925bd654915153fb39ee8e72a8b56a1f" "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "90e0447c82dd161963f5d22408ad6b088b1bf613839a95080c232f9d5dfe4c6a" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(elide-head-headers-to-hide
   (quote
    (("is free software[:;] you can redistribute it" . "\\(Boston, MA 0211\\(1-1307\\|0-1301\\), USA\\|If not, see <http://www\\.gnu\\.org/licenses/>\\)\\.")
     ("The Regents of the University of California\\.  All rights reserved\\." . "SUCH DAMAGE\\.")
     ("Permission is hereby granted, free of charge" . "authorization from the X Consortium\\.")
     ("Copyright .* The Kubernetes Authors." . "limitations under the License."))))
 '(frame-background-mode (quote dark))
 '(helm-gtags-prefix-key "g")
 '(helm-gtags-suggested-key-mapping t)
 '(helm-locate-project-list (quote ("~/frobware/meerkat" "~/linux-4.11")))
 '(ns-command-modifier (quote meta))
 '(package-selected-packages
   (quote
    (magithub go-stacktracer golint irony rtags fringe-helper git-gutter company magit go-projectile terraform-mode direnv w3m gist pass kubernetes-overview helm-ls-git yaml-mode wgrep-ag vcl-mode use-package smex racer python-mode protobuf-mode peep-dired markdown-mode magit-gh-pulls itail helm-rtags helm-gtags guide-key google-c-style godoctor go-guru go-eldoc go-dlv git-gutter-fringe dockerfile-mode company-irony company-go cmake-mode cmake-ide clang-format cargo ag))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "grey30")))))

;; Perls of wisdom:
;;   http://emacshorrors.com/posts/come-in-and-find-out.html
(unless (window-system)
  (or frame-background-mode
      (setq frame-background-mode 'dark)))

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

;; (setq package-archives
;;       '(("melpa-stable" . "https://stable.melpa.org/packages/")
;; 	("melpa" . "http://melpa.org/packages/")
;; 	("org" . "http://orgmode.org/elpa/")
;; 	("gnu" . "http://elpa.gnu.org/packages/")))

;; (package-initialize)

;; (when (not package-archive-contents)
;;   (package-refresh-contents))

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

(require 'use-package)
(require 'bind-key)

(use-package cc-mode
  :ensure t
  :bind ("C-M-m" . cmake-ide-compile))

(use-package dired-x
  :config
  (progn
    (global-set-key (kbd "C-x C-j") 'dired-jump)
    (add-to-list 'dired-omit-extensions ".cmd")
    (setq-default dired-omit-mode t)))

(use-package cmake-mode
  :mode ("\\.cmake$" . cmake-mode)
  :ensure t)

(use-package ag
  :ensure t
  :config
  (progn
    (setq ag-highlight-search t
	  ag-reuse-buffers t)
    (add-hook 'ag-mode-hook 'wgrep-ag-setup)))

(use-package wgrep-ag
  :config
  (progn
    (setq wgrep-auto-save-buffer t))
  :ensure t)

(use-package wgrep
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

;;'(magit-diff-hunk-heading-highlight ((t (:background "grey30" :foreground "grey90")))))

(use-package magithub
  :after magit)

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
  :ensure company
  :config
  (setq company-idle-delay 0.3
	company-tooltip-limit 20
	company-minimum-prefix-length 2
	company-echo-delay 0
	company-auto-complete nil))

(use-package go-eldoc
  :ensure go-eldoc
  :commands go-eldoc-setup
  :init (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package golint
  :ensure golint)

;; (use-package flycheck
;;   :ensure t
;;   :config
;;   (progn
;;     (setq flycheck-highlighting-mode 'lines)
;;     (set-face-underline 'flycheck-error nil)
;;     (set-face-background 'flycheck-error nil)
;;     (set-face-underline 'flycheck-warning nil)))

(use-package ibuffer
  :config
  (progn
    (setq ibuffer-saved-filter-groups
	  (quote (("default"
		   ("dired" (mode . dired-mode))
		   ("perl" (mode . cperl-mode))
		   ("Go" (mode . go-mode))
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
    (bind-key "[::space::]" 'ibuffer-visit-buffer ibuffer-mode-map)))

(aim/add-to-load-path "lisp")

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

;; (use-package ace-jump-mode
;;   :ensure ace-jump-mode
;;   :bind ("C-c a SPC" . ace-jump-mode))

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
  :ensure t
  :init (progn
	  (set-variable 'py-indent-offset 4)
	  (set-variable 'indent-tabs-mode nil)))

(use-package company-go
  :ensure company-go
  :init (add-to-list 'company-backends 'company-go))

(use-package godoctor
  :ensure t)

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
    (use-package godoctor)
    ;;(bind-key "C-c C-P" 'aim/occur-go-public-functions)
    (bind-key "C-c C-f" 'gofmt go-mode-map)
    (bind-key "C-c C-g" 'go-goto-imports go-mode-map)
    (bind-key "C-c C-k" 'godoc go-mode-map)
    (bind-key "C-c C-r" 'go-remove-unused-imports go-mode-map)
    (bind-key "C-M-x" 'aim/run-go-buffer go-mode-map)
    (bind-key "M-." 'godef-jump go-mode-map)
    (bind-key "<tab>" 'company-complete go-mode-map)
    (bind-key "C-c C-r" 'go-remove-unused-imports go-mode-map)))

(use-package go-dlv
  :ensure t)

(use-package go-guru
  :ensure t)

(use-package itail
  :ensure t)

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

(use-package guide-key
  :ensure t
  :config (setq guide-key/guide-key-sequence '("C-c p" "C-x 4")))

(use-package aim-functions
  :load-path "lisp/")

(use-package aim-global-keybindings
  :load-path "lisp/")

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
  (set-face-background 'lazy-highlight "orange"))

(eval-after-load "isearch"
  `(isearch-face-settings))

(defun aim/occur-go-public-functions ()
  (interactive)
  (occur "^func [A-Z]"))		;which is clearly broken

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
	      vc-ignore-dir-regexp
	      tramp-file-name-regexp))

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

(require 'ansi-color)
(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))

(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

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

(defun aim/desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (unless (desktop-save-mode-off)
    (if (eq (desktop-owner) (emacs-pid))
	(desktop-save desktop-dirname))))

(add-hook 'auto-save-hook 'aim/desktop-save)

;; (require 'recentf)
;; (recentf-mode 1)

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

;; (use-package dumb-jump
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "M-.") 'dumb-jump-go))

(defun get-frame-name (&optional frame)
  (interactive)
  "Return the string that names FRAME (a frame).  Default is selected frame."
  (unless frame (setq frame (selected-frame)))
  (if (framep frame)
      (cdr (assq 'name (frame-parameters frame)))
    (error "Function `get-frame-name': Argument not a frame: `%s'" frame)))

(defun visit-dir-as-root (hostname)
  (interactive "shost: ")
  (let ((filename (format "/ssh:%s|sudo:%s:/" hostname hostname)))
    (find-file filename)))

(defun visit-dir (hostname)
  (interactive "shost: ")
  (let ((filename (format "/ssh:%s:~" hostname hostname)))
    (find-file filename)))

;; (unless (window-system)
;;   ;; use xclip to copy/paste in emacs-nox
;;   (when (getenv "DISPLAY")
;;     (defun xclip-cut-function (text &optional push)
;;       (with-temp-buffer
;;	(insert text)
;;	(call-process-region (point-min) (point-max) "xclip" nil 0 nil "-i" "-selection" "clipboard")))
;;     (defun xclip-paste-function()
;;       (let ((xclip-output (shell-command-to-string "xclip -o -selection clipboard")))
;;	(unless (string= (car kill-ring) xclip-output)
;;	  xclip-output )))
;;     (setq interprogram-cut-function 'xclip-cut-function)
;;     (setq interprogram-paste-function 'xclip-paste-function))
;;   (defun terminal-init-screen ()
;;     "Terminal initialization function for screen."
;;     ;; Use the xterm color initialization code.
;;     (load "term/xterm")
;;     (xterm-register-default-colors)
;;     (tty-set-up-initial-frame-faces))
;;   ;; Set color-theme if running in X or a high-color terminal
;;   (defun setup-color-theme-p ()
;;     "Returns true if it looks like the display can handle 24-bit colors"
;;     (or (display-graphic-p)
;;	(< 256 (display-color-cells))
;;	(getenv "KONSOLE_DBUS_SESSION")))
;;   (defun setup-color-theme ()
;;     "Set up my color theme"
;;     (when (setup-color-theme-p)
;;       (set-face-attribute 'default nil :background "#000000")))
;;   (terminal-init-screen)
;;   (add-hook 'window-setup-hook 'setup-color-theme)
;;   ;; xterm mouse support
;;   (require 'mouse)
;;   (xterm-mouse-mode t))

(use-package server
  :ensure t
  :config
  (unless (server-running-p)
    (server-start)))

(if (and (display-graphic-p) aim/is-linux)
    (set-face-background 'cursor "yellow")
  (shell-command (format "echo -ne '\\033]12;#00ff00\\007' > /proc/%d/fd/1" (emacs-pid))))

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green3")
     (set-face-foreground 'diff-removed "red3")))

(use-package go-guru
  :ensure t)

(use-package clang-format
  :ensure t)

(use-package google-c-style
  :ensure t
  :config
  (c-add-style "WebKit" '("Google"
			  (c-basic-offset . 4)
			  (c-offsets-alist . ((innamespace . 0)
					      (access-label . -)
					      (case-label . 0)
					      (member-init-intro . +)
					      (topmost-intro . 0)
					      (arglist-cont-nonempty . +))))))


(defalias 'ttl 'toggle-truncate-lines)

(use-package vcl-mode
  :ensure t)

(use-package cargo
  :ensure t)

(use-package racer
  :ensure t
  :config
  (progn
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)))

(use-package rust-mode
  :config (progn
	    (setq rust-format-on-save t)
	    (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))
  :ensure t)

(setq company-tooltip-align-annotations t)

(use-package rtags
  :ensure t
  :config
  (progn
    (setq rtags-completions-enabled t)
    (rtags-enable-standard-keybindings)
    (add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)))

(define-key c-mode-base-map (kbd "M-.")
  (function rtags-find-symbol-at-point))

;; (define-key c-mode-base-map (kbd "M-,")
;;   (function rtags-find-references-at-point))
;; See https://github.com/Andersbakken/rtags/issues/832))

;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony))

(use-package helm-rtags
  :ensure t)

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :ensure t
  :config
  (progn
    (define-key c-mode-base-map (kbd "M-RET") 'company-irony)
    (add-hook 'c-mode-common-hook 'company-mode)))

;; (use-package projectile
;;   :ensure t
;;   :config
;;   (projectile-global-mode))

(unless (fboundp 'xref-push-marker-stack)
  (defalias 'xref-pop-marker-stack 'pop-tag-mark)

  (defun xref-push-marker-stack (&optional m)
    "Add point to the marker stack."
    (ring-insert find-tag-marker-ring (or m (point-marker)))))

(define-key c-mode-base-map (kbd "M-,")
  (function rtags-location-stack-back))

(use-package cmake-ide
  :ensure t
  :config
  (cmake-ide-setup))

(use-package helm-gtags
  :ensure t
  :init
  (progn
    (setq helm-gtags-ignore-case t
	  helm-gtags-auto-update t
	  helm-gtags-use-input-at-cursor t
	  helm-gtags-pulse-at-cursor t
	  helm-gtags-prefix-key "\C-cg"
	  helm-gtags-suggested-key-mapping t)
    (add-hook 'dired-mode-hook 'helm-gtags-mode)
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'java-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)
    (with-eval-after-load 'helm-gtags
      (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
      (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
      (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
      (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
      (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
      (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))))

(defadvice gdb-inferior-filter
    (around gdb-inferior-filter-without-stealing)
  (with-current-buffer (gdb-get-buffer-create 'gdb-inferior-io)
    (comint-output-filter proc string)))

(ad-activate 'gdb-inferior-filter)

(use-package elide-head
  :config
  (add-hook 'go-mode-hook 'elide-head)
  (add-hook 'c-mode-common-hook 'elide-head))

(use-package helm
  :ensure t)

(use-package helm-ls-git
  :ensure t
  :config
  (global-set-key (kbd "C-c C-l") 'helm-ls-git-ls))

(use-package protobuf-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package pass
  :ensure t)

(use-package gist
  :ensure t)

(use-package go-projectile
  :ensure t)

(use-package go-stacktracer)

(use-package direnv
  :ensure t
  :config
  (progn
    (direnv-mode)
    (setq direnv-always-show-summary nil)))

;;(add-hook 'after-init-hook 'global-company-mode)

(message "Done")
;;; Taken from:
;;; https://stackoverflow.com/questions/3139970/open-a-file-at-line-with-filenameline-syntax
;;;
;; Open files and goto lines like we see from g++ etc. i.e. file:line#
;; (to-do "make `find-file-line-number' work for emacsclient as well")
;; (to-do "make `find-file-line-number' check if the file exists")
(defadvice find-file (around find-file-line-number
                             (filename &optional wildcards)
                             activate)
  "Turn files like file.cpp:14 into file.cpp and going to the 14-th line."
  (save-match-data
    (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
           (line-number (and matched
                             (match-string 2 filename)
                             (string-to-number (match-string 2 filename))))
           (filename (if matched (match-string 1 filename) filename)))
      ad-do-it
      (when line-number
        ;; goto-line is for interactive use
        (goto-char (point-min))
        (forward-line (1- line-number))))))

(defvar ffap-file-at-point-line-number nil
  "Variable to hold line number from the last `ffap-file-at-point' call.")

(defadvice ffap-file-at-point (after ffap-store-line-number activate)
  "Search `ffap-string-at-point' for a line number pattern and
save it in `ffap-file-at-point-line-number' variable."
  (let* ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
         (name
          (or (condition-case nil
                  (and (not (string-match "//" string)) ; foo.com://bar
                       (substitute-in-file-name string))
                (error nil))
              string))
         (line-number-string
          (and (string-match ":[0-9]+" name)
               (substring name (1+ (match-beginning 0)) (match-end 0))))
         (line-number
          (and line-number-string
               (string-to-number line-number-string))))
    (if (and line-number (> line-number 0))
        (setq ffap-file-at-point-line-number line-number)
      (setq ffap-file-at-point-line-number nil))))

(defadvice find-file-at-point (after ffap-goto-line-number activate)
  "If `ffap-file-at-point-line-number' is non-nil goto this line."
  (when ffap-file-at-point-line-number
    (goto-line ffap-file-at-point-line-number)
    (setq ffap-file-at-point-line-number nil)))

;; Pressing d will copy both A and B to buffer C.
(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defun add-d-to-ediff-mode-map ()
  (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))

(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(defun open-var-log-messages (hostname)
  (interactive "shost: ")
  (let ((filename (format "/ssh:%s|sudo:%s:/var/log/messages" hostname hostname)))
    (find-file filename)))

(defun itail-var-log-messages (hostname)
  (interactive "shost: ")
  (let ((filename (format "/ssh:%s|sudo:%s:/var/log/messages" hostname hostname)))
    (itail filename)))
