(add-hook 'after-init-hook
	  (lambda ()
	    (load-theme 'modus-vivendi t)))

(setq package-check-signature nil)

;; from https://matthewbauer.us/bauer/#install and
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq gc-cons-threshold
      most-positive-fixnum)

(add-hook 'after-init-hook
	  (lambda ()
	    (garbage-collect)
	    (setq gc-cons-threshold
		  (car (get 'gc-cons-threshold 'standard-value)))))

(fset 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; https://github.com/hrs/dotfiles/blob/master/emacs/.emacs.d/configuration.org
;; thanks man!

(setq hrs/default-font "DejaVu Sans Mono")
(setq hrs/default-font-size 12)
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
  "Decrease current font size by a factor of
`hrs/font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq hrs/current-font-size
	(max 1
	     (floor (/ hrs/current-font-size hrs/font-change-increment))))
  (hrs/set-font-size))

(define-key global-map (kbd "C-)") 'hrs/reset-font-size)
(define-key global-map (kbd "C-+") 'hrs/increase-font-size)
(define-key global-map (kbd "C--") 'hrs/decrease-font-size)

(hrs/reset-font-size)

;; Mitigate Bug#28350 (security) in Emacs 25.2 and earlier.
;; http://seclists.org/oss-sec/2017/q3/422
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

(setq aim/elpa-mirror-dir
      (expand-file-name (concat user-emacs-directory ".elpa-mirror")))

;; If there's no .elpa-mirror then clone as:
;;   git clone --depth 1 git@github.com:d12frosted/elpa-mirror.git ~/.emacs.d/.elpa-mirror

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
	("MELPA Stable" . "https://stable.melpa.org/packages/")
	("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"     . 5)
	("MELPA Stable" . 10)
	("MELPA"        . 0)))

(setq package-archives
      `(("melpa" . ,(concat aim/elpa-mirror-dir "/melpa/"))
	("stable-melpa" . ,(concat aim/elpa-mirror-dir "/stable-melpa/"))
	("org" . ,(concat aim/elpa-mirror-dir "/org/"))
	("gnu" . ,(concat aim/elpa-mirror-dir "/gnu/")))
      package-archive-priorities
      `(("melpa" . 5)
	("stable-melpa" . 100)
	("org"   . 50)
	("gnu"   . 0)))

(require 'package)
(package-initialize)
;; Avoid loading the packages again after processing the init file.
(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Always compile packages, and use the newest version available.
(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

(defvar aim/is-darwin (eq system-type 'darwin))
(defvar aim/is-linux (eq system-type 'gnu/linux))

(progn
  (add-to-list 'default-frame-alist '(undecorated . nil))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(setq abbrev-file-name
      (expand-file-name (concat user-emacs-directory "/.abbrevs")))

;; If this code is being evaluated by emacs --daemon, ensure that each
;; subsequent frame is themed appropriately.
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque."
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(use-package almost-mono-themes)

(defun get-frame-name (&optional frame)
  (interactive)
  "Return the string that names FRAME (a frame).  Default is selected frame."
  (unless frame (setq frame (selected-frame)))
  (if (framep frame)
      (cdr (assq 'name (frame-parameters frame)))
    (error "Function `get-frame-name': Argument not a frame: `%s'" frame)))

;; from https://nicolas.petton.fr/blog/emacs-dark-window-decoration.html
(defun set-selected-frame-dark-window-decoration ()
  (interactive)
  (let ((frame-name (get-frame-name (selected-frame))))
    (call-process-shell-command (concat "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"dark\" -name \"" frame-name "\""))))

;; (defun hrs/apply-theme ()
;;   (interactive)
;;   ;;(load-theme 'almost-mono-black t)
;;   (if (window-system)
;;       (set-selected-frame-dark-window-decoration))
;;   (transparency 100))

;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;;	      (lambda (frame)
;;		(with-selected-frame frame (hrs/apply-theme))))
;;   (hrs/apply-theme))

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; When saving files, set execute permission if #! is in first line.
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

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

(require 'dired-x)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(setq-default dired-omit-mode t)

;; (use-package dired-x
;;   :defer 5
;;   :config
;;   (progn
;;     (global-set-key (kbd "C-x C-j") 'dired-jump)
;;     (add-to-list 'dired-omit-extensions ".cmd")
;;     (setq-default dired-omit-mode t)))

(use-package cmake-mode
  :mode ("\\.cmake$" . cmake-mode))

(use-package ag
  :config
  (progn
    (setq ag-highlight-search t
	  ag-reuse-buffers t)
    (add-hook 'ag-mode-hook 'wgrep-ag-setup)))

(use-package wgrep-ag
  :config
  (setq wgrep-auto-save-buffer t))

(use-package magit
  :bind ("C-c i" . magit-status)
  :commands magit-status
  :config
  (progn
    (global-magit-file-mode -1)
    (setq magit-refresh-status-buffer nil
	  magit-auto-revert-mode nil
	  magit-diff-arguments (quote ("--function-context" "--no-ext-diff" "--stat"))
	  magit-pull-arguments nil)))

;;'(magit-diff-hunk-heading-highlight ((t (:background "grey30" :foreground "grey90")))))

(use-package magit-gh-pulls
  :commands turn-on-magit-gh-pulls
  :config
  (progn
    (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)))

(use-package markdown-mode
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode)
  :config)

(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands))
  :commands smex
  :config
  (progn
    (smex-initialize)))

(use-package git-commit)
;;  :hook (git-commit-setup-hook . git-commit-turn-on-flyspell))

(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

(use-package ispell
  :config
  ;; Setting ‘flyspell-issue-message-flag’ to nil, as printing
  ;; messages for every word (when checking the entire buffer) causes
  ;; an enormous slowdown.
  (setq flyspell-issue-message-flag nil))

(use-package cmake-mode
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)))

(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))

(use-package yaml-mode
  :ensure yaml-mode
  :mode "\\.ya?ml\\'"
  :config
  (progn
    (bind-key "C-c C-j" 'aj-toggle-fold yaml-mode-map)))

(use-package browse-url)

(use-package go-add-tags
  :ensure go-add-tags)

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

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; (use-package uniquify
;;   :config
;;   (progn
;;     (setq uniquify-buffer-name-style 'reverse)
;;     (setq uniquify-separator "|")
;;     (setq uniquify-after-kill-buffer-p t)
;;     (setq uniquify-ignore-buffers-re "^\\*")))

(use-package dockerfile-mode)

(require 'lisp-mode)

(use-package ffap
  :config (ffap-bindings))

(use-package git-gutter
  :config
  (global-git-gutter-mode t)
  (setq git-gutter:modified-sign " "
	git-gutter:added-sign " "
	git-gutter:deleted-sign " "
	git-gutter:lighter " GG")
  (set-face-background 'git-gutter:modified "DarkGoldenrod4")
  (set-face-foreground 'git-gutter:added "dark green")
  (set-face-foreground 'git-gutter:deleted "dark red")
  ;; (global-set-key (kbd "C-x C-g") 'git-gutter)
  ;; (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
  ;; Jump to next/previous hunk
  (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x n") 'git-gutter:next-hunk))

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
  :config (progn
	    (set-variable 'py-indent-offset 4)
	    (set-variable 'indent-tabs-mode nil)))

(use-package godoctor)

(use-package go-dlv)

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
    ;; (add-to-list 'tramp-default-proxies-alist
    ;;		 '(nil "\\`root\\'" "/ssh:%h:"))
    ;; (add-to-list 'tramp-default-proxies-alist
    ;;		 '((regexp-quote (system-name)) nil nil))
    ;;    (set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
    ;; I added the ".*" for NixOS
    (setq tramp-shell-prompt-pattern
	 "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*")
    (add-to-list 'tramp-connection-properties
		 (list ".*" "locale" "LC_ALL=C"))
    (setq tramp-ssh-controlmaster-options
	  (concat
	   "-o ControlPath=tramp.%%r@%%h:%%p "
	   "-o ControlMaster=auto "
	   "-o ControlPersist=yes"))))

(use-package guide-key
  :config (setq guide-key/guide-key-sequence '("C-c p" "C-x 4")))

(use-package aim-functions
  :load-path "lisp/")

(use-package aim-global-keybindings
  :load-path "lisp/")

;;(add-hook 'lisp-mode #'(complete-mode 1))

(defadvice kill-line (before check-position activate)
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
	     (just-one-space 0)
	     (backward-char 1))))

(electric-indent-mode 1)

(defun aim/run-go-buffer ()
  (interactive)
  (shell-command (format "go run %s" (buffer-file-name (current-buffer)))))

(defun aim/isearch-face-settings ()
  (interactive)
  (set-face-foreground 'isearch "black")
  (set-face-background 'isearch "yellow")
  (set-face-foreground 'lazy-highlight "black")
  (set-face-background 'lazy-highlight "orange"))

;; (eval-after-load "isearch"
;;   `(aim/isearch-face-settings))

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
;;(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))

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

(use-package recentf
  :config
  (progn
    (setq recentf-auto-cleanup 'never
	  recentf-max-saved-items 100)
    (recentf-mode 1)))

(add-to-list 'recentf-exclude
	     (expand-file-name "~/.emacs.d/elpa"))

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

;; (use-package peep-dired
;;   :ensure t
;;   :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
;;   :bind (:map dired-mode-map
;;	      ("P" . peep-dired)))

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

;; (eval-after-load 'diff-mode
;;   '(progn
;;      (set-face-foreground 'diff-added "brightgreen")
;;      (set-face-foreground 'diff-changed "bold white")
;;      (set-face-foreground 'diff-removed "brightred")))

(defalias 'ttl 'toggle-truncate-lines)

(use-package cargo)

;; In the environment you'll need:
;;    export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
;;
;; And you'll need the actual src component:
;;     $ rustup component add rust-src
(use-package racer
  :config
  (progn
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)))

(use-package rust-mode
  :config (progn
	    (setq rust-format-on-save t)))

(unless (fboundp 'xref-push-marker-stack)
  (defalias 'xref-pop-marker-stack 'pop-tag-mark)

  (defun xref-push-marker-stack (&optional m)
    "Add point to the marker stack."
    (ring-insert find-tag-marker-ring (or m (point-marker)))))

(defadvice gdb-inferior-filter
    (around gdb-inferior-filter-without-stealing)
  (with-current-buffer (gdb-get-buffer-create 'gdb-inferior-io)
    (comint-output-filter proc string)))

(ad-activate 'gdb-inferior-filter)

(use-package elide-head
  :config
  (setq elide-head-headers-to-hide
	(quote
	 (("is free software[: ;] you can redistribute it" . "\\(Boston, MA 0211\\(1-1307\\|0-1301\\), USA\\|If not, see <http://www\\.gnu\\.org/licenses/>\\)\\.")
	  ("The Regents of the University of California\\.  All rights reserved\\." . "SUCH DAMAGE\\.")
	  ("Permission is hereby granted, free of charge" . "authorization from the X Consortium\\.")
	  ("Copyright .* The Kubernetes Authors." . "limitations under the License."))))
  (add-hook 'go-mode-hook 'elide-head)
  (add-hook 'c-mode-common-hook 'elide-head))

(use-package helm)
(use-package helm-company)

(use-package helm-ls-git
  :config
  (global-set-key (kbd "C-c C-l") 'helm-ls-git-ls))

(use-package protobuf-mode)

(use-package terraform-mode)

(use-package pass)

(use-package gist)

(use-package direnv
  :config
  (progn
    (direnv-mode)
    (setq direnv-always-show-summary nil)))

(use-package notmuch
  :config
  (setq notmuch-hello-thousands-separator ",")
  (setq notmuch-search-oldest-first nil))

(defun atomic-chrome-server-running-p ()
  "Returns t if the atomic-chrome server is currently running, otherwise nil."
  (let ((retval nil))
    (condition-case ex
	(progn
	  (delete-process
	   (make-network-process
	    :name "atomic-client-test" :host "localhost"
	    :noquery t :service "64292"))
	  (setq retval t))
      ('error nil))
    retval))

(use-package atomic-chrome
  :config
  (when (not (atomic-chrome-server-running-p))
    (atomic-chrome-start-server)))

;; (use-package smart-shift
;;   :ensure t
;;   :config
;;   (global-smart-shift-mode t))

(use-package jinja2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode)))

;; (use-package ini-mode
;;   :ensure t)

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
    (with-no-warnings
      (goto-line ffap-file-at-point-line-number))
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

(use-package pinentry
  :config
  (setq epa-pinentry-mode 'loopback))

(use-package adoc-mode)

(use-package use-package-ensure-system-package)

(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

(defun aim/frame-colours-unspecified (frame)
  (interactive)
  (let ((fg (face-attribute 'default :foreground frame))
	(bg (face-attribute 'default :background frame)))
    (and (equal fg "unspecified-fg") (equal bg "unspecified-bg"))))

(defun aim/on-frame-open (frame)
  (interactive)
  (message "before FRAME %s" (frame-parameters frame))
  (message "before FRAME background-mode %s" (frame-parameter frame 'background-mode))
  (message "before FRAME foreground-mode %s" (frame-parameter frame 'foreground-mode))
  (if (not (display-graphic-p frame))
      (progn
	(if (and (equal (frame-parameter frame 'background-mode) 'light)
		 (aim/frame-colours-unspecified frame))
	    (progn
	      (set-frame-parameter frame 'background-color "#000000")
	      (set-frame-parameter frame 'foreground-color "#FFFFFF"))
	  (if (and (equal (frame-parameter frame 'background-mode) nil)
		   (aim/frame-colours-unspecified frame))
	      (progn
		(set-frame-parameter frame 'background-color "#FFFFFF")
		(set-frame-parameter frame 'foreground-color "#000000"))))))
  (message "after FRAME %s" (frame-parameters frame)))

(defun aim/maybe-frob-colours (frame)
  (interactive)
  (and (string-match "xterm-" (getenv "TERM"))
       (aim/on-frame-open (selected-frame))))

(defun kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer) (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
			   (kill-buffer buffer)))
	(buffer-list)))

;; This package is easiest way to open particular link on
;; github/gitlab/bitbucket/stash/git.savannah.gnu.org from Emacs
(use-package browse-at-remote)

(use-package git-timemachine)

(defun browse-url-chromote (url &rest ignore)
  "Browse URL using browse-url-chromote."
  (interactive "sURL: ")
  (shell-command (expand-file-name "~/bin/browse-url-chromote ") url))

;;(setq browse-url-browser-function 'browse-url-chromote)

(aim/set-global-keybindings)

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(require 'rust-mode)

(when (file-exists-p (expand-file-name "~/emacs-libvterm/vterm-module.so"))
  (add-to-list 'load-path (expand-file-name "~/emacs-libvterm"))
  (require 'vterm))

(use-package helm-pass)

(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (setq nix-indent-function #'nix-indent-line))

(use-package deadgrep)
(global-set-key (kbd "<f5>") #'deadgrep)

(defun aim/light-mode ()
  (interactive)
  (setq frame-background-mode 'light)
  (mapc 'frame-set-background-mode (list (selected-frame))))

(defun aim/dark-mode ()
  (interactive)
  (setq frame-background-mode 'dark)
  (mapc 'frame-set-background-mode (list (selected-frame))))

(use-package auto-compile
  :config (auto-compile-on-load-mode))

;; (use-package projectile
;;   :bind
;;   ("C-c v" . 'projectile-ag)
;;   ("C-c v" . 'projectile-deadgrep)
;;   :config
;;   (setq projectile-completion-system 'ido)
;;   (setq projectile-switch-project-action 'projectile-dired)
;;   (setq projectile-require-project-root nil))

;; https://tuhdo.github.io/helm-projectile.html
;; (use-package helm-projectile)
;; (projectile-global-mode)
;; (setq projectile-completion-system 'helm)
;; (helm-projectile-on)
;; (setq projectile-switch-project-action 'helm-projectile-find-file)
;; (setq projectile-switch-project-action 'helm-projectile)

(use-package helm-ag)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(defvar emacsql-sqlite-executable
  (expand-file-name (concat user-emacs-directory "/emacsql-sqlite")))

(use-package multi-term)

(global-set-key (kbd "C-c t") 'multi-term)
(setq multi-term-program-switches "--login")
(put 'magit-clean 'disabled nil)

;;Load auto-complete
;; (use-package go-autocomplete)
;; (ac-config-default)
;; (require 'auto-complete-config)
;; (require 'go-autocomplete)

;; suggest things when company has nothing to say
(setq-default tab-always-indent 'complete)

;; ;; turn off annoying tooltips
(use-package company
  :config (setq company-frontends nil)
  ;;:hook (after-init . global-company-mode)
  :config
  :ensure company
  :config
  (setq company-idle-delay 0
	company-tooltip-limit 20
	company-minimum-prefix-length 3
	company-echo-delay 0
	company-require-match nil
	company-auto-complete nil)
  :hook (prog-mode . company-mode)
  ;;(global-company-mode 1)
  )

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

(setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
	company-preview-frontend
	company-echo-metadata-frontend))

;;(setq company-auto-complete 'never)
;;(company-tng-configure-default)

;; (use-package go-mode
;;   :ensure go-mode
;;   :mode "\\.go\\'"
;;   :config
;;   ;; (add-hook 'go-mode 'gofmt-before-save)
;;   ;; (remove-hook 'go-mode 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
;;   ;; (setq gofmt-command "goimports")
;;   (bind-key "C-M-x" 'aim/run-go-buffer go-mode-map)
;;   (bind-key "C-M-i" 'helm-company go-mode-map)
;;   ;;(bind-key "M-." 'godef-jump go-mode-map)
;;   )

;; (use-package go-mode
;;   :mode "\\.go\\'"
;;   :custom (gofmt-command "goimports")
;;   :bind (:map go-mode-map
;;	      ("C-c C-n" . go-run)
;;	      ("C-c ."   . go-test-current-test)
;;	      ("C-c f"   . go-test-current-file)
;;	      ("C-c a"   . go-test-current-project))
;;   :config
;;   (add-hook 'before-save-hook #'gofmt-before-save)
;;   (setq gofmt-command "goimports")
;;   (use-package gotest)
;;   (use-package go-tag
;;     :config (setq go-tag-args (list "-transform" "camelcase"))))

;; from https://lupan.pl/dotemacs/
(defun my-go-electric-brace ()
  "Insert an opening brace may be with the closing one.
If there is a space before the brace also adds new line with
properly indented closing brace and moves cursor to another line
inserted between the braces between the braces."
  (interactive)
  (insert "{")
  (when (looking-back " {")
    (newline)
    (indent-according-to-mode)
    (save-excursion
      (newline)
      (insert "}")
      (indent-according-to-mode))))

(defun my-godoc-package ()
  "Display godoc for given package (with completion)."
  (interactive)
  (godoc (or (helm :sources (helm-build-sync-source "Go packages"
			    :candidates (go-packages))
		   :buffer "*godoc packages*")
	     (signal 'quit nil))))

(use-package go-mode
  :init
  (setq go-fontify-function-calls nil)
  :bind
  (:map go-mode-map
	("C-c e g" . godoc)
	("C-c P" . my-godoc-package)
	("{" . my-go-electric-brace))
  :hook ((go-mode . lsp)
	 (go-mode . smartparens-mode)))

(defun aim/setup-ac-complete nil
  (interactive)
  (use-package go-autocomplete)
  (require 'go-autocomplete)
  (require 'auto-complete-config)
  (ac-config-default)
  (define-key ac-complete-mode-map "\t" 'ac-expand)
  (define-key ac-complete-mode-map "\r" 'ac-complete)
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous)
  ;;(set-default 'ac-sources '(ac-source-abbrev ac-source-words-in-buffer))
  )

(defun aim/setup-company-complete nil
  (interactive)
  (use-package company-go
    :after company
    :ensure company-go
    :config (add-to-list 'company-backends 'company-go))
  (add-hook 'go-mode-hook
	    (lambda ()
	      (set (make-local-variable 'company-backends) '(company-go))
	      (company-mode))))

(setq aim/prefer-ac-complete nil)

(if aim/prefer-ac-complete
    (aim/setup-ac-complete)
  (aim/setup-company-complete))

(use-package smartparens
  :ensure t
  :config
  (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  :diminish smartparens-mode)

(use-package flx)

;; (use-package counsel
;;   :config
;;   (global-set-key (kbd "M-x") 'counsel-M-x)
;;   ;;(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;   (global-set-key (kbd "C-c g") 'counsel-git)
;;   (global-set-key (kbd "C-c j") 'counsel-git-grep)
;;   (global-set-key (kbd "C-c a") 'counsel-ag)
;;   (global-set-key (kbd "C-c r") 'counsel-rg)
;;   (global-set-key (kbd "C-x l") 'counsel-locate)
;;   (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;; (use-package ivy
;;   :config
;;   (setq ivy-use-selectable-prompt t
;;	ivy-use-virtual-buffers t       ; Enable bookmarks and recentf
;;	ivy-height 10
;;	ivy-count-format "(%d/%d) "
;;	ivy-on-del-error-function nil
;;	ivy-initial-inputs-alist nil
;;	ivy-count-format " "
;;	ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

;; (use-package ivy
;;   :ensure t
;;   :diminish (ivy-mode . "")
;;   :bind
;;   (:map ivy-mode-map
;;	("C-'" . ivy-avy))
;;   :config
;;   (ivy-mode 1)
;;   ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
;;   (setq ivy-use-virtual-buffers t)
;;   ;; number of result lines to display
;;   (setq ivy-height 10)
;;   ;; does not count candidates
;;   (setq ivy-count-format "")
;;   ;; no regexp by default
;;   (setq ivy-initial-inputs-alist nil)
;;   ;; configure regexp engine.
;;   (setq ivy-re-builders-alist
;;	;; allow input not in order
;;         '((t . ivy--regex-ignore-order))))

;; (use-package ivy-posframe
;;   :config
;;   (setq ivy-posframe-height-alist '((swiper . 10)
;;                                     (t      . 5)))
;;   (setq ivy-posframe-display-functions-alist
;;	'((swiper          . nil)
;;           (complete-symbol . ivy-posframe-display-at-point)
;;           (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
;;           (t               . ivy-posframe-display)))
;;   (ivy-posframe-mode 1))

;; (use-package swiper
;;   :config
;;   (global-set-key (kbd "C-s") 'swiper))

(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets)
;;(use-package yasnippet-classic-snippets)

;;(use-package projectile
;;  :ensure t
;;  :bind (:map projectile-mode-map
;;              ("C-c p" . 'projectile-command-map))
;;  :config
;;  (projectile-mode +1))

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;; (setq lsp-keymap-prefix "s-l")
;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp
;;   :custom
;;   (lsp-auto-guess-root nil)
;;   ;;(lsp-prefer-flymake nil) ; Use flycheck instead of flymake
;;   :config
;;   (setq lsp-auto-guess-root t)
;;   (setq lsp-inhibit-message t)
;;   ;;(setq lsp-message-project-root-warning t)
;;   :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
;;   :hook ((go-mode c-mode c++-mode) . lsp))

;; (use-package lsp-ui
;;   :after lsp-mode
;;   :diminish
;;   :commands lsp-ui-mode
;;   ;; :custom-face
;;   ;; (lsp-ui-doc-background ((t (:background nil))))
;;   ;; (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
;;   :bind (:map lsp-ui-mode-map
;;               ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
;;               ([remap xref-find-references] . lsp-ui-peek-find-references)
;;               ("C-c u" . lsp-ui-imenu))
;;   :custom
;;   (lsp-ui-doc-enable t)
;;   (lsp-ui-doc-header t)
;;   (lsp-ui-doc-include-signature t)
;;   (lsp-ui-doc-position 'top)
;;   (lsp-ui-doc-border (face-foreground 'default))
;;   (lsp-ui-sideline-enable nil)
;;   (lsp-ui-sideline-ignore-duplicate t)
;;   (lsp-ui-sideline-show-code-actions nil)
;;   :config
;;   ;; Use lsp-ui-doc-webkit only in GUI
;;   (setq lsp-ui-doc-use-webkit t)
;;   ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
;;   ;; https://github.com/emacs-lsp/lsp-ui/issues/243
;;   (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
;;     (setq mode-line-format nil)))

;; (use-package company-lsp
;;   :commands company-lsp
;;   :custom
;;   (company-lsp-cache-candidates t) ;; auto, t(always using a cache), or nil
;;   (company-lsp-async t)
;;   (company-lsp-enable-snippet t)
;;   (company-lsp-enable-recompletion t)
;;   :config
;;   (push 'company-lsp company-backends))

(use-package k8s-mode
  :hook (k8s-mode . yas-minor-mode))

;; Making it easier to discover Emacs key presses.
(use-package which-key
  :diminish
  :config (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 3.50))

;; Get org-headers to look pretty! E.g., * → ⊙, ** ↦ ◯, *** ↦ ★
;; https://github.com/emacsorphanage/org-bullets
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package ssh-config-mode)
(defun aim/tramp-borked ()
    (interactive)
    (tramp-cleanup-all-connections)
    (tramp-cleanup-all-buffers))

(use-package docker-tramp
  :ensure t)

(use-package eshell-bookmark
  :after eshell
  :config
  (add-hook 'eshell-mode-hook #'eshell-bookmark-setup))
(org-babel-do-load-languages 'org-babel-load-languages
    '(
	(shell . t)
    )
)

(setq org-confirm-babel-evaluate nil)

(use-package modus-operandi-theme)
(use-package modus-vivendi-theme)
;;(load-theme 'modus-operandi t)          ; Light theme
;;(load-theme 'modus-vivendi t)

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

;; (setq lsp-gopls-staticcheck t)
;; (setq lsp-eldoc-render-all t)
;; (setq lsp-gopls-complete-unimported t)

(use-package lsp-mode
  :config
  ;;(lsp-prefer-flycheck t) ; Use flycheck instead of flymake
  :commands (lsp lsp-deferred)
  :hook ((before-save . lsp-format-buffer)
	 (before-save . lsp-organize-imports))
  :bind (("C-c d" . lsp-describe-thing-at-point)
	 ("C-c e n" . flymake-goto-next-error)
	 ("C-c e p" . flymake-goto-prev-error)
	 ("C-c e r" . lsp-find-references)
	 ("C-c e R" . lsp-rename)
	 ("C-c e i" . lsp-find-implementation)
	 ("C-c e t" . lsp-find-type-definition))
  :hook (go-mode . lsp-deferred))

;;Set up before-save hooks to format buffer and add/delete imports.
;;Make sure you don't have other gofmt/goimports hooks enabled.

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;;Optional - provides fancier overlays.
;; (use-package lsp-ui)

(use-package company-lsp
  :ensure t
  :commands company-lsp)

;;Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(setq lsp-ui-doc-enable nil
      lsp-ui-peek-enable nil
      lsp-ui-sideline-enable nil
      lsp-ui-imenu-enable nil
      lsp-ui-flycheck-enable t)

(defun my-dumb-jump-mode-hook ()
    (define-key dumb-jump-mode-map (kbd "C-M-g") nil)
    (define-key dumb-jump-mode-map (kbd "C-M-p") nil)
    (define-key dumb-jump-mode-map (kbd "C-M-q") nil))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
	 ("M-g j" . dumb-jump-go)
	 ("M-g b" . dumb-jump-back)
	 ("M-g i" . dumb-jump-go-prompt)
	 ("M-g x" . dumb-jump-go-prefer-external)
	 ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'helm
		dumb-jump-debug nil
		dumb-jump-prefer-searcher 'rg)
  :hook (prog-mode . dumb-jump-mode)
  :ensure t)

(eval-after-load "dumb-jump"
  (add-hook 'go-mode 'my-dumb-jump-mode-hook))

;; Fix trailing spaces but only in modified lines
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package server
  :config (or (server-running-p) (server-mode)))
