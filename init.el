(fset 'yes-or-no-p 'y-or-n-p)
(setq vc-follow-symlinks t)
(setq inhibit-startup-screen t)	     ; Don't show startup screen
(blink-cursor-mode -1)		     ; blink off!
(line-number-mode -1)		     ; have line numbers and
(column-number-mode 1)		     ; column numbers in the mode line
(tool-bar-mode -1)		     ; no tool bar with icons
(scroll-bar-mode -1)		     ; no scroll bars
(global-linum-mode -1)		     ; add line numbers on the left
(setq echo-keystrokes 0.02)	     ; Show keystrokes
(setq mouse-yank-at-point t)

(setq aim/is-darwin (eq system-type 'darwin)
      aim/is-linux (eq system-type 'gnu/linux))

;; Load all packages

(require 'package)

;; MELPA
;;
;; Up-to-date packages built on our servers from upstream source
;; Installable in any recent Emacs using "package.el" - no need to
;; install svn/cvs/hg/bzr/git/darcs etc. Curated - no obsolete,
;; renamed, forked or randomly hacked packages Comprehensive - more
;; packages than any other archive Automatic updates - new commits
;; result in new packages Extensible - contribute recipes via github,
;; and we'll build the packages

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar required-packages
  '(s dash rfringe popup php-mode mmm-mode js2-mode sml-mode
    ob-sml clojure-mode coffee-mode color-theme haml-mode
    ack-and-a-half haskell-mode magit markdown-mode mustache-mode
    notify paredit scala-mode yaml-mode yasnippet
    yasnippet-bundle j-mode ace-jump-mode expand-region
    zenburn-theme ido-ubiquitous smooth-scroll flx-ido
    python-mode go-mode nrepl color-theme-solarized git-messenger
    flycheck multiple-cursors powerline projectile
    flymake-go company-go ag)
  "Packages which should be installed upon launch")

(dolist (pkg required-packages)
  (when (not (package-installed-p pkg))
    (package-install pkg)))

(defun color-theme-undo ()
  (interactive)
  (color-theme-reset-faces)
  (color-theme-snapshot))

(require 'color-theme)

;; backup current color theme
(fset 'color-theme-snapshot (color-theme-make-snapshot))

(add-to-list 'load-path (concat user-emacs-directory "vendor/base16-emacs"))
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory))

(defun color-theme-for-window-sys (frame)
  (let ((color-theme-is-global nil))
    (select-frame frame)
    (if (window-system frame)
	(message "doing nothing for window system")
      (global-font-lock-mode -1))))

;; hook on after-make-frame-functions
(add-hook 'after-make-frame-functions 'color-theme-for-window-sys)

(require 'edebug)

(setq aim/is-darwin (eq system-type 'darwin)
      aim/is-linux (eq system-type 'gnu/linux))

(when (or aim/is-linux (not window-system))
  (menu-bar-mode -1))

(require 'dired-x)
(setq-default dired-omit-files-p t) ; this is buffer-local variable
(global-set-key (kbd "C-x C-j") 'dired-jump)

(require 'ibuffer)

(require 'epa-file)
(epa-file-enable)

(require 'ffap)
(ffap-bindings)

;; Git
(require 'magit nil 'noerror)
(eval-after-load 'magit
  (progn '(global-set-key (kbd "C-c i") 'magit-status)))

;; flx-ido completion system, recommended by Projectile
(require 'flx-ido)
(flx-ido-mode 1)
;; change it if you have a fast processor.
(setq flx-ido-threshhold 1000)

;; Project management
(require 'ack-and-a-half)
(require 'projectile)
(projectile-global-mode)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'none)

(require 'edebug)

;; frame-based visualization
(blink-cursor-mode -1)		     ; blink off!
(line-number-mode -1)		     ; have line numbers and
(column-number-mode 1)		     ; column numbers in the mode line
(tool-bar-mode -1)		     ; no tool bar with icons
(scroll-bar-mode -1)		     ; no scroll bars
;;(global-hl-line-mode)		     ; highlight current line
(global-linum-mode -1)		     ; add line numbers on the left

(when (or aim/is-linux (not window-system))
  (menu-bar-mode -1))

(setq vc-follow-symlinks t)

(require 'dired-x)
(setq-default dired-omit-files-p t) ; this is buffer-local variable
(global-set-key (kbd "C-x C-j") 'dired-jump)

(require 'ibuffer)

(require 'epa-file)
(epa-file-enable)

(require 'ffap)
(ffap-bindings)

(require 'aim-functions)

(when aim/is-darwin
  (aim/set-exec-path-from-shell-PATH))

(require 'aim-customize)
(require 'aim-frame)
(require 'aim-recentf)
(require 'aim-uniquify)
(require 'aim-global-keybindings)
(require 'aim-whatever)
;;(require 'aim-color-theme)
(require 'aim-go)
(require 'server)

(unless (server-running-p)
  (server-start))

(defvar short-system-name
  (car (split-string (system-name) "\\."))
  "Returns the short form of (system-name)")

(require 'company)

(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-minimum-prefix-length 0)               ; autocomplete right after '.'
(setq company-idle-delay .5)                         ; shorter delay before autocompletion popup
(setq company-echo-delay 0)                          ; removes annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

(add-to-list 'company-backends 'company-dabbrev t)
(add-to-list 'company-backends 'company-ispell t)

(and (fboundp 'company-aspell)
     (add-to-list 'company-backends 'company-aspell t))

(and (fboundp 'company-ispell)
     (add-to-list 'company-backends 'company-ispell t))

(add-to-list 'company-backends 'company-files t)

(global-company-mode 1)

(require 'autopair nil 'noerror)
