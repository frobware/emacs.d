;;; early-init.el -*- lexical-binding: t; -*-

;;; Lots of this taken from (the awesome) doom emacs - thanks!

;; Prevent unwanted runtime compilation for gccemacs (native-comp)
;; users; packages are compiled ahead-of-time when they are installed
;; and site files are compiled when gccemacs is installed.
(setq native-comp-deferred-compilation nil)

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

(setq package-archives nil)
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; Resizing the Emacs frame can be a terribly expensive part of
;; changing the font. By inhibiting this, we easily halve startup
;; times with fonts that are larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

(setq initial-major-mode 'fundamental-mode)

;; Contrary to what many Emacs users have in their configs, you don't
;; need more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;; set-language-enviornment sets default-input-method, which is
;; unwanted.
(setq default-input-method nil)

;;; The rest is largely my gunk.

;; Prevent the glimpse of un-styled Emacs by disabling these UI
;; elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 5))
(add-to-list 'default-frame-alist '(foreground-color . "white"))
(add-to-list 'default-frame-alist '(background-color . "black"))

(defun aim/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'modus-operandi t))
    ('dark (load-theme 'modus-vivendi t))))

(if (eq system-type 'darwin)
    (progn
      (add-hook 'ns-system-appearance-change-functions #'aim/apply-theme)
      (add-to-list 'default-frame-alist '(font . "JetBrains Mono-20")))
  (progn
    (add-to-list 'default-frame-alist '(font . "JetBrains Mono"))
    (add-to-list 'default-frame-alist '(undecorated . t))
    (add-to-list 'default-frame-alist '(fullscreen . maximized))))
