;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

(setq package-archives nil)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq default-frame-alist
       '((height . 70)
         (width . 174)
         (left . 613)
         (top . 391)
         (vertical-scroll-bars . nil)
         (horizontal-scroll-bars . nil)
         (menu-bar-lines . 0)
         (tool-bar-lines . 0)))

(progn
  ;; (add-to-list default-frame-alist '((menu-bar-lines . 0)
  ;; 				     (tool-bar-lines . 0)
  ;; 				     (vertical-scroll-bars . nil)))
  (add-to-list 'default-frame-alist '(undecorated . t))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(drag-internal-border . 1))
  (add-to-list 'default-frame-alist '(internal-border-width . 5)))

;; Resizing the Emacs frame can be a terribly expensive part of
;; changing the font. By inhibiting this, we easily halve startup
;; times with fonts that are larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; https://www.reddit.com/r/emacs/comments/f3ed3r/how_is_doom_emacs_so_damn_fast/

;; (when (member "Ubuntu One" (font-family-list))
;;   (set-face-attribute 'default nil
;; 		      :family "Ubuntu Mono"
;; 		      :height 140
;; 		      :weight 'regular))

;; ;; Set default font
;; (set-face-attribute 'default nil
;;                     :family "Ubuntu Mono"
;;                     :height 140
;;                     :width 'normal)

;; (setq initial-major-mode 'fundamental-mode)

;; (setq comp-deferred-compilation t)

(setq inhibit-startup-screen t
      inhibit-splash-screen t
      inhibit-startup-message t)
