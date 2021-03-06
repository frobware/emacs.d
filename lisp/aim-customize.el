;;
;; Load and/or create empty custom file.
;;
(let ((fn (expand-file-name "~/.emacs-custom.el")))
  (when (not (file-exists-p fn))
    (shell-command (concat "touch " fn)))
  (setq custom-file fn)
  (load custom-file))

(load custom-file 'noerror)

(setq vc-follow-symlinks t)

(setq auto-mode-alist (cons '("\\.mm$" . c++-mode) auto-mode-alist)
      c-default-style "linux"
      vc-follow-symlinks t
      inhibit-splash-screen t
      ring-bell-function '(lambda ())
      sentence-end-double-space nil
      require-final-newline t)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(setq savehist-mode t)
(setq history-lenth 1000)

;; Share the clipboard
(setq x-select-enable-clipboard t)

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; When saving files, set execute permission if #! is in first line.
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

(provide 'aim-customize)
