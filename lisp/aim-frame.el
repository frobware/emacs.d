;; Do this upfront so that if there's some error then some of the less
;; irritating things have been fixed already.

(require 'server)

(when aim/is-darwin
  (when window-system
    (progn
      (set-default-font "Menlo-18")
      ;; (aim/reverse-video)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'none))))

;; Make new frame visible when connecting via emacsclient
;; (add-hook 'server-switch-hook 'raise-frame)

(defun aim/frame-config (frame)
  "Custom behaviour for new frames."
  (with-selected-frame frame
    (when (display-graphic-p)
      (set-background-color "grey20")
      (set-foreground-color "grey90"))
    (when aim/is-darwin
      (when window-system
	(progn
	  (set-default-font "Menlo-18")
	  ;; (aim/reverse-video)
	  (setq mac-command-modifier 'meta)
	  (setq mac-option-modifier 'none))))))

;; run now
;;(aim/frame-config (selected-frame))

;; and later
;;(add-hook 'after-make-frame-functions 'aim/frame-config)

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(provide 'aim-frame)
