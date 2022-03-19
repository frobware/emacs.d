(when aim/is-linux
  (if (and (daemonp) (locate-library "edit-server"))
      (progn
	(require 'edit-server)
	(setq edit-server-new-frame nil)
	(edit-server-start))))

(add-hook 'edit-server-text-mode-hook
	  (lambda ()
	    (auto-complete-mode 1)
	    (flyspell-mode 1)))

(add-hook 'edit-server-done-hook
	  (lambda ()
	    (shell-command "wmctrl -x -a google-chrome")))

(provide 'aim-edit-server)
