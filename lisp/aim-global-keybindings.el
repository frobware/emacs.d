(mapcar '(lambda (x)
	   (global-set-key (car x) (cdr x)))
	'(("\C-x\C-b"      . electric-buffer-list)
	  ("\C-x\C-j"      . dired-jump)
	  ("\C-x\m"        . gnus-msg-mail)
	  ([f1]            . gnus-slave)
	  ([f2]            . aim/revert-buffer-now)
	  ([f3]            . whitespace-cleanup)
	  ([f4]            . aim/reverse-video)
	  ("\C-xC"         . compile)
	  ("\C-xg"         . goto-line)
	  ("\C-x\C-g"      . goto-line)
	  ("\C-ci"         . magit-status)
	  ))

(when aim/is-linux
  (global-set-key [f11] 'aim/fullscreen))

(provide 'aim-global-keybindings)
