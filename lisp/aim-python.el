(add-to-list 'interpreter-mode-alist
	     (cons '("python" . python-mode) interpreter-mode-alist))

(add-hook 'python-mode
	  '(lambda ()
	     (set-variable 'require-final-newline nil)
	     'py-indent-offset 4
	     'indent-tabs-mode nil))

(provide 'aim-python)
