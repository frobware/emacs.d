(require 'flymake-go)
(require 'go-mode)
(require 'company-go)

(setq gofmt-command "goimports")
(add-to-list 'load-path (expand-file-name "~/.gotools/bin"))

(require 'go-mode-load)
(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook
	  (lambda ()
	    (set (make-local-variable 'company-backends) '(company-go))
	    (flymake-mode-on)
	    (company-mode)))

(defun aim/run-go-buffer ()
  (interactive)
  (shell-command (format "go run %s" (buffer-file-name (current-buffer)))))

;;(require 'go-direx) ;; Don't need to require, if you install by package.el

(add-hook 'go-mode-hook
	  (lambda ()
	    (setq truncate-lines t)
	    (define-key go-mode-map (kbd "C-c C-j") 'go-direx-pop-to-buffer)
	    (electric-pair-mode 1)
	    (flymake-mode 1)
	    (local-set-key (kbd "C-M-x") 'aim/run-go-buffer)
	    (local-set-key (kbd "M-.") 'godef-jump)
	    (local-set-key (kbd "M-/") 'company-complete)
	    (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))

(provide 'aim-go)
