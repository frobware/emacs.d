;; https://github.com/hrs/dotfiles/blob/master/emacs/.emacs.d/configuration.org
;; thanks man!

(setq hrs/default-font-size 20)
(setq hrs/default-font "JetBrains Mono")
(setq hrs/current-font-size hrs/default-font-size)
(setq hrs/font-change-increment 1.1)

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
  "Decrease current font size by a factor `hrs/font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq hrs/current-font-size
	(max 1
	     (floor (/ hrs/current-font-size hrs/font-change-increment))))
  (hrs/set-font-size))

(define-key global-map (kbd "C-)") 'hrs/reset-font-size)
(define-key global-map (kbd "C-+") 'hrs/increase-font-size)
(define-key global-map (kbd "C--") 'hrs/decrease-font-size)

(provide 'hrs)
