;; View occurrence in occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

;; find file as root
(defun djcb-find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

(global-set-key (kbd "C-x C-S-f") 'djcb-find-file-as-root)

(defun occurrences (regexp &rest ignore)
  "Show all matches for REGEXP in an `occur' buffer."
  ;; keep text covered by occur-prefix and match text-properties
  (interactive (occur-read-primary-args))
  (occur regexp)
  (with-current-buffer (get-buffer "*Occur*")
    (let ((inhibit-read-only t)
	  delete-from
	  pos)
      (save-excursion
	(while (setq pos (next-property-change (point)))
	  (goto-char pos)
	  (if (not (or (get-text-property (point) 'occur-prefix)
		       (get-text-property (point) 'occur-match)))
	      (if delete-from
		  (delete-region delete-from (point))
		(setq delete-from (point)))
	    (when delete-from
	      (delete-region delete-from (point))
	      (if (get-text-property (point) 'occur-prefix)
		  (insert "\n")
		(insert " ")))
	    (setq delete-from nil)))))))

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; If you do use M-x term, you will notice there's line mode that acts like
;; emacs buffers, and there's the default char mode that will send your
;; input char-by-char, so that curses application see each of your key
;; strokes.
;;
;; The default way to toggle between them is C-c C-j and C-c C-k, let's
;; better use just one key to do the same.
(require 'term)
(define-key term-raw-map  (kbd "C-'") 'term-line-mode)
(define-key term-mode-map (kbd "C-'") 'term-char-mode)

;; Have C-y act as usual in term-mode, to avoid C-' C-y C-'
;; Well the real default would be C-c C-j C-y C-c C-k.
(define-key term-raw-map  (kbd "C-y") 'term-paste)

;; use ido for minibuffer completion
(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)

(and (require 'git-commit nil 'noerror)
     (add-hook 'git-commit-mode-hook 'turn-on-flyspell))

;; Run Mercurial commands through a single external process.
(and (executable-find "hg")
     (setq monky-process-type 'cmdserver))

(and (require 'git-commit nil 'noerror)
     (add-hook 'git-commit-mode-hook 'turn-on-flyspell))

;; Run Mercurial commands through a single external process.
(and (executable-find "hg")
     (setq monky-process-type 'cmdserver))

(provide 'aim-whatever)
