;; -*- lexical-binding: t -*-

(setq abbrev-file-name "/dev/null")
(setq-default abbrev-mode nil)
(setq ad-redefinition-action 'accept)
(setq package-enable-at-startup nil)

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, and then reset it later
;; using a hook.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Keep a ref to the actual file-name-handler
(defvar default-file-name-handler-alist file-name-handler-alist)

;; Set the file-name-handler to nil (because regexing is cpu intensive)
(setq file-name-handler-alist nil)

;; Reset file-name-handler-alist after initialization
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)
	    (blink-cursor-mode -1)	;indicator that all is good
	    (setq gc-cons-threshold 200000000 ;200MB
		  gc-cons-percentage 0.1
		  file-name-handler-alist default-file-name-handler-alist)))

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-check-for-modifications
      '(check-on-save find-when-checking))

;;(setq straight-check-for-modifications '(watch-files find-when-checking))

(setq straight-use-package-by-default t)
(setq-default straight-vc-git-default-clone-depth 'full)

(straight-use-package 'use-package)

;; Set default font
;; (set-face-attribute 'default nil
;;                     :family "Source Code Pro"
;;                     :height 140
;;                     :width 'normal)

(fset 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; https://github.com/hrs/dotfiles/blob/master/emacs/.emacs.d/configuration.org
;; thanks man!

(setq hrs/default-font "DejaVu Sans Mono")
(setq hrs/default-font-size 14)
(setq hrs/current-font-size hrs/default-font-size)
(setq hrs/font-change-increment 1.2)

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
  "Decrease current font size by a factor of
`hrs/font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq hrs/current-font-size
	(max 1
	     (floor (/ hrs/current-font-size hrs/font-change-increment))))
  (hrs/set-font-size))

(define-key global-map (kbd "C-)") 'hrs/reset-font-size)
(define-key global-map (kbd "C-+") 'hrs/increase-font-size)
(define-key global-map (kbd "C--") 'hrs/decrease-font-size)

;; (hrs/reset-font-size)

;; (progn
;;   (add-to-list 'default-frame-alist '(undecorated . nil))
;;   (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;;   (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; If this code is being evaluated by emacs --daemon, ensure that each
;; subsequent frame is themed appropriately.
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque."
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun get-frame-name (&optional frame)
  (interactive)
  "Return the string that names FRAME (a frame).  Default is selected frame."
  (unless frame (setq frame (selected-frame)))
  (if (framep frame)
      (cdr (assq 'name (frame-parameters frame)))
    (error "Function `get-frame-name': Argument not a frame: `%s'" frame)))

;; from https://nicolas.petton.fr/blog/emacs-dark-window-decoration.html
(defun set-selected-frame-dark-window-decoration ()
  (interactive)
  (let ((frame-name (get-frame-name (selected-frame))))
    (call-process-shell-command (concat "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"dark\" -name \"" frame-name "\""))))

(and window-system
     (set-selected-frame-dark-window-decoration))

;; (defun hrs/apply-theme ()
;;   (interactive)
;;   ;;(load-theme 'almost-mono-black t)
;;   (if (window-system)
;;       (set-selected-frame-dark-window-decoration))
;;   (transparency 100))

;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;;	      (lambda (frame)
;;		(with-selected-frame frame (hrs/apply-theme))))
;;   (hrs/apply-theme))

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; When saving files, set execute permission if #! is in first line.
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message (getenv "USER"))
(setq initial-scratch-message nil)

;; Turn off 3d mode line
;; (set-face-attribute 'mode-line nil :box nil)

(setq vc-follow-symlinks t
      inhibit-startup-screen t
      ring-bell-function #'ignore
      mouse-yank-at-point t)

(use-package esup)

(use-package cmake-mode)

(use-package ag
  :config
  (setq ag-highlight-search t
	ag-reuse-buffers t)
  (add-hook 'ag-mode-hook 'wgrep-ag-setup))

(use-package ag
  :custom
  (ag-highligh-search t)
  (ag-reuse-buffers t)
  (ag-reuse-window t)
  :bind
  ("M-s a" . ag-project)
  :config
  (use-package wgrep-ag))

(use-package wgrep
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(use-package wgrep-ag
  :config
  (setq wgrep-auto-save-buffer t))

(use-package magit
  :bind ("C-c i" . magit-status)
  :commands magit-status
  :config
  (progn
    (global-magit-file-mode -1)
    (setq magit-refresh-status-buffer nil
	  magit-auto-revert-mode t
	  magit-diff-arguments (quote ("--function-context" "--no-ext-diff" "--stat"))
	  magit-pull-arguments nil)))

(use-package markdown-mode
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode))

(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands))
  :config
  (smex-initialize))

(use-package cmake-mode
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)))

(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :config
  (progn
    (bind-key "C-c C-j" 'aj-toggle-fold yaml-mode-map)))

(use-package browse-url)

;; (use-package ibuffer
;;   :config
;;   (progn
;;     (setq ibuffer-saved-filter-groups
;;	  (quote (("default"
;;		   ("dired" (mode . dired-mode))
;;		   ("perl" (mode . cperl-mode))
;;		   ("Go" (mode . go-mode))
;;		   ("erc" (mode . erc-mode))
;;		   ("planner" (or
;;			       (name . "^\\*Calendar\\*$")
;;			       (name . "^diary$")
;;			       (mode . muse-mode)))
;;		   ("emacs" (or
;;			     (name . "^\\*scratch\\*$")
;;			     (name . "^\\*Messages\\*$")))
;;		   ("gnus" (or
;;			    (mode . message-mode)
;;			    (mode . bbdb-mode)
;;			    (mode . mail-mode)
;;			    (mode . gnus-group-mode)
;;			    (mode . gnus-summary-mode)
;;			    (mode . gnus-article-mode)
;;			    (name . "^\\.bbdb$")
;;			    (name . "^\\.newsrc-dribble")))))))
;;     (add-hook 'ibuffer-mode-hook
;;	      (lambda ()
;;		(ibuffer-switch-to-saved-filter-groups "default")))
;;     (bind-key "[::space::]" 'ibuffer-visit-buffer ibuffer-mode-map)))

;;(use-package dockerfile-mode)

(use-package tramp
  :config
  (progn
    (set-default 'tramp-default-method "ssh")
    ;; /sudo:root@10.17.20.215:/var/log/juju/
    ;; /ssh:ubuntu@10.11.20.101|sudo:10.11.20.101:/var/log/juju/machine-0.log
    ;; /ssh:ubuntu@10.17.20.215|sudo:10.17.20.215:/var/log/juju/
    ;; /ssh:ubuntu@10.17.20.215|sudo:10.17.20.215:/
    ;; /ssh:ubuntu@10.17.20.215|sudo:10.17.20.215:/
    ;; http://irreal.org/blog/?p=895
    ;; (add-to-list 'tramp-default-proxies-alist
    ;;		 '(nil "\\`root\\'" "/ssh:%h:"))
    ;; (add-to-list 'tramp-default-proxies-alist
    ;;		 '((regexp-quote (system-name)) nil nil))
    ;;    (set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
    ;; I added the ".*" for NixOS
    (setq tramp-shell-prompt-pattern
	  "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*")
    (add-to-list 'tramp-connection-properties
		 (list ".*" "locale" "LC_ALL=C"))
    (setq tramp-ssh-controlmaster-options
	  (concat
	   "-o ControlPath=tramp.%%r@%%h:%%p "
	   "-o ControlMaster=auto "
	   "-o ControlPersist=yes"))))

(use-package guide-key
  :config
  (setq guide-key/guide-key-sequence '("C-c p" "C-x 4")))

;;(add-hook 'lisp-mode #'(complete-mode 1))

(defadvice kill-line (before check-position activate)
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
	     (just-one-space 0)
	     (backward-char 1))))

(use-package python-mode
  :config (progn
	    (set-variable 'py-indent-offset 4)
	    (set-variable 'indent-tabs-mode nil)))

(use-package git-commit
  :hook (git-commit-setup-hook . git-commit-turn-on-flyspell))

(add-hook 'git-commit-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook 'langtool-check nil 'make-it-local)))

(use-package copy-as-format
  :config
  (setq copy-as-format-default "slack")
  :bind
  (:map mode-specific-map
	:prefix-map copy-as-format-prefix-map
	:prefix "f"
	("f" . copy-as-format)
	("a" . copy-as-format-asciidoc)
	("b" . copy-as-format-bitbucket)
	("d" . copy-as-format-disqus)
	("g" . copy-as-format-github)
	("l" . copy-as-format-gitlab)
	("c" . copy-as-format-hipchat)
	("h" . copy-as-format-html)
	("j" . copy-as-format-jira)
	("m" . copy-as-format-markdown)
	("w" . copy-as-format-mediawiki)
	("o" . copy-as-format-org-mode)
	("p" . copy-as-format-pod)
	("r" . copy-as-format-rst)
	("s" . copy-as-format-slack)))

;; Provides only the command “restart-emacs”.
(use-package restart-emacs
  :commands restart-emacs)

(use-package git-gutter
  :config
  ;; (global-git-gutter-mode t)
  (setq git-gutter:modified-sign " "
	git-gutter:added-sign " "
	git-gutter:deleted-sign " "
	git-gutter:lighter " GG")
  (set-face-background 'git-gutter:modified "DarkGoldenrod4")
  (set-face-foreground 'git-gutter:added "dark green")
  (set-face-foreground 'git-gutter:deleted "dark red")
  ;; (global-set-key (kbd "C-x C-g") 'git-gutter)
  ;; (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
  ;; Jump to next/previous hunk
  (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x n") 'git-gutter:next-hunk))

(use-package hippie-exp
  :straight nil
  :config
  (setq hippie-expand-try-functions-list
	'(try-expand-dabbrev
	  try-expand-dabbrev-from-kill
	  try-expand-dabbrev-all-buffers
	  try-complete-file-name-partially
	  try-complete-file-name
	  try-expand-all-abbrevs
	  try-expand-list
	  try-expand-line
	  try-complete-lisp-symbol-partially
	  try-complete-lisp-symbol))
  :bind ("M-/" . hippie-expand))

(use-package ffap
  :config
  ;; Don't ping things that look like domain names.
  (setq ffap-machine-p-known 'reject)
  (ffap-bindings))

(use-package cc-mode
  :mode (("\\.h\\'"    . c-mode)
	 ("\\.c\\'"    . c-mode)
	 ("\\.cpp\\'"  . c++-mode)
	 ("\\.mm\\'"   . objc-mode)
	 ("\\.java\\'" . java-mode)))

;;;; Builtin Emacs packages

(use-package uniquify
  :straight nil
  :config
  (progn
    (setq uniquify-buffer-name-style 'reverse)
    (setq uniquify-separator "|")
    (setq uniquify-after-kill-buffer-p t)
    (setq uniquify-ignore-buffers-re "^\\*")))

(use-package dired-x
  :straight nil
  :config
  (global-set-key (kbd "C-x C-j") 'dired-jump)
  (setq-default dired-omit-mode t))

;;;; Personal stuff

(defvar aim/is-darwin (eq system-type 'darwin))
(defvar aim/is-linux (eq system-type 'gnu/linux))

;; (setq vc-ignore-dir-regexp
;;       (format "\\(%s\\)\\|\\(%s\\)"
;;	      vc-ignore-dir-regexp
;;	      tramp-file-name-regexp))

(setq sentence-end-double-space nil)

(put 'scroll-left 'disabled nil)

(use-package alert)

(use-package compile
  :init
  (defun compile-finish-hook (buf why)
    (display-buffer buf)
    (alert why :buffer buf))
  (defun ansi-color-compilation-buf ()
    (when (eq major-mode 'compilation-mode)
      (interactive)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook ((compilation-finish-functions . compile-finish-hook)
	 (compilation-filter-hook . ansi-color-compilation-buf)))

;; (require 'desktop)

;; (setq desktop-buffers-not-to-save
;;       (concat "\\("
;;	      "\\.go\\"
;;	      "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
;;	      "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
;;	      "\\)$"))

;; (add-to-list 'desktop-modes-not-to-save 'dired-mode)
;; (add-to-list 'desktop-modes-not-to-save 'Info-mode)
;; (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
;; (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

;;(desktop-save-mode 1)

;; (defun aim/desktop-save ()
;;   (interactive)
;;   ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
;;   (unless (desktop-save-mode-off)
;;     (if (eq (desktop-owner) (emacs-pid))
;;	(desktop-save desktop-dirname))))

;; (add-hook 'auto-save-hook 'aim/desktop-save)

(use-package recentf
  :config
  (progn
    (setq recentf-auto-cleanup 'never
	  recentf-max-saved-items 100)
    (recentf-mode 1)))

(add-to-list 'recentf-exclude
	     (expand-file-name "~/.emacs.d/elpa"))

(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
	  (progn
	    (goto-char start)
	    (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
	(replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(defun visit-dir-as-root (hostname)
  (interactive "shost: ")
  (let ((filename (format "/ssh:%s|sudo:%s:/" hostname hostname)))
    (find-file filename)))

(defun visit-dir (hostname)
  (interactive "shost: ")
  (let ((filename (format "/ssh:%s:~" hostname hostname)))
    (find-file filename)))

(defalias 'ttl 'toggle-truncate-lines)

;; (unless (fboundp 'xref-push-marker-stack)
;;   (defalias 'xref-pop-marker-stack 'pop-tag-mark)
;;   (defun xref-push-marker-stack (&optional m)
;;     "Add point to the marker stack."
;;     (ring-insert find-tag-marker-ring (or m (point-marker)))))

(defadvice gdb-inferior-filter
    (around gdb-inferior-filter-without-stealing)
  (with-current-buffer (gdb-get-buffer-create 'gdb-inferior-io)
    (comint-output-filter proc string)))

(ad-activate 'gdb-inferior-filter)

(use-package elide-head
  :config
  (setq elide-head-headers-to-hide
	(quote
	 (("is free software[: ;] you can redistribute it" . "\\(Boston, MA 0211\\(1-1307\\|0-1301\\), USA\\|If not, see <http://www\\.gnu\\.org/licenses/>\\)\\.")
	  ("The Regents of the University of California\\.  All rights reserved\\." . "SUCH DAMAGE\\.")
	  ("Permission is hereby granted, free of charge" . "authorization from the X Consortium\\.")
	  ("Copyright .* The Kubernetes Authors." . "limitations under the License."))))
  (add-hook 'go-mode-hook 'elide-head)
  (add-hook 'c-mode-common-hook 'elide-head))

(use-package helm)
(use-package helm-company)

(use-package helm-ls-git
  :requires
  helm
  :config
  (global-set-key (kbd "C-c C-l") 'helm-ls-git-ls))

(use-package terraform-mode)

(use-package pass)

(use-package gist)

(use-package epkg)

(use-package direnv
  :config
  (progn
    (direnv-mode)
    (setq direnv-always-show-summary nil)))

(use-package notmuch
  :config
  (setq notmuch-hello-thousands-separator ",")
  (setq notmuch-search-oldest-first nil))

(defun atomic-chrome-server-running-p ()
  "Returns t if the atomic-chrome server is currently running, otherwise nil."
  (let ((retval nil))
    (condition-case ex
	(progn
	  (delete-process
	   (make-network-process
	    :name "atomic-client-test" :host "localhost"
	    :noquery t :service "64292"))
	  (setq retval t))
      ('error nil))
    retval))

(use-package atomic-chrome
  :config
  (when (not (atomic-chrome-server-running-p))
    (atomic-chrome-start-server)))

;; (use-package smart-shift
;;   :ensure t
;;   :config
;;   (global-smart-shift-mode t))

(use-package jinja2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode)))

;;; https://stackoverflow.com/questions/3139970/open-a-file-at-line-with-filenameline-syntax
;;;
;; Open files and goto lines like we see from g++ etc. i.e. file:line#
;; (to-do "make `find-file-line-number' work for emacsclient as well")
;; (to-do "make `find-file-line-number' check if the file exists")
(defadvice find-file (around find-file-line-number
			     (filename &optional wildcards)
			     activate)
  "Turn files like file.cpp:14 into file.cpp and going to the 14-th line."
  (save-match-data
    (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
	   (line-number (and matched
			     (match-string 2 filename)
			     (string-to-number (match-string 2 filename))))
	   (filename (if matched (match-string 1 filename) filename)))
      ad-do-it
      (when line-number
	;; goto-line is for interactive use
	(goto-char (point-min))
	(forward-line (1- line-number))))))

(defvar ffap-file-at-point-line-number nil
  "Variable to hold line number from the last `ffap-file-at-point' call.")

(defadvice ffap-file-at-point (after ffap-store-line-number activate)
  "Search `ffap-string-at-point' for a line number pattern and
save it in `ffap-file-at-point-line-number' variable."
  (let* ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
	 (name
	  (or (condition-case nil
		  (and (not (string-match "//" string)) ; foo.com://bar
		       (substitute-in-file-name string))
		(error nil))
	      string))
	 (line-number-string
	  (and (string-match ":[0-9]+" name)
	       (substring name (1+ (match-beginning 0)) (match-end 0))))
	 (line-number
	  (and line-number-string
	       (string-to-number line-number-string))))
    (if (and line-number (> line-number 0))
	(setq ffap-file-at-point-line-number line-number)
      (setq ffap-file-at-point-line-number nil))))

(defadvice find-file-at-point (after ffap-goto-line-number activate)
  "If `ffap-file-at-point-line-number' is non-nil goto this line."
  (when ffap-file-at-point-line-number
    (with-no-warnings
      (goto-line ffap-file-at-point-line-number))
    (setq ffap-file-at-point-line-number nil)))

(use-package ediff
  :config
  ;; Pressing d will copy both A and B to buffer C.
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
		     (concat
		      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
		      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer)))))

(defun add-d-to-ediff-mode-map ()
  (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))

(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(defun open-var-log-messages (hostname)
  (interactive "shost: ")
  (let ((filename (format "/ssh:%s|sudo:%s:/var/log/messages" hostname hostname)))
    (find-file filename)))

(use-package itail)

(defun itail-var-log-messages (hostname)
  (interactive "shost: ")
  (let ((filename (format "/ssh:%s|sudo:%s:/var/log/messages" hostname hostname)))
    (itail filename)))

(use-package pinentry
  :config
  (setq epg-pinentry-mode 'loopback))

(use-package adoc-mode)

(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

;; (defun aim/frame-colours-unspecified (frame)
;;   (interactive)
;;   (let ((fg (face-attribute 'default :foreground frame))
;;	(bg (face-attribute 'default :background frame)))
;;     (and (equal fg "unspecified-fg") (equal bg "unspecified-bg"))))

;; (defun aim/on-frame-open (frame)
;;   (interactive)
;;   (message "before FRAME %s" (frame-parameters frame))
;;   (message "before FRAME background-mode %s" (frame-parameter frame 'background-mode))
;;   (message "before FRAME foreground-mode %s" (frame-parameter frame 'foreground-mode))
;;   (if (not (display-graphic-p frame))
;;       (progn
;;	(if (and (equal (frame-parameter frame 'background-mode) 'light)
;;		 (aim/frame-colours-unspecified frame))
;;	    (progn
;;	      (set-frame-parameter frame 'background-color "#000000")
;;	      (set-frame-parameter frame 'foreground-color "#FFFFFF"))
;;	  (if (and (equal (frame-parameter frame 'background-mode) nil)
;;		   (aim/frame-colours-unspecified frame))
;;	      (progn
;;		(set-frame-parameter frame 'background-color "#FFFFFF")
;;		(set-frame-parameter frame 'foreground-color "#000000"))))))
;;   (message "after FRAME %s" (frame-parameters frame)))

;; (defun aim/maybe-frob-colours (frame)
;;   (interactive)
;;   (and (string-match "xterm-" (getenv "TERM"))
;;        (aim/on-frame-open (selected-frame))))

;; (defun kill-dired-buffers ()
;;   (interactive)
;;   (mapc (lambda (buffer) (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
;;			   (kill-buffer buffer)))
;;	(buffer-list)))

;; This package is easiest way to open particular link on
;; github/gitlab/bitbucket/stash/git.savannah.gnu.org from Emacs
(use-package browse-at-remote)

(use-package git-timemachine)

(defun browse-url-chromote (url &rest ignore)
  "Browse URL using browse-url-chromote."
  (interactive "sURL: ")
  (shell-command (expand-file-name "~/bin/browse-url-chromote ") url))

;;(setq browse-url-browser-function 'browse-url-chromote)

(when (file-exists-p (expand-file-name "~/emacs-libvterm/vterm-module.so"))
  (add-to-list 'load-path (expand-file-name "~/emacs-libvterm"))
  (require 'vterm))

(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (setq nix-indent-function #'nix-indent-line)
  :bind
  (:map nix-mode-map
	("C-c C-j" . aj-toggle-fold)))

(use-package deadgrep
  :config
  (global-set-key (kbd "<f5>") #'deadgrep))

(defun aim/light-mode ()
  (interactive)
  (setq frame-background-mode 'light)
  (mapc 'frame-set-background-mode (list (selected-frame))))

(defun aim/dark-mode ()
  (interactive)
  (setq frame-background-mode 'dark)
  (mapc 'frame-set-background-mode (list (selected-frame))))

(use-package auto-compile
  :config (auto-compile-on-load-mode))

;; (use-package projectile
;;   :bind
;;   ("C-c v" . 'projectile-ag)
;;   ("C-c v" . 'projectile-deadgrep)
;;   :config
;;   (setq projectile-completion-system 'ido)
;;   (setq projectile-switch-project-action 'projectile-dired)
;;   (setq projectile-require-project-root nil))

;; https://tuhdo.github.io/helm-projectile.html
;; (use-package helm-projectile)
;; (projectile-global-mode)
;; (setq projectile-completion-system 'helm)
;; (helm-projectile-on)
;; (setq projectile-switch-project-action 'helm-projectile-find-file)
;; (setq projectile-switch-project-action 'helm-projectile)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(defvar emacsql-sqlite-executable
  (expand-file-name (concat user-emacs-directory "/emacsql-sqlite")))

(use-package multi-term
  :config
  (global-set-key (kbd "C-c t") 'multi-term)
  (setq multi-term-program-switches "--login")
  (put 'magit-clean 'disabled nil))

;;Load auto-complete
;; (use-package go-autocomplete)
;; (ac-config-default)
;; (require 'auto-complete-config)
;; (require 'go-autocomplete)

;; suggest things when company has nothing to say
(setq-default tab-always-indent 'complete)

;; turn off annoying tooltips
(use-package company
  :config
  (setq company-frontends nil)
  (setq company-idle-delay 0
	company-tooltip-limit 20
	company-minimum-prefix-length 1
	company-echo-delay 0
	company-require-match nil
	company-tooltip-align-annotations t ; Align annotation to the right side.
	company-auto-complete nil)
  :hook (prog-mode . company-mode)
  ;;(global-company-mode 1)
  )

(use-package flx)
(use-package company-flx)

;; Sort candidates using completion history
(use-package company-statistics)

(with-eval-after-load 'company
  (company-flx-mode +1))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(with-eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-select-previous)
     (define-key company-active-map (kbd "<tab>") 'company-select-previous)
     (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
     (define-key company-active-map (kbd "<backtab>") 'company-select-previous)))

(setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
	company-preview-frontend
	company-echo-metadata-frontend))

;;(setq company-auto-complete 'never)
;;(company-tng-configure-default)

(use-package go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save)
  (remove-hook 'go-mode 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  :bind
  (:map go-mode-map
	("M-RET" . compile)
	("C-M-x" . aim/run-go-buffer)
	;;("M-." . godef-jump))
	))

;; (use-package go-mode
;;   :mode "\\.go\\'"
;;   :custom (gofmt-command "goimports")
;;   :bind (:map go-mode-map
;;	      ("C-c C-n" . go-run)
;;	      ("C-c ."   . go-test-current-test)
;;	      ("C-c f"   . go-test-current-file)
;;	      ("C-c a"   . go-test-current-project))
;;   :config
;;   (add-hook 'before-save-hook #'gofmt-before-save)
;;   (setq gofmt-command "goimports")
;;   (use-package gotest)
;;   (use-package go-tag
;;     :config (setq go-tag-args (list "-transform" "camelcase"))))

(use-package flycheck)

(defun aim/setup-ac-complete nil
  (interactive)
  (use-package go-autocomplete)
  (require 'go-autocomplete)
  (require 'auto-complete-config)
  (ac-config-default)
  (define-key ac-complete-mode-map "\t" 'ac-expand)
  (define-key ac-complete-mode-map "\r" 'ac-complete)
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous)
  ;;(set-default 'ac-sources '(ac-source-abbrev ac-source-words-in-buffer))
  )

(defun aim/setup-company-complete nil
  (interactive)
  (use-package company-go
    :after company
    :ensure company-go
    :config (add-to-list 'company-backends 'company-go))
  (add-hook 'go-mode-hook
	    (lambda ()
	      (set (make-local-variable 'company-backends) '(company-go))
	      (company-mode))))

(setq aim/prefer-ac-complete nil)

(if aim/prefer-ac-complete
    (aim/setup-ac-complete)
  (aim/setup-company-complete))

(use-package smartparens
  :config
  (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  :diminish smartparens-mode)

(use-package flx)

;;(use-package flycheck
;;  :init
;;  (global-flycheck-mode))

;; (use-package counsel
;;   :config
;;   (global-set-key (kbd "M-x") 'counsel-M-x)
;;   ;;(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;   (global-set-key (kbd "C-c g") 'counsel-git)
;;   (global-set-key (kbd "C-c j") 'counsel-git-grep)
;;   (global-set-key (kbd "C-c a") 'counsel-ag)
;;   (global-set-key (kbd "C-c r") 'counsel-rg)
;;   (global-set-key (kbd "C-x l") 'counsel-locate)
;;   (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;; (use-package ivy
;;   :config
;;   (setq ivy-use-selectable-prompt t
;;	ivy-use-virtual-buffers t       ; Enable bookmarks and recentf
;;	ivy-height 10
;;	ivy-count-format "(%d/%d) "
;;	ivy-on-del-error-function nil
;;	ivy-initial-inputs-alist nil
;;	ivy-count-format " "
;;	ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

;; (use-package ivy
;;   :diminish (ivy-mode . "")
;;   :bind
;;   (:map ivy-mode-map
;;	("C-'" . ivy-avy))
;;   :config
;;   (ivy-mode 1)
;;   ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
;;   (setq ivy-use-virtual-buffers t)
;;   ;; number of result lines to display
;;   (setq ivy-height 10)
;;   ;; does not count candidates
;;   (setq ivy-count-format "")
;;   ;; no regexp by default
;;   (setq ivy-initial-inputs-alist nil)
;;   ;; configure regexp engine.
;;   (setq ivy-re-builders-alist
;;	;; allow input not in order
;;         '((t . ivy--regex-ignore-order))))

;; (use-package ivy-posframe
;;   :config
;;   (setq ivy-posframe-height-alist '((swiper . 10)
;;                                     (t      . 5)))
;;   (setq ivy-posframe-display-functions-alist
;;	'((swiper          . nil)
;;           (complete-symbol . ivy-posframe-display-at-point)
;;           (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
;;           (t               . ivy-posframe-display)))
;;   (ivy-posframe-mode 1))

;; (use-package swiper
;;   :config
;;   (global-set-key (kbd "C-s") 'swiper))

;; (use-package yasnippet
;;   :init
;;   (yas-global-mode 1))

;; (use-package yasnippet-snippets)
;; ;;(use-package yasnippet-classic-snippets)

;;(use-package projectile
;;  :bind (:map projectile-mode-map
;;              ("C-c p" . 'projectile-command-map))
;;  :config
;;  (projectile-mode +1))

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;; (setq lsp-keymap-prefix "s-l")
;; (use-package lsp-mode
;;   :commands lsp
;;   :custom
;;   (lsp-auto-guess-root nil)
;;   ;;(lsp-prefer-flymake nil) ; Use flycheck instead of flymake
;;   :config
;;   (setq lsp-auto-guess-root t)
;;   (setq lsp-inhibit-message t)
;;   ;;(setq lsp-message-project-root-warning t)
;;   :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
;;   :hook ((go-mode c-mode c++-mode) . lsp))

;; (use-package lsp-ui
;;   :after lsp-mode
;;   :diminish
;;   :commands lsp-ui-mode
;;   ;; :custom-face
;;   ;; (lsp-ui-doc-background ((t (:background nil))))
;;   ;; (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
;;   :bind (:map lsp-ui-mode-map
;;               ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
;;               ([remap xref-find-references] . lsp-ui-peek-find-references)
;;               ("C-c u" . lsp-ui-imenu))
;;   :custom
;;   (lsp-ui-doc-enable t)
;;   (lsp-ui-doc-header t)
;;   (lsp-ui-doc-include-signature t)
;;   (lsp-ui-doc-position 'top)
;;   (lsp-ui-doc-border (face-foreground 'default))
;;   (lsp-ui-sideline-enable nil)
;;   (lsp-ui-sideline-ignore-duplicate t)
;;   (lsp-ui-sideline-show-code-actions nil)
;;   :config
;;   ;; Use lsp-ui-doc-webkit only in GUI
;;   (setq lsp-ui-doc-use-webkit t)
;;   ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
;;   ;; https://github.com/emacs-lsp/lsp-ui/issues/243
;;   (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
;;     (setq mode-line-format nil)))

;; (use-package company-lsp
;;   :commands company-lsp
;;   :custom
;;   (company-lsp-cache-candidates t) ;; auto, t(always using a cache), or nil
;;   (company-lsp-async t)
;;   (company-lsp-enable-snippet t)
;;   (company-lsp-enable-recompletion t)
;;   :config
;;   (push 'company-lsp company-backends))

(use-package k8s-mode
  :hook (k8s-mode . yas-minor-mode))

;; Making it easier to discover Emacs key presses.
(use-package which-key
  :diminish
  :config (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 3.50))

;; Get org-headers to look pretty! E.g., * → ⊙, ** ↦ ◯, *** ↦ ★
;; https://github.com/emacsorphanage/org-bullets
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; (use-package ssh-config-mode)

(defun aim/tramp-borked ()
  (interactive)
  (tramp-cleanup-all-connections)
  (tramp-cleanup-all-buffers))

(use-package docker-tramp)

(use-package eshell-bookmark
  :after eshell
  :config
  (add-hook 'eshell-mode-hook #'eshell-bookmark-setup))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

(setq org-confirm-babel-evaluate nil)

(use-package modus-operandi-theme)
(use-package modus-vivendi-theme)

;;(load-theme 'modus-operandi t)          ; Light theme
;;(load-theme 'modus-vivendi t)

(use-package heaven-and-hell
  :init
  (setq heaven-and-hell-theme-type 'dark) ;; Omit to use light by default
  (setq heaven-and-hell-themes
	'((light . modus-operandi)
	  (dark . modus-vivendi))) ;; Themes can be the list: (dark . (tsdh-dark wombat))
  (setq heaven-and-hell-load-theme-no-confirm t)
  ;;:hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
	 ("<f6>" . heaven-and-hell-toggle-theme)))

;; (setq lsp-gopls-staticcheck t)
;; (setq lsp-eldoc-render-all t)
;; (setq lsp-gopls-complete-unimported t)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((before-save . lsp-format-buffer)
	 (before-save . lsp-organize-imports))
  :bind (("C-c d" . lsp-describe-thing-at-point)
	 ("C-c e n" . flymake-goto-next-error)
	 ("C-c e p" . flymake-goto-prev-error)
	 ("C-c e r" . lsp-find-references)
	 ("C-c e R" . lsp-rename)
	 ("C-c e i" . lsp-find-implementation)
	 ("C-c e t" . lsp-find-type-definition))
  :hook ((go-mode . lsp-deferred)
	 (lisp-mode . lsp-deferred)))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(remove-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; (add-hook 'before-save-hook 'gofmt-before-save)

;; Prefer go-mode's gofmt over LSP sluggishness
(remove-hook 'before-save-hook 'lsp-organize-imports)
(remove-hook 'before-save-hook 'lsp-format-buffer)

;;Optional - provides fancier overlays.
;; (use-package lsp-ui)

(use-package company-lsp
  :commands company-lsp)

;;Optional - provides snippet support.
;; (use-package yasnippet
;;   :commands yas-minor-mode
;;   :hook (go-mode . yas-minor-mode))

(setq lsp-ui-doc-enable nil
      lsp-ui-peek-enable nil
      lsp-ui-sideline-enable nil
      lsp-ui-imenu-enable nil
      lsp-ui-flycheck-enable nil)

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
	 ("M-g j" . dumb-jump-go)
	 ("M-g b" . dumb-jump-back)
	 ("M-g i" . dumb-jump-go-prompt)
	 ("M-g x" . dumb-jump-go-prefer-external)
	 ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'helm
		dumb-jump-debug nil
		dumb-jump-prefer-searcher 'rg)
  :hook (prog-mode . dumb-jump-mode))

(defun my-dumb-jump-mode-hook ()
  (define-key dumb-jump-mode-map (kbd "C-M-g") nil)
  (define-key dumb-jump-mode-map (kbd "C-M-p") nil)
  (define-key dumb-jump-mode-map (kbd "C-M-q") nil))

(eval-after-load "dumb-jump"
  (add-hook 'go-mode 'my-dumb-jump-mode-hook))

(use-package langtool
  :bind
  (:map git-commit-mode-map
	("C-x `" . langtool-correct-buffer))
  :config
  (setq langtool-http-server-host "localhost"
	langtool-http-server-port 8081
	langtool-default-language "en-GB"))

;; (use-package ispell
;;   :config
;;   ;; Setting ‘flyspell-issue-message-flag’ to nil, as printing
;;   ;; messages for every word (when checking the entire buffer) causes
;;   ;; an enormous slowdown.
;;   (setq flyspell-issue-message-flag nil))

;; (use-package flyspell
;;   :custom
;;   (ispell-program-name "aspell")
;;   ;; Default dictionary. To change do M-x ispell-change-dictionary RET.
;;   ;; (aspell-dictionary "en_GB")
;;   (aspell-program-name "aspell")
;;   ;; (ispell-dictionary "en_GB")
;;   (ispell-program-name "aspell")
;;   :config
;;   (define-key flyspell-mode-map [down-mouse-3] 'flyspell-correct-word)
;;   (add-hook 'org-mode-hook 'flyspell-mode)
;;   (add-hook 'TeX-mode-hook 'flyspell-mode)
;;   (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode))

(defun aim/run-go-buffer ()
  (interactive)
  (shell-command (format "go run %s" (buffer-file-name (current-buffer)))))

(defun aim/isearch-face-settings ()
  (interactive)
  (set-face-foreground 'isearch "black")
  (set-face-background 'isearch "yellow")
  (set-face-foreground 'lazy-highlight "black")
  (set-face-background 'lazy-highlight "orange"))

(defun aim/load-file-if-exists (filename)
  (interactive)
  (and (file-exists-p filename)
       (load-file filename)))

(defun aim/reverse-video nil
  "*Invert default face"
  (interactive)
  (let* ((fg (face-foreground 'default))
	 (bg (face-background 'default)))
    (set-face-foreground 'default bg)
    (set-face-background 'default fg)))

(defun aim/fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defun aim/revert-buffer-now ()
  "revert-(current-buffer) asking no questions"
  (interactive)
  (revert-buffer nil t))

(defun aim/check-frame-colours ()
  (interactive)
  (and window-system
       (if (string-equal (downcase (face-foreground 'default)) "black")
	   (aim/reverse-video))))

(defun aim/set-global-keybindings nil
  (interactive)
  (mapcar #'(lambda (x)
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
	    )))

(when aim/is-linux
  (global-set-key [f11] 'aim/fullscreen))

(aim/set-global-keybindings)
;; (add-hook 'after-init-hook
;;	  (lambda ()
;;	    (load-theme 'modus-vivendi t)))
;; (electric-indent-mode 1)

(auto-compression-mode)

;;; LSP speedups
;;
;; Increase the amount of data which Emacs reads from the process.
;; Again the emacs default is too low 4k considering that the some of
;; the language server responses are in 800k - 3M range.
(setq read-process-output-max (* 5 (* 1024 1024))) ;; 1mb

;; Optional: use company-capf . Although company-lsp also supports
;; caching lsp-mode’s company-capf does that by default. To achieve
;; that uninstall company-lsp or put these lines in your config:
(setq lsp-prefer-capf t)

;; The buffer *Flymake log* tends to fill up with things like:
;; > Warning [flymake init.el]: Disabling backend flymake-proc-legacy-flymake
;; > because (error Can’t find a suitable init function)
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

(use-package theme-changer
  :config
  (setq calendar-location-name "UK") 
  (setq calendar-latitude 51.5558)
  (setq calendar-longitude 1.7797)
  (change-theme 'modus-operandi 'modus-vivendi))

;;(use-package vterm)

(defun my-minibuffer-setup ()
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 1.25))))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)

;; Easily adjust the font size in all frames
;;
;; Enable the global minor mode `default-text-scale-mode' to bind
;; C-M-= and C-M-- to `default-text-scale-increase' and
;; `default-text-scale-decrease' respectively. Alternatively, bind
;; those commands directly in another map.
(use-package default-text-scale
  :config
  (default-text-scale-mode))

(use-package spell-fu
  :hook
  ((markdown-mode org-mode text-mode) . spell-fu-mode)
  :init
  (setq spell-fu-faces-exclude '(org-meta-line org-link org-code)))

(unless (fboundp 'json-serialize)
  (error "you don't have a json-serialize builtin-in function"))
