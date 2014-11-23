(deftheme aim
  "Created 2014-10-17.")

(custom-theme-set-variables
 'aim
 '(menu-bar-mode nil)
 '(custom-safe-themes (quote ("56cb99174fad69feba8edd6663c592e77d77332fb4c4bb5be058ef459a426463" default))))

(custom-theme-set-faces
 'aim
 '(cursor ((t (:background "green"))))
 '(isearch ((t (:background "yellow"))))
 '(fringe ((t (:background "grey20"))))
 '(hl-line ((t (:inherit highlight))))
 '(mode-line ((t (:background "grey15" :foreground "green" :inverse-video nil :box nil))))
 '(default ((t (:inherit nil :stipple nil :background "#181818" :foreground "#efefef" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 104 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

(provide-theme 'aim)
