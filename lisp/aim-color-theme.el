(and (require 'color-theme nil 'noerror)
     (progn
       (color-theme-initialize)
       (and (require 'color-theme-solarized nil 'noerror)
	    (color-theme-solarized-dark))))

(provide 'aim-color-theme)
