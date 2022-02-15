(setq useless-minor-modes '(" AC"   ;; First must have a space. :-(
                            "GitGutter"
                            "Undo-Tree"
                            "Fly"
                            "ARev"
                            "Abbrev"
                            "Fill"
                            "ColorIds"
                            "FIC"
                            "FlyC.*"))
(setq sml/hidden-modes (mapconcat 'identity useless-minor-modes "\\| *"))
