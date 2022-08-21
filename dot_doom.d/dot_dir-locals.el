;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

;;
((org-mode . ((eval . (progn (setq org-latex-listings 'minted)
                             (setq org-latex-minted-options
                                   '(("frame" "lines")
                                     ("fontsize" "\\footnotesize")
                                     ("tabsize" "2")
                                     ("breaklines" "true")
                                     ("breakanywhere" "true") ;; break anywhere, no just on spaces
                                     ("style" "default")
                                     ("bgcolor" "MyBGGray") ;; color defined in the template
                                     ("linenos" "true")))))
              (geiser-scheme-implementation . 'guile)
              (org-export-in-background . nil))))
