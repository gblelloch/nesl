(if (> (string-to-int (substring emacs-version 0 2)) 18)
    (load "nesl19.el")
  (load "nesl18.el"))
