(setq ocamlformat-location (getenv "OCAMLFORMAT_LOCATION"))
(when (> (length ocamlformat-location) 0)
 (add-to-list 'load-path (concat ocamlformat-location "/share/emacs/site-lisp"))
 (require 'ocamlformat)
 (add-hook 'tuareg-mode-hook
           (lambda () (add-hook 'before-save-hook 'ocamlformat-before-save))))
