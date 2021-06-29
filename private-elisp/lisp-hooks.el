;;; private-elisp/lisp-hooks.el -*- lexical-binding: t; -*-
(add-hook! 'lisp-mode-hook
           'evil-cleverparens-mode)
(add-hook! 'emacs-lisp-mode
           #'(lambda()
             (evil-cleverparens-mode)))
(provide 'my-lisp-hooks)
