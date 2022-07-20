;;; ../emacs-doom-config/private-elisp/tree-sitter.el -*- lexical-binding: t; -*-

;;; TEMPORARY UNTIL DOOM UPSTREAM ADDS TREE-SITTER
;; Treesitter
;; Compile grammars instead of fetching pre-compiled binaries
;; as they currently don't work with Apple's M1 chip
(add-to-list 'load-path "/Users/fran/elisp-tree-sitter/core")
(add-to-list 'load-path "/Users/fran/elisp-tree-sitter/lisp")
(add-to-list 'load-path "/Users/fran/tree-sitter-langs")
(setq tree-sitter-load-path '("/Users/fran/elisp-tree-sitter/langs"))
(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-debug)
(require 'tree-sitter-query)
(require 'tree-sitter-langs)
(defun funcs//tree-sitter-has-lang  (mode)
  (assoc mode tree-sitter-major-mode-language-alist))
(setq tree-sitter-blacklist '('python-mode))
(add-hook 'prog-mode-hook
          #'(lambda ()
             (when (and
                    (funcs//tree-sitter-has-lang major-mode)
                    (not (member major-mode tree-sitter-blacklist)))
               (tree-sitter-hl-mode))))

(dolist (hook '(c-mode-hook solidity-mode-hook rust-mode-hook))
  (add-hook hook 'smart-semicolon-mode))
