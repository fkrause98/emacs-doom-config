;;; ../emacs-doom-config/elixir.el -*- lexical-binding: t; -*-
(defun check-for-elixir-ls ()
    (if (file-exists-p elixir-ls-folder)
        (add-to-list 'exec-path "~/elixir-ls")
      (warn (format "Missing Elixir language server folder in: %s" elixir-ls-folder))))
(check-for-elixir-ls)

;; To run tests with seed 0
;; (setq alchemist-mix-test-default-options '("--seed 0" "--trace"))

;; Add elixir to doom's autoformat on save languages
(setq +format-on-save-enabled-modes
      '(t elixir-mode))
;; Disable for reasons I don't remember
(setq alchemist-mix-test-default-options nil)

;; Alternate delay for company mode.
;; Indent first line for snippets, can be annoying if
;; set to nil, so I enable it.
(add-hook 'elixir-mode-hook
          #'(lambda()
              (setq company-idle-delay 0.2
                    yas-also-auto-indent-first-line t)))

;; Show which function I'm visiting in the modeline
(add-hook 'elixir-mode-hook 'which-function-mode)

;; Workaround to enable running credo after lsp
(defvar-local my/flycheck-local-cache nil)
(defun my/flycheck-checker-get (fn checker property)
  (or (alist-get property (alist-get checker my/flycheck-local-cache))
      (funcall fn checker property)))
(advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)
(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'elixir-mode)
              (setq my/flycheck-local-cache '((lsp . ((next-checkers . (elixir-credo)))))))))
