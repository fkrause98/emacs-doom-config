;;; mac.el -*- lexical-binding: t; -*-
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (when (display-graphic-p)
;;; Use Fira Code if available
;; (when (member "Fira Code" (font-family-list))
;;               (setq doom-font "Fira Code"))
;;; Shell to use
(setq shell-file-name "/bin/zsh"
      vterm-shell "/usr/local/bin/zsh")
;;; Elixir
(add-to-list 'exec-path "~/elixir-ls")
(add-hook 'elixir-mode-hook
        '(lambda () (setq-local company-minimum-prefix-length 3)))
;;; Modifiers
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)
(setq mac-pass-command-to-system nil)
(exec-path-from-shell-initialize)
(unless (display-graphic-p)
  (global-blamer-mode 1))
