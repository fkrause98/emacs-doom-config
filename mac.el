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
(setq doom-font
      (font-spec
       :family
       (cond
        ((member "JetBrains Mono" (font-family-list)) "JetBrains Mono")
        ((member "Fira Code" (font-family-list)) "Fira Code")
        (t "Monaco"))
       :size 11))
;;; Shell to use
(setq shell-file-name "fish"
      vterm-shell "/opt/homebrew/bin/fish")
;;; Elixir
(setq elixir-ls-folder "~/elixir-ls")


;; Company can be a bit annoying whit its
;; suggestions.
(add-hook 'elixir-mode-hook
        '(lambda () (setq-local company-minimum-prefix-length 3)))
;;; Modifiers
;; One thing that mac does well is
;; the command key, let's use it.
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)
(setq mac-pass-command-to-system t)
;; (exec-path-from-shell-initialize)
;; Start server if not running
(unless (boundp 'server-running-p)
    (server-start))
