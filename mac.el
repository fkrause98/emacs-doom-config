;;; linux.el -*- lexical-binding: t; -*-
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
;;; Shell to use
(setq shell-file-name "/bin/zsh"
      vterm-shell "/usr/local/bin/fish")
;;; Elixir
(add-to-list 'exec-path "~/elixir-ls")
;;;
