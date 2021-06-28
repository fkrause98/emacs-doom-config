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
(setq shell-file-name "/usr/bin/bash"
      vterm-shell "/usr/bin/fish")
(setq doom-font (font-spec :family "Fira Code Medium" :size 12)
        doom-variable-pitch-font (font-spec :family "EB Garamond" :size 12))

;;; For when I like to do some web programming
(setq httpd-root "~/Programación/")
(add-hook! 'web-mode 'my-impatient-mode-server-url)
(defun start-my-server ()
  (interactive)
  (progn
    (httpd-start)
    (impatient-mode 1)
    (my-impatient-mode-server-url)))
(defun my-impatient-mode-server-url()
  (interactive)
  (progn
  (kill-new "http://localhost:8080/imp/")))
;;; Believe it or not, emacs has a mode for telegram
(telega-mode-line-mode 1)
(add-hook 'telega-load-hook
          #'(lambda ()
            (define-key global-map (kbd "C-c t") telega-prefix-map)))

;;; Elixir
(add-to-list 'exec-path "~/elixir-ls/release")

;;; Image for the init dashboard
(when (or (display-graphic-p) (daemonp))
  (progn
    (require 'random-banner-image)
    (require 'screenshot-svg)
    (random-banner-image (expand-file-name "splash/" doom-private-dir))))

(setq highlight-indent-guides-method 'character)
