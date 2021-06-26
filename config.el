;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Francisco Krause Arnim"
      user-mail-address "fkrause@gmail.com")
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (when (display-graphic-p)
(setq doom-font (font-spec :family "Fira Code Medium" :size 12)
        doom-variable-pitch-font (font-spec :family "EB Garamond" :size 12))
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-palenight)
;; (setq doom-theme 'doom-zenburn)
;; (setq doom-theme 'sanityinc-tomorrow-blue)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type 'relative)
(setq display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;; Private elisp
(add-to-list 'load-path "~/.doom.d/private-elisp/")
(when (or (display-graphic-p) (daemonp))
  (progn
    (require 'random-banner-image "random-banner.el" )
    (require 'screenshot-svg "screenshot.el")
    (random-banner-image (expand-file-name "splash/" doom-private-dir))))
;;; Scrolling
(setq scroll-conservatively 101)
;; (setq scroll-margin 7)
;;; Indent guides options
;; (if (display-graphic-p)
;;     (setq highlight-indent-guides-method 'bitmap)
;;   (setq highlight-indent-guides-method 'fill))
(setq highlight-indent-guides-method 'character)
;;; Deft folder
(setq deft-directory "/home/francisco/Documentos/deft")
;;; Shell to use
(setq shell-file-name "/usr/bin/bash"
      vterm-shell "/usr/bin/fish")

;;; Autosave for Org Mode's code blocks.
(setq org-edit-src-turn-on-auto-save t
      org-src-preserve-indentation t)
;;; Some personal org options, check my-org-bindings.el for the specifics.
(require 'my-org-bindings)

(add-hook 'org-mode-hook 'map-insert-text-in-math-mode)
(add-hook 'org-mode-hook 'org-fragtog-mode)
(add-hook 'org-mode-hook '(lambda ()
                            (progn
                              (map-insert-environment)
                              (map-insert-math)
                              (setq-local doom-variable-pitch-font (font-spec :family "EB Garamond" :size 12)))))
(add-hook 'org-mode-hook '(lambda ()
                            (setq org-pretty-entities t)))
(add-hook 'org-mode-hook 'org-superstar-mode)
(add-hook 'org-mode-hook 'my-custom-entities-for-org)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(with-eval-after-load 'evil
  (scroll-on-jump-advice-add evil-undo)
  (scroll-on-jump-advice-add evil-redo)
  (scroll-on-jump-advice-add evil-jump-item)
  (scroll-on-jump-advice-add evil-jump-forward)
  (scroll-on-jump-advice-add evil-jump-backward)
  (scroll-on-jump-advice-add evil-ex-search-next)
  (scroll-on-jump-advice-add evil-ex-search-previous)
  (scroll-on-jump-advice-add evil-forward-paragraph)
  (scroll-on-jump-advice-add evil-backward-paragraph)

  ;; Actions that themselves scroll.
  (scroll-on-jump-with-scroll-advice-add evil-scroll-down)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-up)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-center)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-top)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-bottom))

(with-eval-after-load 'goto-chg
  (scroll-on-jump-advice-add goto-last-change)
  (scroll-on-jump-advice-add goto-last-change-reverse))

(global-set-key (kbd "<C-M-next>") (scroll-on-jump-interactive 'diff-hl-next-hunk))
(global-set-key (kbd "<C-M-prior>") (scroll-on-jump-interactive 'diff-hl-previous-hunk))

(setq scroll-on-jump-use-curve t)

(setq gnus-select-method '( nnimap "imap.gmail.com" ))
(setq gnus-message-archive-group "Gmail]/Sent Mail")

(setq company-idle-delay 0)
(setq httpd-root "/home/francisco/Programación/")
(custom-set-variables
 '(livedown-autostart nil) ; automatically open preview when opening markdown files
 '(livedown-open t)        ; automatically open the browser window
 '(livedown-port 1337)     ; port for livedown server
 '(livedown-browser 'firefox))  ; browser to use
(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))
(telega-mode-line-mode 1)
(add-hook 'telega-load-hook
	  (lambda ()
	    (define-key global-map (kbd "C-c t") telega-prefix-map)))
(setq elfeed-feeds
      '("https://www.clarin.com/rss/lo-ultimo/"))
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(setq org-latex-minted-options '(("breaklines" "true")
                                 ("breakanywhere" "true")))
;;; Centaur tabs
(setq centaur-tabs-set-close-button nil)
;;; Python setup

;; (add-hook! 'python-mode-hook '(lambda ()
;;                                (elpy-enable)
;;                                (elpy-mode)
;;                                ;; Disable annoying documentation popup
;;                                (setq lsp-signature-auto-activate nil)
;;                                (setq flymake nil)
;;                                (setq elpy-modules
;;                                      (delq 'elpy-module-flymake
;;                                            (delq 'elpy-module-highlight-indentation
;;                                                  elpy-modules)))))
;; (my-elpy-binds)
;; (defun my-elpy-binds()
;;         (evil-global-set-key 'global (kbd "C-k") nil)
;;         (evil-define-key 'insert 'elpy (kbd "C-k") 'elpy-nav-forward-block)
;;         (evil-define-key 'insert 'elpy (kbd "C-j") 'elpy-nav-backward-block)
;;         (evil-define-key 'insert 'elpy (kbd "C-h") 'elpy-nav-backward-indent)
;;         (evil-define-key 'insert 'elpy (kbd "C-l") 'elpy-nav-forward-indent))

(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
            sql-mode         ; sqlformat is currently broken
            tex-mode         ; latexindent is broken
            latex-mode
            c++-mode         ; C++ autoindent is not meant for mortals...
            html-mode))

(add-hook! 'web-mode 'my-impatient-mode-server-url)

(defun my-impatient-mode-server-url()
  (interactive)
  (progn
  (kill-new "http://localhost:8080/imp/")))

;;; Ruby
(add-hook 'ruby-mode
          (progn
            (evil-define-key
              '(normal insert)
              ruby-mode-map
              (kbd "C-c C-c")
              'ruby-send-buffer)))
;;; Elixir
;; (add-hook 'elixir-mode
;;           (lsp))
(add-to-list 'exec-path "/home/francisco/elixir-ls/release/language_server.sh")

(setq doom-themes-treemacs-variable-pitch-face nil)
;;; C++
(setq c-default-style "stroustrup")
(setq-default tab-width 4)

;;; Utilites
(defun toggle-lines ()
  (interactive)
  (cond ((eq display-line-numbers 'relative) (setq display-line-numbers 't))
        ((eq display-line-numbers 't) (setq display-line-numbers 'relative))))
(defun start-my-server ()
  (interactive)
  (progn
    (httpd-start)
    (impatient-mode 1)
    (my-impatient-mode-server-url)))

;;; PDF
(add-hook 'pdf-view
          (progn
            (auto-revert-mode)))

;;; Keybindings
(map! :g "C-c C-b" 'eval-buffer)
(map! :g "C-x C-j" 'dired)
(map! :g "C-c f" 'helm-recentf)
(map! :g "C-x C-b" 'helm-buffers-list)
(map! :g "C-s" 'swiper)

;; I want backspace to go up a level, like ivy
(add-hook! 'helm-find-files-after-init-hook
  (map! :map helm-find-files-map
        "<DEL>" #'helm-find-files-up-one-level))
;;; Lisp
(add-hook! 'lisp-mode-hook
           #'evil-cleverparens-mode)
(add-hook! 'emacs-lisp-mode
          #'evil-cleverparens-mode)
(add-hook! 'lisp-mode-hook
           #'(lambda ()
               (add-hook! 'after-save-hook #'evil-cleverparens-mode)))

(add-hook 'doom-dashboard
          #'(lambda ()
              (split-window-right)
              (other-window)
              (fireplace)))
