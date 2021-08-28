;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Francisco Krause Arnim"
      user-mail-address "fkrause@gmail.com")
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-palenight)
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


(when (eq system-type 'gnu/linux)
  (load! "linux"))
(when (eq system-type 'darwin)
  (load! "mac"))
;;; Private elisp
(add-load-path! "private-elisp")
;;; Image for the init dashboard
(when (or (display-graphic-p) (daemonp))
  (progn
    (load! "private-elisp/random-banner")
    (load! "private-elisp/screenshot")
    (random-banner-image (expand-file-name "splash/" doom-private-dir))))
;;; Scrolling
(setq scroll-conservatively 101)
;;; Indent guides options
;; (if (display-graphic-p)
;;     (setq highlight-indent-guides-method 'bitmap)
;;   (setq highlight-indent-guides-method 'fill))
;;; Org mode
(setq org-re-reveal-mousewheel t)
(setq org-edit-src-turn-on-auto-save t
      org-src-preserve-indentation t
      org-use-sub-superscripts t)
;;; Some personal org options, check my-org-bindings.el for the specifics.
(require 'my-org-bindings)
(require 'my-org-mode-hooks)
(setq neo-theme (if (display-graphic-p)
                    'icons
                  'arrow))

;;; Scrolling animations when jumping aroun
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


(custom-set-variables
 '(livedown-autostart nil) ; automatically open preview when opening markdown files
 '(livedown-open t)        ; automatically open the browser window
 '(livedown-port 1337)     ; port for livedown server
 '(livedown-browser 'firefox))  ; browser to use
;;; Black magic for C++
(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(setq org-latex-minted-options '(("breaklines" "true")
                                 ("breakanywhere" "true")))
(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
            sql-mode         ; sqlformat is currently broken
            tex-mode         ; latexindent is broken
            latex-mode
            c++-mode         ; C++ autoindent is not meant for mortals...
            html-mode))
;;; Treemacs
(setq doom-themes-treemacs-variable-pitch-face nil)

;;; Utilites
(defun toggle-lines ()
  (interactive)
  (cond ((eq display-line-numbers 'relative) (setq display-line-numbers 't))
        ((eq display-line-numbers 't) (setq display-line-numbers 'relative))))

;;; PDF
(add-hook 'pdf-view
          #'(progn
              (auto-revert-mode)))

;;; Keybindings
(map! :g "C-c C-b" 'eval-buffer)
(map! :g "C-x C-j" 'dired)
(map! :g "C-c f" 'helm-recentf)
(map! :g "C-x C-b" 'helm-buffers-list)
(map! :g "C-s" 'swiper)

;; I want backspace to go up a level if using Helm, like ivy
(add-hook! 'helm-find-files-after-init-hook
  (map! :map helm-find-files-map
        "<DEL>" #'helm-find-files-up-one-level))
;;; Ruby
(add-hook 'ruby-mode
          (progn
            (evil-define-key
              '(normal insert)
              ruby-mode-map
              (kbd "C-c C-c")
              'ruby-send-buffer)))
;;; C++
(setq c-default-style "stroustrup")
(setq-default tab-width 4)

(map! "M-x" 'helm-M-x)

;;; Elisp / Lisp
(load! "private-elisp/lisp-hooks")
;;; Elixir
(setq alchemist-mix-test-default-options '("--seed 0" "--trace"))
;;; Web mode
(setq web-mode-enable-current-element-highlight t)

;;; Evil
(modify-syntax-entry ?_ "w")
(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil)
(global-evil-matchit-mode 1)

(setq company-idle-delay 0.5)
(setq company-show-numbers t)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-align-annotations t)
;; invert the navigation direction if the the completion popup-isearch-match
;; is displayed on top (happens near the bottom of windows)
(setq company-tooltip-flip-when-above t)
