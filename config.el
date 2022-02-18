;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Francisco Krause Arnim" user-mail-address "fkrause@gmail.com")
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-palenight)

(setq doom-themes-enable-bold t doom-themes-enable-italic t)
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory
      (if (file-exists-p "~/Documents")
          "~/Documents/org/"
          "~/Documentos/org/"))


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
  (load! "mac")
  (setq doom-font "Fira Code")
  (unless (boundp 'server-running-p)
    (server-start)))
;;; Private elisp
(add-load-path! "private-elisp")
;;; Image for the init dashboard
(when (or (display-graphic-p)
          (daemonp))
  (progn (load! "private-elisp/random-banner")
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
(setq org-edit-src-turn-on-auto-save t org-src-preserve-indentation t org-use-sub-superscripts t)
(add-hook 'org-mode-hook
          #'(lambda ()
              (company-mode -1)))
;;; Some personal org options, check my-org-bindings.el for the specifics.
(require 'my-org-bindings)
(require 'my-org-mode-hooks)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;;; Scrolling animations when jumping around
;; (with-eval-after-load 'evil (scroll-on-jump-advice-add evil-undo)
;;                       (scroll-on-jump-advice-add evil-redo)
;;                       (scroll-on-jump-advice-add evil-jump-item)
;;                       (scroll-on-jump-advice-add evil-jump-forward)
;;                       (scroll-on-jump-advice-add evil-jump-backward)
;;                       (scroll-on-jump-advice-add evil-ex-search-next)
;;                       (scroll-on-jump-advice-add evil-ex-search-previous)
;;                       (scroll-on-jump-advice-add evil-forward-paragraph)
;;                       (scroll-on-jump-advice-add evil-backward-paragraph)
;;                       ;; Actions that themselves scroll.
;;                       (scroll-on-jump-with-scroll-advice-add evil-scroll-down)
;;                       (scroll-on-jump-with-scroll-advice-add evil-scroll-up)
;;                       (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-center)
;;                       (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-top)
;;                       (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-bottom))
;; (with-eval-after-load 'goto-chg (scroll-on-jump-advice-add goto-last-change)
;;                       (scroll-on-jump-advice-add goto-last-change-reverse))

;; (global-set-key (kbd "<C-M-next>")
;;                 (scroll-on-jump-interactive 'diff-hl-next-hunk))
;; (global-set-key (kbd "<C-M-prior>")
;;                 (scroll-on-jump-interactive 'diff-hl-previous-hunk))

;; (setq scroll-on-jump-use-curve t)


(custom-set-variables '(livedown-autostart nil) ; automatically open preview when opening markdown files
                      '(livedown-open t) ; automatically open the browser window
                      '(livedown-port 1337)         ; port for livedown server
                      '(livedown-browser 'firefox)) ; browser to use
;;; Black magic for C++
(setq lsp-clients-clangd-args '("-j=3" "--cross-file-rename" "--background-index"
                                "--completion-style=bundled" "--header-insertion=never"
                                "--limit-results=80"))
(setq org-latex-listings 'minted org-latex-packages-alist '(("" "minted")) org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                                                                                   "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                                                                                   "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(setq org-latex-minted-options '(("breaklines" "true")
                                 ("breakanywhere" "true")))
(setq +format-on-save-enabled-modes '(not emacs-lisp-mode ; elisp's mechanisms are good enough
                                          sql-mode ; sqlformat is currently broken
                                          tex-mode ; latexindent is broken
                                          latex-mode c++-mode ; C++ autoindent is not meant for mortals...
                                          html-mode))
;;; Treemacs
(setq doom-themes-treemacs-variable-pitch-face nil)

;;; Utilites
(defun toggle-lines ()
  (interactive)
  (cond ((eq display-line-numbers 'relative)
         (setq display-line-numbers 't))
        ((eq display-line-numbers 't)
         (setq display-line-numbers 'relative))))

;;; PDF
(add-hook 'pdf-view #'(progn (auto-revert-mode)))

;;; Helm
(after! helm (progn (helm-autoresize-mode)
                    (setq helm-swoop-speed-or-color t helm-split-window-inside-p t
                          helm-autoresize-min-height 40)))
;;; Keybindings
(map! :g "C-c C-b" 'eval-buffer)
(map! :g "C-x C-j" 'dired)
(map! :g "C-c f" 'helm-recentf)
(map! :g "C-x C-b" 'helm-buffers-list)
;; I want backspace to go up a level if using Helm, like ivy
(add-hook! 'helm-find-files-after-init-hook (map! :map helm-find-files-map
                                                  "<DEL>" #'helm-find-files-up-one-level))
;;; Ruby
(add-hook 'ruby-mode (progn
                       (evil-define-key
                         '(normal insert) ruby-mode-map (kbd "C-c C-c")
                              'ruby-send-buffer)))
;;; C++
(setq c-default-style "stroustrup")
(add-hook 'c++-mode-hook
          (lambda ()
            (progn
              (setq-local
               company-minimum-prefix-length 4
               tab-width 4 e
               vil-shift-width 4))))


;;; Elisp / Lisp
(load! "private-elisp/lisp-hooks")
(add-hook 'emacs-lisp-mode-hook
          'evil-cleverparens-mode)
;;; Eshell
(eshell-vterm-mode)
(defalias 'eshell/v 'eshell-vterm-exec-visual)
(setq eshell-up-ignore-case nil)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (company-mode -1)
              (esh-autosuggest-mode)))
;;; Elixir
;; (setq alchemist-mix-test-default-options '("--seed 0" "--trace"))
(setq +format-on-save-enabled-modes
      '(t elixir-mode))
(setq alchemist-mix-test-default-options nil)
(add-hook 'elixir-mode-hook
          #'(lambda()
              (setq company-idle-delay 0.2)))
(add-hook 'elixir-mode-hook 'which-function-mode)
(setq yas-also-auto-indent-first-line t)
;;; Web mode
(setq web-mode-enable-current-element-highlight t)

;;; Evil
;; (modify-syntax-entry ?_ "w")
(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil)
(global-evil-matchit-mode 1)
;; (setq evil-insert-state-map (make-sparse-keymap))
;; (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
;; (define-key evil-insert-state-map (kbd "C-o") 'evil)


;;; Company mode
(setq company-idle-delay 0.2 company-show-numbers t company-tooltip-limit 10
      company-minimum-prefix-length 4 company-tooltip-align-annotations t
      company-tooltip-flip-when-above nil c-syntactic-indentation t)
;; (after! comp--all-builtin-types)
;;; Python
;; (when (fboundp 'elpy-enable)
;; (after! python
;;   (set-company-backend! 'python-mode 'elpy-company-backend))
;;   (progn (elpy-enable)
;;          (add-hook 'python-mode-hook
;;                    (lambda () 'elpy-mode))))
;; (after! python
;;   (set-company-backend! 'python-mode 'company-anaconda))
(setq read-process-output-max (* 1 1024 1024))

;;; Grip

(setq browse-url-generic-program "firefox" grip-preview-use-webkit nil)

;; (global-tree-sitter-mode)
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)


(after! projectile
  (setq projectile-project-root-files-bottom-up (remove ".git"
                                                        projectile-project-root-files-bottom-up)))

(setq eshell-term-name "xterm-256color")

(setq company-idle-delay 0.3)
(setq company-show-numbers t)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-align-annotations t)
;; invert the navigation direction if the the completion popup-isearch-match
;; is displayed on top (happens near the bottom of windows)
(setq company-tooltip-flip-when-above t)

(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2)
            (setq evil-shift-width 2)))
;; (defun funcs//tree-sitter-has-lang  (mode)
;;   (assoc mode tree-sitter-major-mode-language-alist))

(add-hook 'prog-mode-hook
          #'(lambda ()
             ;; (when (funcs//tree-sitter-has-lang major-mode)
             ;;   (tree-sitter-hl-mode))
             (rainbow-delimiters-mode)))
(ivy-rich-mode)
(all-the-icons-ivy-rich-mode)

;;; Term mode
(add-hook 'term-mode-hook 'my-term-mode-hook)
(defun my-term-mode-hook ()
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20611
  (setq bidi-paragraph-direction 'left-to-right))


(use-package blamer
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))


(after! lsp-ui
  (setq lsp-ui-doc-enable nil))


(add-hook
 'dashboard-mode-hook
 #'(lambda ()
     (perfect-margin-mode -1)))
(defun neq (x y)
  (not (eq x y)))
(add-hook
 'prog-mode-hook
 #'(lambda ()
     (perfect-margin-mode)))
;; (perfect-margin-mode)
