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

;;; OS
;; Config for Linux distros.
(when (eq system-type 'gnu/linux)
  (load! "linux"))
;; Config for MacOs.
(when (eq system-type 'darwin)
  (load! "mac"))
;;; Load files inside the "private-elisp" folder.
(let ((no-dots-regex "^[^\.].*$")
      (private-elisp-fldr (concat doom-private-dir "private-elisp")))
  (dolist
      (file (directory-files private-elisp-fldr t no-dots-regex))
        (load!  file)))

;;; Dashboard
(when (or (display-graphic-p)
           (daemonp))
   (random-banner-image (expand-file-name "splash/" doom-private-dir)))
(add-hook
 'dashboard-mode-hook
 #'(lambda ()
     (perfect-margin-mode -1)))
;;; Scrolling
(setq scroll-conservatively 101)
;;; Org mode
(setq
 org-re-reveal-mousewheel t
 org-edit-src-turn-on-auto-save t
 org-src-preserve-indentation t
 org-use-sub-superscripts t)
(setq neo-theme
      (if (display-graphic-p)
          'icons
        'arrow))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))

;;; Markdown
(custom-set-variables '(livedown-autostart nil) ; automatically open preview when opening markdown files
                      '(livedown-open t) ; automatically open the browser window
                      '(livedown-port 1337)         ; port for livedown server
                      '(livedown-browser 'firefox)) ; browser to use
;; Grip
(setq browse-url-generic-program "firefox" grip-preview-use-webkit nil)
;;; C++
;;Black magic
(setq lsp-clients-clangd-args '("-j=3" "--cross-file-rename" "--background-index"
                                "--completion-style=bundled" "--header-insertion=never"
                                "--limit-results=80"))
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
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

(defun replace-with-indices(to-replace)
  ;; Replace every occurrence of to-replace appending a number
  ;; to the end, starts counting from 0
  (interactive "MInput Regex: ")
  (let ((i 0))
    (while (search-forward-regexp to-replace nil t)
      (let ((replacement
             (concat to-replace
                     (number-to-string i))))
        (when (y-or-n-p "Replace?")
          (replace-match replacement)
          (setq i (1+ i)))))))

;;; PDF
(add-hook 'pdf-view 'auto-revert-mode)

;;; Random Keybindings
(map! :g "C-c C-b" 'eval-buffer)
(map! :g "C-x C-j" 'dired)
(map! :g "C-c f" 'counsel-recentf)
(map! :g "C-x C-b" '+ivy/switch-workspace-buffer)
;; Use C-{k,j} to move in ielm
(map! :map inferior-emacs-lisp-mode-map
      :i "C-k" 'comint-previous-input
      :i "C-j" 'comint-next-input
      :n "C-k" 'comint-previous-input
      :n "C-j" 'comint-next-input)
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
               tab-width 4
               evil-shift-width 4))))


;;; Eshell
(eshell-vterm-mode)
(defalias 'eshell/v 'eshell-vterm-exec-visual)
(setq eshell-up-ignore-case nil)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (company-mode -1)
              (esh-autosuggest-mode)))
(setq eshell-term-name "xterm-256color")
;;; Elixir
(load! "elixir")
;;; Web mode
;; Useful for higlighting tags.
(setq web-mode-enable-current-element-highlight t)
(add-hook 'web-mode-hook
          (lambda ()
            ;; Indent to 2 spaces.
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (setq web-mode-enable-current-column-highlight t)
            (setq evil-shift-width 2)))

;;; Evil
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)
(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil)
(global-evil-matchit-mode 1)

;;; Company mode
(setq company-idle-delay 0.2
      company-show-numbers t
      company-tooltip-limit 10
      company-minimum-prefix-length 4
      company-tooltip-align-annotations t
      company-tooltip-flip-when-above nil
      c-syntactic-indentation t)
(setq company-idle-delay 0.3)
(setq company-show-numbers t)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-align-annotations t)
;; invert the navigation direction if the the completion popup-isearch-match
;; is displayed on top (happens near the bottom of windows)
(setq company-tooltip-flip-when-above t)
;; (setq read-process-output-max (* 1 1024 1024))


;;; Projectile
(after! projectile
  (setq projectile-project-root-files-bottom-up
        (remove
         ".git"
         projectile-project-root-files-bottom-up)))

;;; Prog mode
(add-hook
 'prog-mode-hook
 #'(lambda ()
     ;; To almost center text
     (perfect-margin-mode)
     ;; Rainbow parens
     (rainbow-delimiters-mode)))
;;; Ivy
;; Some fancy icons for ivy
(ivy-rich-mode)
(all-the-icons-ivy-rich-mode)

;; Blamer
(use-package! blamer
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t))))

;;; LSP
(after! lsp-ui
  (setq lsp-ui-doc-enable nil))


;;; SQL
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines)))
;; Helps to see better, a little at least.
(when (display-graphic-p)
  (setq-default line-spacing 4))
  ;; (add-hook 'elixir-mode-hook
  ;;           '(lambda ()
  ;;              (setq line-spacing 8))))



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

;; (add-hook 'c-mode-hook
;;           'smart-semicolon-mode)
;; (add-hook 'solidity-mode-hook
;;           'smart-semicolon-mode)
;; (add-hook 'rust-mode-hook
;;           'smart-semicolon-mode)
(dolist (hook '(c-mode-hook solidity-mode-hook rust-mode-hook))
  (add-hook hook 'smart-semicolon-mode))
(add-to-list 'auto-mode-alist  '("\\.asm\\'" . nasm-mode))
;;(setq tramp-verbose 10)
;;(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(defun sys-call-table ()
  (interactive)
  (async-shell-command "open  https://faculty.nps.edu/cseagle/assembly/sys_call.html"))

(set-lookup-handlers! 'nasm-mode
  :documentation 'x86-lookup)
;; (apheleia-global-mode +1)
(setq highlight-indent-guides-responsive 'top)
(after! visual-regexp
  (require 'visual-regexp-steroids)
  (setq vr/engine 'python
        vr/command-python
        (let ((regex-py-folder "~/.emacs.d/.local/straight/repos/visual-regexp-steroids.el/regexp.py"))
          (if (file-exists-p "/usr/bin/python3")
            (concat "python3 " regex-py-folder)
            (concat "python" regex-py-folder)))))
;; (use-package scrollkeeper
;;   :quelpa (scrollkeeper :fetcher github :repo "alphapapa/scrollkeeper.el")
;;   :general ([remap scroll-up-command] #'scrollkeeper-contents-up
;;             [remap scroll-down-command] #'scrollkeeper-contents-down))

(after! lsp
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                    :major-modes '(c-mode)
                    :remote? t
                    :server-id 'clangd-remote)))


;; (map! :n "C-d" #'scrollkeeper-contents-up
;;       :n "C-u" #'scrollkeeper-contents-down)

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
  (scroll-on-jump-advice-add evil-goto-mark)

  ;; Actions that themselves scroll.
  (scroll-on-jump-with-scroll-advice-add evil-goto-line)
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
