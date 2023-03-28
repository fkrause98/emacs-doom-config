;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Francisco Krause Arnim" user-mail-address "fkrause@gmail.com")
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-palenight)

(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
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
(setq-local doom-config-dir (expand-file-name "~/.doom.d"))
;;; Load files inside the "private-elisp" folder.
(let ((no-dots-regex "^[^\.].*$")
      (private-elisp-fldr (concat doom-config-dir "/private-elisp")))
  (dolist
      (file (directory-files private-elisp-fldr t no-dots-regex))
        (load!  file)))

;;; Dashboard
(when (or (display-graphic-p)
           (daemonp))
   (random-banner-image (concat doom-config-dir "/splash" )))
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
;;; Treemacs
(setq doom-themes-treemacs-variable-pitch-face nil)

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

;; Helps to see better, a little at least.
(when (display-graphic-p)
  (setq-default line-spacing 4))
(setq highlight-indent-guides-responsive 'top)



(dolist (hook '(c-mode-hook solidity-mode-hook rust-mode-hook))
  (add-hook hook 'tree-sitter-hl-mode))
(dolist (hook '(c-mode-hook solidity-mode-hook rust-mode-hook))
  (add-hook hook 'smart-semicolon-mode))

(setq chatgpt-shell-openai-key
      (plist-get (car (auth-source-search :host "openai.com"))
                 :secret))
;; (setq gptel-api-key
;;       chatgpt-shell-openai-key)
