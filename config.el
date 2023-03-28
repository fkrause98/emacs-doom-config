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
(after! magit
 (defun th/magit--with-difftastic (buffer command)
   "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
   (let ((process-environment
          (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                        (number-to-string (frame-width)))
                process-environment)))
     ;; Clear the result buffer (we might regenerate a diff, e.g., for
     ;; the current changes in our working directory).
     (with-current-buffer buffer
       (setq buffer-read-only nil)
       (erase-buffer))
     ;; Now spawn a process calling the git COMMAND.
     (make-process
      :name (buffer-name buffer)
      :buffer buffer
      :command command
      ;; Don't query for running processes when emacs is quit.
      :noquery t
      ;; Show the result buffer once the process has finished.
      :sentinel (lambda (proc event)
                  (when (eq (process-status proc) 'exit)
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      (ansi-color-apply-on-region (point-min) (point-max))
                      (setq buffer-read-only t)
                      (view-mode)
                      (end-of-line)
                      ;; difftastic diffs are usually 2-column side-by-side,
                      ;; so ensure our window is wide enough.
                      (let ((width (current-column)))
                        (while (zerop (forward-line 1))
                          (end-of-line)
                          (setq width (max (current-column) width)))
                        ;; Add column size of fringes
                        (setq width (+ width
                                       (fringe-columns 'left)
                                       (fringe-columns 'right)))
                        (goto-char (point-min))
                        (pop-to-buffer
                         (current-buffer)
                         `(;; If the buffer is that wide that splitting the frame in
                           ;; two side-by-side windows would result in less than
                           ;; 80 columns left, ensure it's shown at the bottom.
                           ,(when (> 80 (- (frame-width) width))
                              #'display-buffer-at-bottom)
                           (window-width
                            . ,(min width (frame-width))))))))))))
 (defun th/magit-show-with-difftastic (rev)
   "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
   (interactive
    (list (or
           ;; If REV is given, just use it.
           (when (boundp 'rev) rev)
           ;; If not invoked with prefix arg, try to guess the REV from
           ;; point's position.
           (and (not current-prefix-arg)
                (or (magit-thing-at-point 'git-revision t)
                    (magit-branch-or-commit-at-point)))
           ;; Otherwise, query the user.
           (magit-read-branch-or-commit "Revision"))))
   (if (not rev)
       (error "No revision specified")
     (th/magit--with-difftastic
      (get-buffer-create (concat "*git show difftastic " rev "*"))
      (list "git" "--no-pager" "show" "--ext-diff" rev))))
 (defun th/magit-diff-with-difftastic (arg)
   "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
   (interactive
    (list (or
           ;; If RANGE is given, just use it.
           (when (boundp 'range) range)
           ;; If prefix arg is given, query the user.
           (and current-prefix-arg
                (magit-diff-read-range-or-commit "Range"))
           ;; Otherwise, auto-guess based on position of point, e.g., based on
           ;; if we are in the Staged or Unstaged section.
           (pcase (magit-diff--dwim)
             ('unmerged (error "unmerged is not yet implemented"))
             ('unstaged nil)
             ('staged "--cached")
             (`(stash . ,value) (error "stash is not yet implemented"))
             (`(commit . ,value) (format "%s^..%s" value value))
             ((and range (pred stringp)) range)
             (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
   (let ((name (concat "*git diff difftastic"
                       (if arg (concat " " arg) "")
                       "*")))
     (th/magit--with-difftastic
      (get-buffer-create name)
      `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))
 (transient-define-prefix th/magit-aux-commands ()
   "My personal auxiliary magit commands."
   ["Auxiliary commands"
    ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
    ("s" "Difftastic Show" th/magit-show-with-difftastic)])


 (transient-append-suffix 'magit-dispatch "!"
   '("#" "My Magit Cmds" th/magit-aux-commands))

 (define-key magit-status-mode-map (kbd "#") #'th/magit-aux-commands))
