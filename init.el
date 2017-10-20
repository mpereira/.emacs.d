(require 'package)

(package-initialize)

(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; * Stuff I keep forgetting:
;; ** Upgrade packages: M-x list-packages (C-z to get out of evil mode) U x
;; ** Paste into the minibuffer: C-y
;; ** org mode file links to search patterns can't start with open parens:
;;    https://www.mail-archive.com/emacs-orgmode@gnu.org/msg112359.html
;; ** EXPRESSION can be used only once per `org-agenda-prefix-format'.
;; ** Emulate C-u (universal-argument)
;; *** For raw prefix arg (interactive "P")
;;     (let ((current-prefix-arg '(4)))
;;       (call-interactively 'some-func))
;; *** Otherwise
;;     (let ((current-prefix-arg 4))
;;       (call-interactively 'some-func))

;; Uncomment this to terminate init.el loading early.
;; (with-current-buffer " *load*"
;;   (goto-char (point-max)))

;; Tramp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tramp)

;; Disable version control on tramp buffers to avoid freezes.
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; Server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'server)
(unless (server-running-p)
  (server-start))

;; Color theme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://emacsthemes.com
;; http://daylerees.github.io/
;; http://raebear.net/comp/emacscolors.html

;; doom-themes dependency.
;; Make sure to run `M-x all-the-icons-install-fonts` on new installs.
(use-package all-the-icons
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-tomorrow-night t)

  ;; Enable flashing mode-line on errors.
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme
  (doom-themes-neotree-config)

  ;; Correct (and improve) org-mode's native fontification.
  (doom-themes-org-config))

;; mode-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "magit"
  (defconst mpereira/mode-line-projectile
    '(:eval
      (let ((face 'modeline-buffer-id))
        (when (projectile-project-name)
          (concat
           (propertize " " 'face face)
           (propertize (format "%s" (projectile-project-name)) 'face face)
           (propertize " " 'face face))))))

  (defconst mpereira/mode-line-vc
    '(:eval
      (when (and (stringp vc-mode) (string-match "Git[:-]" vc-mode))
        (let ((branch (replace-regexp-in-string "^ Git[:-]" "" vc-mode))
              (face 'magit-branch-current))
          (concat
           (propertize " " 'face face)
           (propertize (format "%s" branch) 'face face)
           (propertize " " 'face face))))))

  (defun mpereira/shorten-directory (dir max-length)
    "Show up to MAX-LENGTH characters of a directory name DIR."
    (let ((directory-truncation-string (if (char-displayable-p ?…) "…/" ".../"))
          (longname (abbreviate-file-name dir)))
      ;; If it fits, return the string.
      (if (<= (string-width longname) max-length) longname
        ;; If it doesn't, shorten it.
        (let ((path (reverse (split-string longname "/")))
              (output ""))
          (when (and path (equal "" (car path)))
            (setq path (cdr path)))
          (let ((max (- max-length (string-width directory-truncation-string))))
            ;; Concat as many levels as possible, leaving 4 chars for safety.
            (while (and path (<= (string-width (concat (car path) "/" output))
                                 max))
              (setq output (concat (car path) "/" output))
              (setq path (cdr path))))
          ;; If we had to shorten, prepend .../
          (when path
            (setq output (concat directory-truncation-string output)))
          output))))

  (defconst mpereira/mode-line-buffer
    '(:eval
      (let ((modified-or-ro-symbol (cond
                                    ((and buffer-file-name (buffer-modified-p))
                                     "~")
                                    (buffer-read-only ":RO")
                                    (t "")))
            (face 'bold)
            (directory-face 'shadow)
            (modified-symbol-face 'default)
            (directory (if buffer-file-name
                           (mpereira/shorten-directory default-directory 15)
                         "")))
        (concat
         (propertize " " 'face face)
         (propertize (format "%s" directory) 'face directory-face)
         (propertize "%b" 'face face)
         (propertize modified-or-ro-symbol 'face modified-symbol-face)
         (propertize " " 'face face)))))

  (defconst mpereira/mode-line-major-mode
    '(:eval
      (propertize " %m " 'face 'font-lock-comment-face)))

  (defconst mpereira/mode-line-buffer-position
    '(:eval
      (unless eshell-mode
        (propertize " %p %l,%c " 'face 'tooltip))))


  (setq-default mode-line-format (list mpereira/mode-line-projectile
                                       mpereira/mode-line-vc
                                       mpereira/mode-line-buffer
                                       mpereira/mode-line-major-mode
                                       mpereira/mode-line-buffer-position
                                       mode-line-end-spaces)))

;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mpereira/setq-local-show-trailing-whitespace-nil ()
  (interactive)
  (setq-local show-trailing-whitespace nil))

(defun mpereira/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun mpereira/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;; Modified https://www.emacswiki.org/emacs/DescribeThingAtPoint
(defun mpereira/describe-thing-at-point ()
  "Show the documentation of the Elisp function and variable near point.
  This checks in turn:
  -- for a function name where point is
  -- for a keymap name where point is
  -- for a variable name where point is
  -- for a surrounding function call"
  (interactive)
  (let (sym)
    (cond ((setq sym (ignore-errors
                       (with-syntax-table emacs-lisp-mode-syntax-table
                         (save-excursion
                           (or (not (zerop (skip-syntax-backward "_w")))
                               (eq (char-syntax (char-after (point))) ?w)
                               (eq (char-syntax (char-after (point))) ?_)
                               (forward-sexp -1))
                           (skip-chars-forward "`'")
                           (let ((obj (read (current-buffer))))
                             (and (symbolp obj) (fboundp obj) obj))))))
           (describe-function sym))
          ((setq sym (variable-at-point)) (if (keymapp (symbol-value sym))
                                              (describe-keymap sym)
                                            (describe-variable sym)))
          ((setq sym (function-at-point)) (describe-function sym)))))

(require 'thingatpt) ;; for `thing-at-point'.
(defun mpereira/eval-sexp-at-or-surrounding-pt ()
  "Evaluate the sexp following the point, or surrounding the point"
  (interactive)
  (save-excursion
    (forward-char 1)
    (if (search-backward "(" nil t)
        (message "%s" (eval (read-from-whole-string (thing-at-point 'sexp)))))))

(defun mpereira/split-window-below-and-switch ()
  "Split the window horizontally then switch to the new window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun mpereira/split-window-right-and-switch ()
  "Split the window vertically then switch to the new window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun mpereira/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))
    (message "Can only toggle window split for 2 windows")))

(defun mpereira/indent-buffer ()
  "Indents the current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(with-eval-after-load "lispy"
  (defun mpereira/inside-or-at-the-end-of-string ()
    (when (lispy--in-string-p)
      (let* ((p (point))
             (bounds (lispy--bounds-string)))
        (and (not (= p (car bounds)))
             (not (= p (cdr bounds)))))))

  (defun mpereira/backward-sexp (arg)
    "Moves to the beginning of the previous ARG nth sexp."
    (interactive "p")
    (if (mpereira/inside-or-at-the-end-of-string)
        (let ((bounds (lispy--bounds-string)))
          (goto-char (car bounds))
          (backward-sexp (- arg 1)))
      (backward-sexp arg)))

  (defun mpereira/forward-sexp (arg)
    "Moves to the beginning of the next ARG nth sexp. The fact that this doesn't
exist in any structured movement package is mind-boggling to me."
    (interactive "p")
    (when (mpereira/inside-or-at-the-end-of-string)
      (let ((bounds (lispy--bounds-string)))
        (goto-char (- (car bounds) 1))))
    (dotimes (i arg)
      (forward-sexp 1)
      (if (looking-at lispy-right)
          (backward-sexp 1)
        (progn
          (forward-sexp 1)
          (backward-sexp 1))))))

;; Based on
;;
;; https://github.com/bbatsov/persp-projectile/
;; blob/7686633acf44402fa90429759cca6a155e4df2b9/persp-projectile.el#L66
;;
;; and
;;
;; https://github.com/syl20bnr/spacemacs/
;; blob/b7e51d70aa3fb81df2da6dc16d9652a002ba5e6b/
;; layers/%2Bspacemacs/spacemacs-layouts/funcs.el#352
(with-eval-after-load "ivy"
  (with-eval-after-load "projectile"
    (with-eval-after-load "perspective"
      (defun mpereira/ivy-persp-switch-project (arg)
        (interactive "P")
        (ivy-read "Switch to Project Perspective: "
                  (if (projectile-project-p)
                      (cons (abbreviate-file-name (projectile-project-root))
                            (projectile-relevant-known-projects))
                    projectile-known-projects)
                  :action
                  (lambda (project)
                    (let* ((name (file-name-nondirectory
                                  (directory-file-name project)))
                           (persp (gethash name perspectives-hash)))
                      (cond
                       ;; Project-specific perspective already exists.
                       ((and persp (not (equal persp persp-curr)))
                        (persp-switch name))
                       ;; Project-specific perspective doesn't exist.
                       ((not persp)
                        (let ((frame (selected-frame)))
                          (persp-switch name)
                          (projectile-switch-project-by-name project)
                          ;; Clean up if we switched to a new frame. `helm' for
                          ;; one allows finding files in new frames so this is a
                          ;; real possibility.
                          (when (not (equal frame (selected-frame)))
                            (with-selected-frame frame
                              (persp-kill name)))))))))))))

(with-eval-after-load "evil"
  (with-eval-after-load "lispyville"
    (defun mpereira/insert-to-beginning-of-list (arg)
      (interactive "p")
      (lispyville-backward-up-list)
      (evil-forward-char)
      (evil-insert arg))

    (defun mpereira/append-to-end-of-list (arg)
      (interactive "p")
      (lispyville-up-list)
      (evil-insert arg))))

;; Options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'diminish)
(diminish 'auto-revert-mode)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

;; Disable eldoc.
(global-eldoc-mode -1)

;; Break lines automatically when typing.
(auto-fill-mode t)
(diminish 'auto-fill-function)

;; Show line and column numbers in mode line.
(line-number-mode t)
(column-number-mode t)

;; Highlight current line;
(global-hl-line-mode t)

;; Don't append customizations to init.el.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Shh...
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)

;; macOS modifiers.
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq mac-control-modifier 'control)
(setq ns-function-modifier 'hyper)

;; Start scratch buffers in text-mode.
(setq initial-major-mode 'text-mode)

;; Make cursor the width of the character it is under e.g. full width of a TAB.
(setq x-stretch-cursor t)

;; By default Emacs thinks a sentence is a full-stop followed by 2 spaces. Make
;; it a full-stop and 1 space.
(setq sentence-end-double-space nil)

(fset 'yes-or-no-p 'y-or-n-p)

;; Switch to help buffer when it's opened.
(setq help-window-select t)

;; Don't recenter buffer point when point goes outside window.
(setq scroll-conservatively 100)

(require 'linum)
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'linum-mode))

;; Better unique names for similarly-named file buffers.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Remember point position between sessions.
(require 'saveplace)
(save-place-mode t)

;; Don't create companion files.
(setq create-lockfiles nil)

;; Save a bunch of session state stuff.
(require 'savehist)
(setq savehist-additional-variables '(regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (expand-file-name "savehist" user-emacs-directory))
(savehist-mode t)

;; Show trailing whitespace.
(require 'whitespace)
(setq whitespace-style '(face lines-tail trailing))
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'whitespace-mode))
(setq-default show-trailing-whitespace t)
(diminish 'whitespace-mode)

;; 80 columns.
(setq-default fill-column 80)
(setq-default comment-column 80)

;; UTF8 stuff.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Tab first tries to indent the current line, and if the line was already
;; indented, then try to complete the thing at point.
(setq tab-always-indent 'complete)

;; Make it impossible to insert tabs.
(setq-default indent-tabs-mode nil)

(setq-default tab-width 2)

;; Finder's "Open with Emacs" creates a buffer in the existing Emacs frame.
(setq ns-pop-up-frames nil)

(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; sh-mode indentation.
(add-hook 'sh-mode-hook
          (lambda ()
            (setq-local sh-basic-offset 2)
						(setq-local sh-indentation 2)))

(setq mpereira/leader ",")

;; secrets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file "~/.emacs.d/secrets.el")

;; general ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package general
  :ensure t)

;; eshell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'eshell)
(require 'em-dirs) ;; for `eshell/pwd'.

;; Don't display the "Welcome to the Emacs shell" banner.
(setq eshell-banner-message "")

;; TODO: understand the consequences os this.
(setenv "PAGER" "cat")

(setq eshell-scroll-to-bottom-on-input 'all)
(setq eshell-buffer-maximum-lines 20000)
(setq eshell-error-if-no-glob t)
(setq eshell-hist-ignoredups t)
(setq eshell-save-history-on-exit t)
(setq eshell-destroy-buffer-when-process-dies t)
;; `find` and `chmod` behave differently on eshell than unix shells. Prefer unix
;; behavior.
(setq eshell-prefer-lisp-functions nil)

;; Visual commands are commands which require a proper terminal. eshell will run
;; them in a term buffer when you invoke them.
(setq eshell-visual-commands
      '("less" "tmux" "htop" "top" "bash" "zsh" "fish" "glances"))
(setq eshell-visual-subcommands
      '(("git" "log" "l" "diff" "show")))

;; Eshell needs this variable set in addition to the PATH environment variable.
(setq-default eshell-path-env (getenv "PATH"))

(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (eshell-send-input))

(defun mpereira/eshell-clear ()
  (interactive)
  (eshell/clear)
  (eshell-send-input)
  ;; FIXME: this is a hack to scroll cursor to top.
  (set-window-start (selected-window) (point))
  (scroll-down 1))

(defun mpereira/counsel-esh-history ()
  "Browse Eshell history."
  (interactive)
  (setq ivy-completion-beg (eshell-bol))
  (setq ivy-completion-end (point))
  (let ((elements eshell-history-ring)
        (initial-input (buffer-substring-no-properties ivy-completion-beg ivy-completion-end)))
    (ivy-read "Symbol name: "
              (delete-dups
               (when (> (ring-size elements) 0)
                 (ring-elements elements)))
              :action #'ivy-completion-in-region-action
              :initial-input initial-input)))

;; eshell-mode-map needs to be configured in an `eshell-mode-hook'.
;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2016-02/msg01532.html
(defun mpereira/initialize-eshell ()
  (interactive)
  (setq pcomplete-cycle-completions nil)
  (setq-local beacon-mode nil)

  (general-define-key
   :states '(normal visual)
   :keymaps '(eshell-mode-map)
   "C-k" 'eshell-previous-prompt
   "C-j" 'eshell-next-prompt)

  (general-define-key
   :states '(insert)
   :keymaps '(eshell-mode-map)
   ;; TAB here doesn't work for some reason.
   "<tab>" 'completion-at-point
   "C-k" 'eshell-previous-input
   ;; TODO: when on an empty prompt and going up and back down (or down and back
   ;; up), make it so that the prompt is empty again instead of cycling back to
   ;; the first input.
   "C-j" 'eshell-next-input
   "C-/" 'mpereira/counsel-esh-history
   ;; https://github.com/ksonney/spacemacs/commit/297945a45696e235c6983a78acdf05b5f0e015ca
   "C-l" 'mpereira/eshell-clear))

(add-hook 'eshell-mode-hook 'mpereira/initialize-eshell)
(add-hook 'eshell-exit-hook (lambda ()
                              (interactive)
                              (unless (one-window-p)
                                (delete-window))))

(defun mpereira/remote-p ()
  (tramp-tramp-file-p default-directory))

(defun mpereira/remote-user ()
  "Return remote user name."
  (tramp-file-name-user (tramp-dissect-file-name default-directory)))

(defun mpereira/remote-host ()
  "Return remote host."
  ;; `tramp-file-name-real-host' is removed and replaced by
  ;; `tramp-file-name-host' in Emacs 26, see
  ;; https://github.com/kaihaosw/eshell-prompt-extras/issues/18
  (if (fboundp 'tramp-file-name-real-host)
      (tramp-file-name-real-host (tramp-dissect-file-name default-directory))
    (tramp-file-name-host (tramp-dissect-file-name default-directory))))


;; https://www.emacswiki.org/emacs/EshellPrompt
(defun mpereira/fish-path (path)
  "Return a potentially trimmed-down version of the directory PATH, replacing
parent directories with their initial characters to try to get the character
length of PATH (sans directory slashes) down to MAX-LEN."
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (max-len 30)
         (len (+ (1- (length components))
                 (cl-reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str
                        (cond ((= 0 (length (car components))) "/")
                              ((= 1 (length (car components)))
                               (concat (car components) "/"))
                              (t
                               (if (string= "."
                                            (string (elt (car components) 0)))
                                   (concat (substring (car components) 0 2)
                                           "/")
                                 (string (elt (car components) 0) ?/)))))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

(defun mpereira/eshell-prompt ()
  (let ((user-name (if (mpereira/remote-p)
                       (mpereira/remote-user)
                     (user-login-name)))
        (host-name (if (mpereira/remote-p)
                       (mpereira/remote-host)
                     (system-name))))
    (concat
     (propertize user-name 'face '(:foreground "green"))
     " "
     (propertize "at" 'face 'eshell-ls-unreadable)
     " "
     (propertize host-name 'face '(:foreground "cyan"))
     " "
     (propertize "in" 'face 'eshell-ls-unreadable)
     " "
     (propertize (mpereira/fish-path (eshell/pwd)) 'face 'dired-directory)
     "\n"
     (propertize (if (= (user-uid) 0)
                     "#"
                   "$")
                 'face 'eshell-prompt)
     " ")))

;; Unused (for now?)
(setq mpereira/eshell-prompt-string
      (let ((prompt (mpereira/eshell-prompt))
            (inhibit-read-only t))
        (set-text-properties 0 (length prompt) nil prompt)
        prompt))

(setq eshell-prompt-function 'mpereira/eshell-prompt)
(setq eshell-prompt-regexp "^[$#] ")
;; This causes the prompt to not be protected.
;; (setq eshell-highlight-prompt nil)

;; restart-emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package restart-emacs
  :ensure t
  :config
  (setq restart-emacs-restore-frames t))

;; s ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package s
  :ensure t)

;; dash ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dash
  :ensure t)

;; suggest ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package suggest
  :ensure t)

;; minibuffer-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package minibuffer-line
  :ensure t
  :config
  (setq minibuffer-line-format
        '((:eval
           (let ((time-string (format-time-string "%a %b %d %R")))
             (concat
              (propertize (make-string (- (frame-text-cols)
                                          (string-width time-string))
                                       ?\s)
                          'face '(:background "gray13"))
              time-string)))))
  (minibuffer-line-mode t))

;; beacon ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  (setq beacon-size 40))

;; undo-tree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(diminish 'undo-tree-mode)

(dolist (hook '(undo-tree-mode-hook
                undo-tree-visualizer-mode-hook))
  (add-hook hook 'mpereira/setq-local-show-trailing-whitespace-nil))

(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)

;; js ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default js-indent-level 2)

;; mappings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(general-define-key
 "<escape>" 'keyboard-quit)

(general-define-key
 :keymaps '(minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map)
 "<escape>" 'minibuffer-keyboard-quit)

;; FIXME: isn't M-x bound in insert mode in the first place and why doesn't
;; this binding work?
(general-define-key
 :states '(insert)
 "M-x" 'execute-extended-command)

(general-define-key
 :states '(insert)
 "TAB" 'company-complete)

(general-define-key
 :keymaps 'emacs-lisp-mode-map
 :states '(normal)
 "C-]" 'xref-find-definitions-other-window
 "K" 'mpereira/describe-thing-at-point)

(general-define-key
 :keymaps '(global-map)
 :states '(normal visual)
 :prefix mpereira/leader
 :infix "e"
 ":" 'eval-expression)

(general-define-key
 :keymaps '(emacs-lisp-mode-map)
 :states '(normal)
 :prefix mpereira/leader
 :infix "e"
 "e" 'mpereira/eval-sexp-at-or-surrounding-pt
 "(" 'eval-defun
 "E" 'eval-buffer)

(general-define-key
 :keymaps 'emacs-lisp-mode-map
 :states '(visual)
 :prefix mpereira/leader
 :infix "e"
 "e" 'eval-region)

(general-define-key
 "M-F" 'toggle-frame-fullscreen
 "M-+" 'text-scale-increase
 "M--" 'text-scale-decrease)

(eval-after-load 'evil-ex
  '(evil-ex-define-cmd "bD" 'mpereira/delete-file-and-buffer))

(general-define-key
 :states '(normal visual)
 :prefix mpereira/leader
 "," 'evil-buffer
 "u" 'undo-tree-visualize
 "b" 'switch-to-buffer
 "dk" 'describe-key
 "dm" 'describe-mode
 "w" 'save-buffer
 "q" 'evil-quit
 "hs" 'mpereira/split-window-below-and-switch
 "vs" 'mpereira/split-window-right-and-switch
 "hv" 'mpereira/toggle-window-split
 "vh" 'mpereira/toggle-window-split)

;; Return to original cursor position when cancelling search.
(general-define-key
 :keymaps 'isearch-mode-map
 "<escape>" 'isearch-cancel)
(general-define-key
 :keymaps 'evil-ex-search-keymap
 "<escape>" 'minibuffer-keyboard-quit)

;; org-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-directory "~/Dropbox/org/")

(setq org-modules '(org-habit org-info))

(setq org-return-follows-link t)

(setq org-log-done 'time)

;; Show empty line between collapsed trees if they are separated by just 1
;; line break.
(setq org-cycle-separator-lines 1)

;; org-clock stuff.
(setq org-clock-idle-time 15)
(setq org-clock-mode-line-total 'current)
(setq org-clock-in-switch-to-state "DOING")

(setq org-agenda-files (list org-directory))

;; Full screen org-agenda.
(setq org-agenda-window-setup 'only-window)
;; Don't destroy window splits.
(setq org-agenda-restore-windows-after-quit t)
;; Show only the current instance of a repeating timestamp.
(setq org-agenda-repeating-timestamp-show-all nil)
;; Don't look for free-form time string in headline.
(setq org-agenda-search-headline-for-time nil)

(setq org-agenda-tags-column -110)

(setq org-attach-auto-tag "attachment")

(defun mpereira/org-current-subtree-state-p (state)
  (string= state (org-get-todo-state)))

(defun mpereira/org-up-heading-top-level ()
  "Move to the top level heading."
  (while (not (= 1 (org-outline-level)))
    (org-up-heading-safe)))

(defun mpereira/org-skip-all-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (mpereira/org-current-state-p "TODO"))
    (setq should-skip-entry t))
  (save-excursion
    (while (and (not should-skip-entry) (org-goto-sibling t))
      (when (mpereira/org-current-state-p "TODO"))
      (setq should-skip-entry t)))
  (when should-skip-entry
    (or (outline-next-heading)
        (goto-char (point-max)))))

(defun mpereira/org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun mpereira/org-skip-subtree-unless-habit ()
  "Skip an agenda entry unless it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        nil
      subtree-end)))

(defun mpereira/org-skip-inbox ()
  "Skip agenda entries coming from the inbox."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-get-category) "inbox")
        subtree-end
      nil)))

(defun mpereira/org-skip-someday-projects-subheadings ()
  "Skip agenda entries under a project with state \"SOMEDAY\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (mpereira/org-up-heading-top-level)
    (if (mpereira/org-current-subtree-state-p "SOMEDAY")
        subtree-end
      nil)))

(defun mpereira/org-entry-at-point-get (property)
  (org-entry-get (point) property))

(defun mpereira/deadline-or-scheduled ()
  (interactive)
  (cond
   ((mpereira/org-entry-at-point-get "DEADLINE") "DEADLINE")
   ((mpereira/org-entry-at-point-get "SCHEDULED") "SCHEDULED")))

(defun mpereira/org-entry-get-timestamp-at-point ()
  (interactive)
  (let ((timestamp (or (mpereira/org-entry-at-point-get "DEADLINE")
                       (mpereira/org-entry-at-point-get "SCHEDULED"))))
    (format-time-string "%e %8B %Y" (org-read-date t t timestamp))))

(defun mpereira/org-agenda-tags-suffix ()
  (interactive)
  (format "%s %10s"
          (mpereira/org-entry-get-timestamp-at-point)
          (mpereira/deadline-or-scheduled)))

(defun mpereira/org-agenda-project-name-prefix-format ()
  (s-truncate 20 (car (org-get-outline-path t))))

(defun mpereira/custom-agenda ()
  (interactive)
  (let* ((settings
          '((todo "DOING"
                  ((org-agenda-overriding-header "\nDoing\n")
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'scheduled))))
            (agenda ""
                    ((org-deadline-warning-days 0)
                     (org-agenda-span 'day)
                     (org-agenda-use-time-grid t)
                     (org-agenda-format-date "")
                     (org-habit-show-habits nil)
                     (org-agenda-overriding-header
                      (concat
                       "\nToday "
                       "(" (format-time-string "%A, %B %d" (current-time)) ")"))))
            (agenda ""
                    ((org-agenda-start-day "+1d")
                     (org-agenda-start-on-weekday nil)
                     (org-agenda-overriding-header "\nNext 7 Days\n")))
            (tags-todo (concat "SCHEDULED>\"<+7d>\"&SCHEDULED<=\"<+120d>\""
                               "|"
                               "DEADLINE>\"<+7d>\"&DEADLINE<=\"<+120d>\"/!")
                       ((org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'todo 'done))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-prefix-format
                         " %-12:c %(mpereira/org-agenda-tags-suffix)  ")
                        (org-agenda-sorting-strategy '(timestamp-up))
                        (org-agenda-remove-times-when-in-prefix nil)
                        (org-agenda-overriding-header
                         "\nNext Deadlines and Schedules\n")))
            (agenda ""
                    ((org-agenda-skip-function
                      'mpereira/org-skip-subtree-unless-habit)
                     (org-agenda-span 'day)
                     (org-agenda-format-date "")
                     (org-habit-show-all-today t)
                     (org-agenda-overriding-header "\nHabits")))
            (todo "TODO"
                  ((org-agenda-skip-function
                    '(or (org-agenda-skip-entry-if 'scheduled 'deadline)
                         (mpereira/org-skip-inbox)
                         (mpereira/org-skip-subtree-if-habit)
                         (mpereira/org-skip-all-but-first)
                         (mpereira/org-skip-someday-projects-subheadings)))
                   (org-agenda-sorting-strategy '(deadline-up
                                                  scheduled-up
                                                  time-up
                                                  timestamp-up
                                                  todo-state-up
                                                  alpha-up))
                   (org-agenda-prefix-format
                    " %-12:c %-22(mpereira/org-agenda-project-name-prefix-format)")
                   (org-agenda-overriding-header "\nNext Tasks\n")))))
         (inbox-file (concat org-directory "inbox.org"))
         (inbox-buffer (find-file-noselect inbox-file))
         (inbox (with-current-buffer inbox-buffer
                  (org-element-contents (org-element-parse-buffer 'headline))))
         (_ (when inbox
              (add-to-list
               'settings
               `(todo "TODO"
                      ((org-agenda-overriding-header "\nInbox\n")
                       (org-agenda-files (list ,inbox-file)))))))
         (org-agenda-custom-commands (list
                                      (list
                                       "c" "Custom agenda view"
                                       settings
                                       '((org-agenda-block-separator ?\-))))))
    (org-agenda nil "c")))

;; Redo agenda after capturing.
(add-hook 'org-capture-after-finalize-hook 'org-agenda-maybe-redo)

;; https://lists.gnu.org/archive/html/emacs-orgmode/2015-06/msg00266.html
(defun mpereira/org-agenda-delete-empty-blocks ()
  "Remove empty agenda blocks.
A block is identified as empty if there are fewer than 2 non-empty lines in the
block (excluding the line with `org-agenda-block-separator' characters)."
  (when org-agenda-compact-blocks
    (user-error "Cannot delete empty compact blocks"))
  (setq buffer-read-only nil)
  (save-excursion
    (goto-char (point-min))
    (let* ((blank-line-re "^\\s-*$")
           (content-line-count (if (looking-at-p blank-line-re) 0 1))
           (start-pos (point))
           (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
      (while (and (not (eobp)) (forward-line))
        (cond
         ((looking-at-p block-re)
          (when (< content-line-count 2)
            (delete-region start-pos (1+ (point-at-bol))))
          (setq start-pos (point))
          (forward-line)
          (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
         ((not (looking-at-p blank-line-re))
          (setq content-line-count (1+ content-line-count)))))
      (when (< content-line-count 2)
        (delete-region start-pos (point-max)))
      (goto-char (point-min))
      ;; The above strategy can leave a separator line at the beginning of the
      ;; buffer.
      (when (looking-at-p block-re)
        (delete-region (point) (1+ (point-at-eol))))))
  (setq buffer-read-only t))

(add-hook 'org-agenda-finalize-hook #'mpereira/org-agenda-delete-empty-blocks)

(setq org-tags-column -80)

;; Fontify code in code blocks.
(setq org-src-fontify-natively t)

;; Make TAB act as if it were issued in a buffer of the language’s major mode.
(setq org-src-tab-acts-natively t)

(org-babel-do-load-languages 'org-babel-load-languages
                             '((sh . t)
                               (emacs-lisp . t)))

(setq org-confirm-babel-evaluate nil)

(setq org-todo-keywords '((sequence "TODO(t!)"
                                    "DOING(d!)"
                                    "WAITING(w@/!)"
                                    "|"
                                    "SOMEDAY(s@/!)"
                                    "CANCELLED(c@/!)"
                                    "DONE(D!)")))

(setq org-capture-templates
      '(("t" "Inbox" entry
         (file "inbox.org")
         "* TODO %i%?")
        ("c" "Calendar" entry
         (file "gcal/calendar.org")
         "* %i%?\n  %^{When?}t")
        ("a" "Appointment" entry
         (file "appointments.org")
         "* %i%?\n  %^{When?}t")
        ("j" "Journal" entry
         (file+olp+datetree "ego.org" "Journal")
         "* %U %^{Title}\n  %?")))

(add-hook 'org-capture-mode-hook #'evil-insert-state)

(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; `org-reverse-note-order' set to true along with the two following hooks gets
;; us two things after refiling:
;; 1. Line breaks between top-level headings are maintained.
;; 2. Entries are sorted and top-level heading visibility is set to CHILDREN.
(setq org-reverse-note-order t)

(defun mpereira/org-sort-parent-entries (&rest args)
  ;; `org-sort-entries' doesn't respect `save-excursion'.
  (let ((origin (point)))
    (org-up-heading-safe)
    (apply #'org-sort-entries args)
    (goto-char origin)))

(add-hook 'org-after-refile-insert-hook
          (lambda ()
            (interactive)
            (mpereira/org-sort-parent-entries nil ?o)))

(defun mpereira/org-cycle-cycle ()
  (org-cycle)
  ;; https://www.mail-archive.com/emacs-orgmode@gnu.org/msg86779.html
  (ignore-errors
    (org-cycle)))

(add-hook 'org-after-sorting-entries-or-items-hook #'mpereira/org-cycle-cycle)

;; Save org buffers after some operations.
(dolist (hook '(org-refile
                org-agenda-add-note
                org-agenda-deadline
                org-agenda-kill
                org-agenda-refile
                org-agenda-schedule
                org-agenda-set-property
                org-agenda-set-tags))
  ;; https://github.com/bbatsov/helm-projectile/issues/51
  (advice-add hook :after (lambda (&rest _) (org-save-all-org-buffers))))

(general-define-key
 :states '(normal visual)
 :prefix mpereira/leader
 :infix "o"
 "a" 'mpereira/custom-agenda
 "A" 'org-agenda
 "c" 'org-capture
 "l" 'org-store-link
 "D" 'org-check-deadlines)

(general-define-key
 :keymaps '(org-mode-map)
 :states '(normal)
 "(" 'org-up-element
 ")" 'outline-next-visible-heading
 "C-S-h" 'org-metaleft
 "C-S-j" 'org-metadown
 "C-S-k" 'org-metaup
 "C-S-l" 'org-metaright
 "C-j" 'org-forward-heading-same-level
 "C-k" 'org-backward-heading-same-level)

(general-define-key
 :keymaps '(org-mode-map)
 :states '(normal visual)
 :prefix mpereira/leader
 :infix "f"
 "o" 'counsel-org-goto)

(defun mpereira/call-interactively-with-prefix-arg (prefix-arg func)
  (let ((current-prefix-arg prefix-arg))
    (call-interactively func)))

(general-define-key
 :keymaps '(org-mode-map)
 :states '(normal visual)
 :prefix mpereira/leader
 :infix "o"
 "!" 'org-time-stamp-inactive
 "." 'org-time-stamp
 "|" 'org-columns
 "\\" 'org-columns
 "Cc" 'org-clock-cancel
 "Cd" 'org-clock-display
 "Ci" 'org-clock-in
 "Cl" 'org-clock-in-last
 "Co" 'org-clock-out
 "d" 'org-deadline
 "b" 'org-tree-to-indirect-buffer
 "B" 'outline-show-branches
 "f" 'org-attach
 "i" 'org-insert-link
 "n" 'org-add-note
 "p" 'org-set-property
 "r" 'org-refile
 "Rd" (lambda ()
        (interactive)
        (mpereira/call-interactively-with-prefix-arg '(4) 'org-deadline))
 "Rs" (lambda ()
        (interactive)
        (mpereira/call-interactively-with-prefix-arg '(4) 'org-schedule))
 "s" 'org-schedule
 "S" 'org-sort-entries
 "t" 'org-set-tags
 "x" 'org-cut-subtree)

(general-define-key
 :keymaps '(org-columns-map)
 "s" (lambda ()
       (interactive)
       (org-columns-quit)
       (org-sort-entries nil ?r)
       (org-columns)))

(defun mpereira/org-gcal-entry-at-point-p ()
  (when-let ((link (org-entry-get (point) "LINK")))
    (string-match "Go to gcal web page" link)))

;; Empirically, 2 seconds seems to be good enough.
(setq mpereira/org-gcal-request-timeout 2)

(general-define-key
 :keymaps '(org-agenda-mode-map)
 "/" 'org-agenda-filter-by-regexp
 "c" (lambda ()
       (interactive)
       ;; When capturing to a calendar org-gcal sends a network request that
       ;; reorders the calendar headings on completion, causing them to have a
       ;; different order than the agenda entries. Here we install a buffer
       ;; local hook that will sync the agenda entries with the calendar
       ;; headings.
       (add-hook 'org-capture-after-finalize-hook
                 (lambda ()
                   (interactive)
                   (run-at-time mpereira/org-gcal-request-timeout
                                nil
                                #'org-agenda-maybe-redo))
                 nil
                 t)
       (org-agenda-capture))
 "d" 'org-agenda-deadline
 "f" 'org-attach
 "F" 'org-gcal-sync
 "g" (lambda ()
       (interactive)
       (org-agenda-filter-remove-all)
       (org-save-all-org-buffers)
       (org-agenda-maybe-redo))
 "h" nil
 "j" 'org-agenda-next-item
 "k" 'org-agenda-previous-item
 "l" nil
 "n" 'org-agenda-add-note
 "r" 'org-agenda-refile
 "s" 'org-agenda-schedule
 "T" 'org-agenda-set-tags
 "u" 'org-agenda-undo
 "w" nil
 "x" (lambda ()
       (interactive)
       (save-window-excursion
         (let ((agenda-buffer (current-buffer)))
           (org-agenda-goto)
           (if (mpereira/org-gcal-entry-at-point-p)
               (progn
                 (org-gcal-delete-at-point)
                 ;; org-gcal only removes the calendar headings after the
                 ;; network request finishes.
                 (run-at-time mpereira/org-gcal-request-timeout
                              nil #'org-agenda-maybe-redo))
             (progn
               (quit-window)
               (org-agenda-kill))))))
 "C-j" 'org-agenda-next-item
 "C-k" 'org-agenda-previous-item)

;; org-gcal ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-gcal
  :ensure t
  :config
  (setq mpereira/org-gcal-directory (concat org-directory "gcal/"))
  (setq org-gcal-client-id mpereira/secret-org-gcal-client-id
        org-gcal-client-secret mpereira/secret-org-gcal-client-secret
        org-gcal-file-alist `(("murilo@murilopereira.com"
                               .
                               ,(concat mpereira/org-gcal-directory
                                        "calendar.org"))))
  (add-to-list 'org-agenda-files mpereira/org-gcal-directory t)
  
  ;; https://github.com/myuhe/org-gcal.el/issues/50#issuecomment-231525887
  (defun mpereira/org-gcal--notify (title mes)
    (message "org-gcal::%s - %s" title mes))

  (fset 'org-gcal--notify 'mpereira/org-gcal--notify))

;; google-this ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package google-this
  :ensure t
  :config
  (google-this-mode 1)

  (general-define-key
   ;; :keymaps '(google-this-mode-submap)
   :states '(normal)
   :prefix mpereira/leader
   "fg" 'google-this)

  (general-define-key
   ;; :keymaps '(google-this-mode-submap)
   :states '(visual)
   :prefix mpereira/leader
   "fg" 'google-this-region))

;; company-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)

  (setq company-require-match 'never)

  (general-define-key
   :keymaps '(company-active-map)
   "C-j" 'company-select-next
   "C-k" 'company-select-previous))

;; ansi-term ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq explicit-shell-file-name "/usr/local/bin/fish")

;; FIXME
(general-define-key
 :keymaps '(term-raw-map term-mode-map)
 :states '(normal)
 "p" 'term-paste
 ;; TODO: make P paste before.
 "P" 'term-paste)

;; FIXME
(general-define-key
 :keymaps '(term-raw-map term-mode-map)
 :states '(insert)
 "M-v" 'term-paste)

;; Kill term buffers when term process exits.
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))

(ad-activate 'term-sentinel)

(add-hook 'term-mode-hook (lambda ()
                            (setq-local term-prompt-regexp "^\$ +")
                            (setq-local term-eol-on-send nil)
                            (setq-local show-trailing-whitespace nil)
                            (setq-local global-hl-line-mode nil)))

;; sql ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sql)

(add-hook 'sql-interactive-mode-hook (lambda () (toggle-truncate-lines t)))

;; xclip ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package xclip
  :ensure t
  :config
  ;; For copy/paste on terminal emacs clients.
  ;; Would it be possible to get this working for a Chromebook emacsclient
  ;; started from Secure Shell?
  (turn-on-xclip))

;; writeroom-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package writeroom-mode
  :ensure t)

;; exec-path-from-shell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-variables
        (append exec-path-from-shell-variables
                '("SSH_AUTH_SOCK"
                  "SSH_AGENT_PID")))
  (exec-path-from-shell-initialize))

;; expand-region ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package expand-region
  :ensure t
  :config
  (general-define-key
   :states '(normal visual)
   "+" 'er/expand-region))

;; rainbow-delimiters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode))

;; yaml-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml(?:\\.j2)?\\'" . yaml-mode))

  (general-define-key
   :keymaps '(yaml-mode-map)
   :states '(insert)
   "RET" 'newline-and-indent))

;; help-fns+ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package help-fns+
  :ensure t)

;; es-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package es-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.es$" . es-mode)))

;; pos-tip ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SOMEDAY: pos-tip doesn't seem to work correctly in macOS. Maybe it will on
;; Emacs 25?
(use-package pos-tip
  :ensure t)

;; aggressive-indent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :config
  (add-hook 'prog-mode-hook 'aggressive-indent-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'makefile-mode))

;; gist ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gist
  :ensure t
  :config
  (general-define-key
   :states '(normal visual)
   :prefix mpereira/leader
   :infix "gi"
   "p" 'gist-region-or-buffer-private
   "i" 'gist-region-or-buffer
   "l" 'gist-list)

  (general-define-key
   :keymaps '(gist-list-menu-mode-map)
   "g" nil
   "k" nil)

  ;; TODO: can we use `(evil-set-initial-state 'gist-list-menu-mode 'normal)`
  ;; instead of most of the mappings below?
  (general-define-key
   :keymaps '(gist-list-menu-mode-map)
   "C-j" 'next-line
   "C-k" 'previous-line
   "j" 'next-line
   "k" 'previous-line
   "C-f" 'scroll-up-command
   "C-b" 'scroll-down-command
   "r" 'gist-list-reload
   "gg" 'beginning-of-buffer
   "G" 'end-of-buffer
   "/" 'evil-search-forward
   "n" 'evil-search-next
   "N" 'evil-search-previous
   "X" 'gist-kill-current))

;; markdown-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :diminish markdown-live-preview-mode)

;; lispy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lispy
  :ensure t
  :diminish lispy-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode)
  (add-hook 'clojure-mode-hook 'lispy-mode)

  ;; Disable most lispy mappings.
  (setq lispy-mode-map lispy-mode-map-base)
  (setcdr (assq 'lispy-mode minor-mode-map-alist)
          lispy-mode-map)

  (general-define-key
   :keymaps 'lispy-mode-map
   :states '(insert)
   "<backspace>" 'lispy-delete-backward
   "<deletechar>" 'lispy-delete
   ")" 'lispy-right-nostring
   "\"" 'lispy-doublequote
   "[" 'lispy-brackets
   "]" 'lispy-close-square
   "{" 'lispy-braces
   "}" 'lispy-close-curly)

  (general-define-key
   :keymaps 'lispy-mode-map
   :states '(normal)
   :prefix mpereira/leader
   "r" 'lispy-raise-sexp
   "R" 'lispy-raise-some
   "(" 'lispy-wrap-round
   "[" 'lispy-wrap-brackets
   "{" 'lispy-wrap-braces
   "c" 'lispy-clone))

;; lispyville ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lispyville
  :ensure t
  :after evil lispy
  :diminish lispyville-mode
  :config
  (add-hook 'lispy-mode-hook 'lispyville-mode)

  (lispyville-set-key-theme '(operators))

  (general-define-key
   :keymaps '(lispyville-mode-map)
   :states '(insert)
   "ESC" 'lispyville-normal-state)

  (general-define-key
   :keymaps '(lispyville-mode-map)
   :states '(normal)
   "S" 'lispyville-change-whole-line
   "B" 'mpereira/backward-sexp
   "gA" 'mpereira/append-to-end-of-list
   "gI" 'mpereira/insert-to-beginning-of-list
   "W" 'mpereira/forward-sexp
   "(" 'lispyville-backward-up-list
   ")" 'lispyville-up-list
   "C-(" 'lispyville-beginning-of-defun
   "C-)" 'lispyville-end-of-defun
   "{" 'lispyville-previous-opening
   "}" 'lispyville-next-opening
   ">)" 'lispy-forward-slurp-sexp
   "<)" 'lispy-forward-barf-sexp
   "<(" 'lispy-backward-slurp-sexp
   ">(" 'lispy-backward-barf-sexp
   "|" 'lispy-split
   "_" 'lispy-join
   "<f" 'lispyville-drag-backward
   ">f" 'lispyville-drag-forward
   "C-9" 'lispy-describe-inline
   "C-0" 'lispy-arglist-inline))

;; which-key ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

;; projectile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :ensure t
  :config
  (projectile-mode t)

  (setq projectile-enable-caching nil)
  (setq projectile-require-project-root t)

  (defun mpereira/projectile-eshell ()
    (interactive)
    (if (projectile-project-p)
        (let ((eshell-buffer-name (concat "*eshell " (projectile-project-name) "*")))
          (projectile-with-default-dir (projectile-project-root)
            (eshell t)))
      (eshell t)))

  (general-define-key
   :states '(normal)
   :prefix mpereira/leader
   :infix "s"
   "h" 'mpereira/projectile-eshell
   "H" 'projectile-run-term
   "c" 'projectile-run-async-shell-command-in-root))

;; perspective ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package perspective
  :ensure t
  :init
  (setq persp-show-modestring nil)
  :config
  (persp-mode t))

;; persp-projectile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package persp-projectile
  :ensure t
  :after perspective projectile
  :config
  (general-define-key
   :states '(normal)
   :prefix mpereira/leader
   :infix "p"
   "p" 'persp-switch-last))

;; term-projectile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package term-projectile
  :ensure t
  :after projectile)

;; avy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package avy
  :ensure t
  :config
  (setq avy-all-windows nil)
  (general-define-key
   :states '(normal visual)
   "s" 'avy-goto-char-timer))

;; ivy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode t)

  (setq ivy-use-selectable-prompt t)
  (setq ivy-height 20)
  (setq ivy-wrap t)

  ;; FIXME: the following does change ivy-occur buffers to wgrep mode but it is
  ;; nonfunctional.
  ;; (add-hook 'ivy-occur-grep-mode-hook 'ivy-wgrep-change-to-wgrep-mode)

  (general-define-key
   :states '(normal visual)
   :prefix mpereira/leader
   "." 'ivy-resume)

  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line
   "C-f" 'ivy-scroll-up-command
   "C-b" 'ivy-scroll-down-command
   "C-o" 'ivy-occur
   "C-h" 'ivy-beginning-of-buffer
   "C-l" 'ivy-end-of-buffer
   "C-/" 'ivy-avy
   "<escape>" 'minibuffer-keyboard-quit))

;; command-log-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package command-log-mode
  :ensure t
  :diminish command-log-mode
  :config
  (setq command-log-mode-auto-show t)
  (setq command-log-mode-window-size 60))

;; wgrep ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package wgrep
  :ensure t)

;; counsel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package counsel
  :ensure t
  :after ivy
  :config
  (general-define-key
   :states '(normal visual)
   :prefix mpereira/leader
   :infix "f"
   ":" 'counsel-expression-history
   "b" 'ivy-switch-buffer
   "f" 'counsel-find-file
   "k" 'counsel-descbinds
   "l" 'counsel-find-library
   "m" 'describe-keymap
   "n" 'counsel-describe-function
   "p" 'package-list-packages-no-fetch
   "v" 'counsel-describe-variable
   "y" 'counsel-yank-pop))

;; swiper ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package swiper
  :ensure t
  :config
  (general-define-key
   :keymaps '(swiper-map swiper-all-map ivy-minibuffer-map)
   "<escape>" 'minibuffer-keyboard-quit ;; is this still needed?
   "C-r" 'evil-paste-from-register))

;; counsel-projectile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package counsel-projectile
  :ensure t
  :after counsel projectile
  :config
  (setq projectile-switch-project-action 'counsel-projectile-find-file)

  (general-define-key
   :states '(normal visual)
   :prefix mpereira/leader
   :infix "p"
   "s" 'mpereira/ivy-persp-switch-project
   "b" 'counsel-projectile-switch-to-buffer
   "f" 'counsel-projectile-find-file
   "g" 'counsel-projectile-ag))

;; neotree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package neotree
  :ensure t
  :after projectile
  :config
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (if project-dir
          (progn
            (neotree-dir project-dir)
            (neotree-find file-name))
        (message "Could not find git project root."))))

  (setq neo-smart-open t)

  (general-define-key
   :states '(normal visual)
   :prefix mpereira/leader
   :infix "p"
   "t" 'neotree-project-dir)

  (general-define-key
   :keymaps 'neotree-mode-map
   :states '(normal visual)
   :prefix mpereira/leader
   :infix "p"
   "t" 'neotree-hide)

  (general-define-key
   :keymaps 'neotree-mode-map
   :states '(normal visual)
   "RET" 'neotree-enter
   "TAB" 'neotree-enter
   "r" 'neotree-refresh
   "q" 'neotree-hide))

;; slamhound ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package slamhound
  :after cider
  :ensure t)

;; scala-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package scala-mode
  :ensure t)

;; clojure-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package clojure-mode
  :ensure t)

;; inf-clojure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package inf-clojure
  :ensure t)

;; clojure-cheatsheet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: navigate clojure-cheatsheet buffer with consistent keybindings.
(use-package clojure-cheatsheet
  :ensure t)

;; cider ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cider
  :ensure t
  :diminish cider--debug-mode
  :config
  (setq cider-prompt-for-symbol nil)
  (setq cider-repl-display-help-banner nil)

  (general-define-key
   :keymaps 'cider-mode-map
   :states '(normal visual)
   "K" 'cider-doc
   "gf" 'cider-find-var)

  (general-define-key
   :keymaps 'cider-mode-map
   :states '(normal)
   :prefix mpereira/leader
   "ee" 'cider-eval-sexp-at-point
   "e(" 'cider-eval-defun-at-point
   "eE" 'cider-eval-buffer
   "dd" 'cider-debug-defun-at-point
   "tt" 'cider-test-run-test
   "tr" 'cider-test-rerun-test
   "tT" 'cider-test-run-ns-tests
   "tR" 'cider-test-rerun-failed-tests
   "pt" 'cider-test-run-project-tests)

  (general-define-key
   :keymaps 'cider-mode-map
   :states '(visual)
   :prefix mpereira/leader
   "ee" 'cider-eval-region))

;; diff-hl ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode t)

  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  (set-face-foreground 'diff-hl-insert "none")
  (set-face-background 'diff-hl-insert "green4")
  (set-face-foreground 'diff-hl-change "none")
  (set-face-background 'diff-hl-change "yellow3")
  (set-face-foreground 'diff-hl-delete "none")
  (set-face-background 'diff-hl-delete "red4")

  (general-define-key
   :states '(normal visual)
   :prefix mpereira/leader
   "gr" 'diff-hl-revert-hunk)

  (general-define-key
   :states '(normal visual)
   "]c" 'diff-hl-next-hunk
   "[c" 'diff-hl-previous-hunk))

;; redtick ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package redtick
  :ensure t)

;; browse-at-remote ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package browse-at-remote
  :ensure t
  :config
  (general-define-key
   :states '(normal visual)
   :prefix mpereira/leader
   "go" 'browse-at-remote))

;; magit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :config
  (add-hook 'with-editor-mode-hook 'evil-insert-state)

  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)

  (general-define-key
   :states '(normal)
   :prefix mpereira/leader
   :infix "g"
   "b" 'magit-blame
   "c" 'magit-commit-popup
   "d" 'magit-diff-buffer-file
   "D" 'magit-diff-unstaged
   "l" 'magit-log-buffer-file
   "L" 'magit-log-all
   "p" 'magit-push-popup
   "s" 'magit-status
   "w" 'magit-stage-file
   "W" 'magit-stage-modified
   "<" 'smerge-keep-mine
   ">" 'smerge-keep-other)

  ;; This makes magit slow when there are a lot of buffers. See:
  ;; https://github.com/magit/magit/issues/2687#issuecomment-224845496
  (add-hook 'magit-update-uncommitted-buffer-hook 'vc-refresh-state))

;; magit-gh-pulls ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit-gh-pulls
  :ensure t
  :after magit
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

;; evil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :ensure t
  :init
  (setq evil-symbol-word-search t)
  (setq evil-shift-width 2)
  (setq evil-move-cursor-back t)
  (setq evil-move-beyond-eol nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode t)

  ;; Make it possible for other modes to use these bindings (e.g. company mode
  ;; uses it for navigating completions).
  (general-define-key
   :keymaps '(evil-insert-state-map)
   "C-j" nil
   "C-k" nil)

  (fset 'evil-visual-update-x-selection 'ignore)

  (evil-set-initial-state 'package-menu-mode 'normal)

  (general-define-key
   :keymaps '(evil-motion-state-map)
   ";" 'evil-ex
   ":" 'evil-repeat-find-char)

  ;; Using `bind-keys*` instead of `general-define-key` because term-mode-map
  ;; binds these to term-send-raw.
  (bind-keys*
   ("M-h" . evil-window-left)
   ("M-j" . evil-window-down)
   ("M-k" . evil-window-up)
   ("M-l" . evil-window-right))

  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  (use-package evil-org
    :ensure t
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)

    ;; Org todo notes don't have a specific major mode, so change to insert
    ;; state based on its buffer name.
    ;; FIXME: doesn't seem to be working.
    (add-hook 'org-mode-hook
              (lambda ()
                (when (string= "*Org Note*" (buffer-name))
                  (evil-insert-state))))

    (defmacro calendar-action (func)
      `(lambda ()
         (interactive)
         (org-eval-in-calendar '(,func 1))))

    (general-define-key
     :keymaps '(org-read-date-minibuffer-local-map)
     "q" 'minibuffer-keyboard-quit
     "l" (calendar-action calendar-forward-day)
     "h" (calendar-action calendar-backward-day)
     "j" (calendar-action calendar-forward-week)
     "k" (calendar-action calendar-backward-week)
     ">" (calendar-action calendar-forward-month)
     "<" (calendar-action calendar-backward-month)
     "}" (calendar-action calendar-forward-year)
     "{" (calendar-action calendar-backward-year)
     "0" (calendar-action calendar-beginning-of-week)
     "$" (calendar-action calendar-end-of-week))

    (evil-set-initial-state 'calendar-mode 'emacs)

    (general-define-key
     :keymaps '(calendar-mode-map)
     "l" 'calendar-forward-day
     "h" 'calendar-backward-day
     "h" 'calendar-backward-day
     "j" 'calendar-forward-week
     "k" 'calendar-backward-week
     ">" 'calendar-forward-month
     "<" 'calendar-backward-month
     "}" 'calendar-forward-year
     "{" 'calendar-backward-year
     "0" 'calendar-beginning-of-week
     "$" 'calendar-end-of-week)

    (add-hook 'evil-org-mode-hook
              (lambda ()
                (evil-org-set-key-theme '(operators
                                          navigation
                                          textobjects
                                          todo)))))

  (use-package evil-magit
    :after magit
    :ensure t
    :config
    (general-define-key
     :keymaps 'magit-mode-map
     :states '(normal visual)
     "j" 'evil-next-visual-line
     "k" 'evil-previous-visual-line
     "C-j" 'magit-section-forward
     "C-k" 'magit-section-backward)

    (general-define-key
     :states '(normal)
     :keymaps '(git-rebase-mode-map)
     "x" 'git-rebase-kill-line
     "C-S-j" 'git-rebase-move-line-down
     "C-S-k" 'git-rebase-move-line-up))

  (use-package evil-extra-operator
    :ensure t
    :init
    (setq evil-extra-operator-eval-key "ge")
    :config
    (add-hook 'prog-mode-hook 'evil-extra-operator-mode))

  (use-package evil-exchange
    :ensure t
    :config
    (evil-exchange-install))

  (use-package evil-escape
    :ensure t
    :diminish evil-escape-mode
    :config
    (evil-escape-mode)
    (setq evil-escape-key-sequence "kj"))

  (use-package evil-nerd-commenter
    :ensure t
    :config
    (general-define-key
     :keymaps '(normal)
     "gc" 'evilnc-comment-operator))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode t))

  (use-package evil-goggles
    :ensure t
    :diminish evil-goggles-mode
    :config
    (evil-goggles-mode)

    ;; Optionally use diff-mode's faces; as a result, deleted text will be
    ;; highlighed with `diff-removed` face which is typically some red color
    ;; (as defined by the color theme) other faces such as `diff-added` will
    ;; be used for other actions.
    (evil-goggles-use-diff-faces)))

;; mingus ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package mingus
  :ensure t
  :config
  (dolist (mode '(mingus-help-mode
                  mingus-playlist-mode
                  mingus-browse-mode))
    (evil-set-initial-state mode 'emacs))

  (dolist (hook '(mingus-browse-hook
                  mingus-playlist-hooks))
    (add-hook hook 'mpereira/setq-local-show-trailing-whitespace-nil)))
