(require 'package)

(package-initialize)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Color theme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ample-theme
  :ensure t
  :config
  (add-hook 'after-init-hook (lambda () (load-theme 'ample t))))

;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://www.emacswiki.org/emacs/DescribeThingAtPoint
(defun describe-thing-at-point ()
  "Show the documentation of the Elisp function and variable near point.
  This checks in turn:
  -- for a function name where point is
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
          ((setq sym (variable-at-point)) (describe-variable sym))
          ((setq sym (function-at-point)) (describe-function sym)))))

;; FIXME: popup is showing at random positions.
;; FIXME: help-xref-interned creates a help buffer.
(defun describe-thing-at-point-in-popup ()
  (interactive)
  (let* ((thing (symbol-at-point))
         (help-xref-following t)
         (content (with-temp-buffer
                    (help-mode)
                    (help-xref-interned thing)
                    (buffer-string))))
    (pos-tip-show content nil nil nil 999)))

(require 'thingatpt)
(defun eval-sexp-at-or-surrounding-pt ()
  "Evaluate the sexp following the point, or surrounding the point"
  (interactive)
  (save-excursion
    (forward-char 1)
    (if (search-backward "(" nil t)
        (message "%s" (eval (read-from-whole-string (thing-at-point 'sexp)))))))

(defun toggle-window-split ()
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

;; Options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(line-number-mode t)
(column-number-mode t)
(global-hl-line-mode t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

(fset 'yes-or-no-p 'y-or-n-p)

(require 'linum)
(add-hook 'prog-mode-hook 'linum-mode)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(save-place-mode t)

(require 'savehist)
(setq savehist-additional-variables '(search-ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (expand-file-name "savehist" user-emacs-directory))
(savehist-mode t)

(require 'whitespace)
(setq whitespace-style '(face lines-tail trailing))
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'whitespace-mode))
(setq-default show-trailing-whitespace t)

(setq comment-column 80)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq tab-always-indent 'complete)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; exec-path-from-shell

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-variables
        (append exec-path-from-shell-variables
                '("SONIAN_USER"
                  "SONIAN_RELEASES_REPO_URL"
                  "SONIAN_RELEASES_REPO_USERNAME"
                  "SONIAN_RELEASES_REPO_PASSWORD"
                  "SONIAN_RELEASES_REPO_SIGN"
                  "SONIAN_SNAPSHOTS_REPO_URL"
                  "SONIAN_SNAPSHOTS_REPO_USERNAME"
                  "SONIAN_SNAPSHOTS_REPO_PASSWORD"
                  "SONIAN_SNAPSHOTS_REPO_SIGN")))
  (exec-path-from-shell-initialize))

;; general ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package general
  :ensure t
  :config
  (setq my-leader ",")

  (general-define-key
   "<escape>" 'keyboard-quit)

  (general-define-key
   :keymaps '(minibuffer-local-map
              minibuffer-local-ns-map
              minibuffer-local-completion-map
              minibuffer-local-must-match-map
              minibuffer-local-isearch-map)
   "<escape>" 'minibuffer-keyboard-quit)

  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   :states '(normal visual)
   "C-S-k" 'describe-thing-at-point
   "K" 'describe-thing-at-point-in-popup)

  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   :states '(normal visual)
   :prefix my-leader
   "eE" 'eval-buffer)

  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   :states '(normal)
   :prefix my-leader
   "ee" 'eval-sexp-at-or-surrounding-pt)

  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   :states '(visual)
   :prefix my-leader
   "ee" 'eval-region)

  (general-define-key
   :states '(normal visual)
   "<s-return>" 'toggle-frame-fullscreen
   "s-+" 'text-scale-increase
   "s--" 'text-scale-decrease)

  (general-define-key
   :states '(normal visual)
   :prefix my-leader
   "," 'mode-line-other-buffer
   "dk" 'describe-key
   "df" 'describe-function
   "dv" 'describe-variable
   "sh" 'eshell
   "b" 'switch-to-buffer
   "w" 'save-buffer
   "q" 'evil-quit
   "hs" 'split-window-vertically
   "vs" 'split-window-horizontally
   "hv" 'toggle-window-split
   "vh" 'toggle-window-split))

;; expand-region ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package expand-region
  :ensure t
  :config
  (general-define-key
   :states '(normal visual)
   "+" 'er/expand-region))

;; pos-tip ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package pos-tip
  :ensure t)

;; aggressive-indent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :config
  (add-hook 'prog-mode-hook 'aggressive-indent-mode))

;; lispy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lispy
  :ensure t
  :diminish lispy-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode)
  (add-hook 'clojure-mode-hook 'lispy-mode)
  (general-define-key
   :keymaps 'lispy-mode-map
   :states '(insert)
   "[" 'lispy-brackets
   "]" 'lispy-close-square
   "{" 'lispy-braces
   "}" 'lispy-close-curly))

;; lispyville ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lispyville
  :ensure t
  :after evil lispy
  :diminish lispyville-mode
  :config
  (add-hook 'lispy-mode-hook 'lispyville-mode)

  (general-define-key
   :states '(normal visual)
   :keymaps 'lispyville-mode-map
   :prefix my-leader
   "R" 'lispy-raise-sexp)

  (general-define-key
   :keymaps '(lispyville-mode-map)
   :states '(insert)
   "ESC" 'lispyville-normal-state)

  (general-define-key
   :keymaps '(lispyville-mode-map)
   :states '(normal visual)
   "y" 'lispyville-yank
   "d" 'lispyville-delete
   "c" 'lispyville-change
   "Y" 'lispyville-yank-line
   "D" 'lispy-kill
   "C" 'lispyville-change-line
   "B" 'lispyville-backward-sexp
   ;; FIXME: W
   "W" 'lispyville-forward-sexp
   "(" 'lispyville-backward-up-list
   ")" 'lispyville-up-list)

  (general-define-key
   :keymaps '(lispyville-mode-map)
   :states '(normal)
   ;; FIXME: barfs and slurps
   ">)" 'lispyville->
   "<)" 'lispyville-<
   "<(" 'lispy-slurp
   ">(" 'lispy-barf
   "|" 'lispy-split
   "_" 'lispy-join
   "<f" 'lispyville-move-up
   ">f" 'lispyville-move-down))

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
  (projectile-global-mode)
  (setq projectile-enable-caching t))

;; find-file-in-project ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package find-file-in-project
  :ensure t)

;; ivy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode t)

  (setq ivy-height 20)

  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line
   "C-f" 'ivy-scroll-up-command
   "C-b" 'ivy-scroll-down-command
   "C-h" 'ivy-beginning-of-buffer
   "C-l" 'ivy-end-of-buffer
   "<escape>" 'minibuffer-keyboard-quit))

;; counsel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package counsel
  :ensure t
  :after ivy
  :config
  (general-define-key
   :states '(normal visual)
   :prefix my-leader
   "ff" 'counsel-find-file
   "fb" 'ivy-switch-buffer)

  (general-define-key
   :keymaps '(emacs-lisp-mode-map)
   :states '(normal visual)
   :prefix my-leader
   "fv" 'counsel-describe-variable
   "fl" 'counsel-find-library
   "fF" 'counsel-describe-function))

;; counsel-projectile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package counsel-projectile
  :ensure t
  :after counsel projectile
  :config
  (setq projectile-switch-project-action 'counsel-projectile-find-file)

  (general-define-key
   :states '(normal visual)
   :prefix my-leader
   "ps" 'counsel-projectile-switch-project
   "pb" 'counsel-projectile-switch-to-buffer
   "pf" 'counsel-projectile-find-file
   "pg" 'counsel-projectile-ag
   "/" 'swiper))

;; neotree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package neotree
  :ensure t
  :config
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (ffip-project-root))
          (file-name (buffer-file-name)))
      (if project-dir
          (progn
            (neotree-dir project-dir)
            (neotree-find file-name))
        (message "Could not find git project root."))))

  (setq neo-smart-open t)

  (general-define-key
   :states '(normal visual)
   :prefix my-leader
   "tt" 'neotree-project-dir)

  (general-define-key
   :keymaps 'neotree-mode-map
   :states '(normal visual)
   :prefix my-leader
   "tt" 'neotree-hide)

  (general-define-key
   :keymaps 'neotree-mode-map
   :states '(normal visual)
   "RET" 'neotree-enter
   "TAB" 'neotree-enter
   "r" 'neotree-refresh
   "q" 'neotree-hide))

;; clojure-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package clojure-mode
  :ensure t)

;; cider ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cider
  :ensure t
  :config
  (general-define-key
   :keymaps 'cider-mode-map
   :states '(normal visual)
   "K" 'cider-doc
   "gf" 'cider-find-var)

  (general-define-key
   :keymaps 'cider-mode-map
   :states '(normal visual)
   :prefix my-leader
   "eE" 'cider-eval-buffer)

  (general-define-key
   :keymaps 'cider-mode-map

   :states '(normal)
   :prefix my-leader
   "ee" 'cider-eval-sexp-at-point)

  (general-define-key
   :keymaps 'cider-mode-map
   :states '(visual)
   :prefix my-leader
   "ee" 'cider-eval-region))

;; magit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :config
  (general-define-key
   :prefix my-leader
   :states '(normal)
   "gs" 'magit-status
   "gb" 'magit-blame))

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
  :config
  (evil-mode t)

  (setq evil-shift-width 2)

  (general-define-key
   :keymaps '(evil-motion-state-map)
   ";" 'evil-ex
   ":" 'evil-repeat-find-char)

  (general-define-key
   "s-h" 'evil-window-left
   "s-j" 'evil-window-down
   "s-k" 'evil-window-up
   "s-l" 'evil-window-right)

  (use-package evil-magit
    :after magit
    :ensure t
    :config
    (general-define-key
     :keymaps 'magit-mode-map
     :states '(normal visual)
     "j" 'magit-section-forward
     "k" 'magit-section-backward))

  (use-package evil-extra-operator
    :ensure t
    :init
    (setq evil-extra-operator-eval-key (kbd "ge"))
    :config
    (add-hook 'prog-mode-hook 'evil-extra-operator-mode))

  (use-package evil-escape
    :ensure t)

  (use-package evil-nerd-commenter
    :ensure t
    :config
    (general-define-key
     :keymaps '(normal)
     "gc" 'evilnc-comment-operator))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode t)))

(require 'diminish)
(diminish 'auto-revert-mode)
(diminish 'global-whitespace-mode)
(diminish 'undo-tree-mode)
(diminish 'whitespace-mode)
