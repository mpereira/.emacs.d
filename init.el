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
(setq-default save-place t)

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

  (setq general-default-keymaps 'evil-normal-state-map
        general-default-states 'normal)

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
   :keymaps 'global-map
   :states '(normal visual)
   "<s-return>" 'toggle-frame-fullscreen
   "s-+" 'text-scale-increase
   "s--" 'text-scale-decrease)

  (general-define-key
   :keymaps 'global-map
   :states '(normal visual)
   :prefix my-leader
   "," 'mode-line-other-buffer
   "dk" 'describe-key
   "df" 'describe-function
   "dv" 'describe-variable
   "b" 'switch-to-buffer
   "w" 'save-buffer
   "q" 'evil-quit
   "hs" 'split-window-vertically
   "vs" 'split-window-horizontally))

;; expand-region ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package expand-region
  :ensure t
  :config
  (general-define-key
   :keymaps 'global-map
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

;; Lispy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Lispyville ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lispyville
  :ensure t
  :after evil lispy
  :diminish (lispyville-mode "LY")
  :config
  (add-hook 'lispy-mode-hook 'lispyville-mode)
  (evil-define-key 'normal lispyville-mode-map
    (kbd "y") 'lispyville-yank
    (kbd "d") 'lispyville-delete
    (kbd "c") 'lispyville-change
    (kbd "x") 'lispyville-delete-char-or-splice
    (kbd "Y") 'lispyville-yank-line
    (kbd "D") 'lispy-kill
    (kbd "C") 'lispyville-change-line
    (kbd "X") 'lispyville-delete-char-or-splice-backwards
    (kbd "B") 'lispyville-backward-sexp
    ;; FIXME: W
    (kbd "W") 'lispyville-forward-sexp
    (kbd "(") 'lispyville-backward-up-list
    (kbd ")") 'lispyville-up-list
    ;; FIXME: barfs and slurps
    (kbd ">)") 'lispyville->
    (kbd "<)") 'lispyville-<
    (kbd "<(") 'lispy-slurp
    (kbd ">(") 'lispy-barf
    (kbd "|") 'lispy-split
    (kbd "_") 'lispy-join
    (kbd "<f") 'lispyville-move-up
    (kbd ">f") 'lispyville-move-down)

  (evil-define-key 'visual lispyville-mode-map
    (kbd "y") 'lispyville-yank
    (kbd "d") 'lispyville-delete
    (kbd "c") 'lispyville-change
    (kbd "Y") 'lispyville-yank-line
    (kbd "D") 'lispy-kill
    (kbd "C") 'lispyville-change-line
    (kbd "B") 'lispyville-backward-sexp
    ;; FIXME: W
    (kbd "W") 'lispyville-forward-sexp
    (kbd "(") 'lispyville-backward-up-list
    (kbd ")") 'lispyville-up-list)

  (evil-define-key 'insert lispyville-mode-map
    (kbd "ESC") 'lispyville-normal-state)

  (general-define-key
   :states '(normal visual)
   :keymaps 'lispyville-mode-map
   :prefix my-leader
   "R" 'lispy-raise-sexp))

;; Auto-Complete ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :config
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  (setq ac-auto-start nil)
  (setq ac-use-quick-help t)
  (setq ac-quick-help-delay 0.5)
  (setq ac-use-menu-map t)
  (define-key ac-menu-map "\C-j" 'ac-next)
  (define-key ac-menu-map "\C-k" 'ac-previous))

;; Which-key ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (setq projectile-enable-caching t)
  (general-define-key
   :keymaps 'global-map
   :states '(normal)
   :prefix my-leader
   "pf" 'projectile-find-file
   "ps" 'projectile-switch-project))

;; neotree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package neotree
  :ensure t
  :config
  (general-define-key
   :keymaps 'global-map
   :states '(normal visual)
   :prefix my-leader
   "tt" 'neotree-projectile-action)

  (general-define-key
   :keymaps 'neotree-mode-map
   :states '(normal visual)
   "RET" 'neotree-enter
   "q" 'neotree-hide)

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
   "K" 'cider-doc)

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

;; Magit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :config
  (general-define-key
   :prefix my-leader
   :keymaps 'global-map
   :states '(normal)
   "gs" 'magit-status
   "gb" 'magit-blame))

;; Evil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :ensure t
  :init
  :config
  (add-hook
   'evil-mode-hook
   (lambda ()
     (setq evil-shift-width 2)

     (define-key evil-normal-state-map (kbd ";") 'evil-ex)
     (define-key evil-normal-state-map (kbd ":") 'evil-repeat-find-char)

     (evil-define-key '(normal visual) global-map
       (kbd "C-h") 'evil-window-left
       (kbd "C-j") 'evil-window-down
       (kbd "C-k") 'evil-window-up
       (kbd "C-l") 'evil-window-right)))

  (add-hook
   'occur-mode-hook
   (lambda ()
     (evil-add-hjkl-bindings occur-mode-map 'emacs
       (kbd "/")       'evil-search-forward
       (kbd "j")       'evil-search-next
       (kbd "k")       'evil-search-previous
       (kbd "C-d")     'evil-scroll-down
       (kbd "C-u")     'evil-scroll-up
       (kbd "C-w C-w") 'other-window)))

  (evil-mode t)

  (use-package evil-magit
    :after magit
    :ensure t)

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
    (evil-define-key 'normal global-map
      (kbd "gc") 'evilnc-comment-operator))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode t)))

(require 'diminish)
(diminish 'auto-revert-mode)
(diminish 'global-whitespace-mode)
(diminish 'undo-tree-mode)
