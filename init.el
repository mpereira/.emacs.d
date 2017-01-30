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

;; Options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

(require 'linum)
(add-hook 'prog-mode-hook 'linum-mode)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(require 'whitespace)
(setq whitespace-style '(face lines-tail trailing))
(global-whitespace-mode t)
(setq-default show-trailing-whitespace t)

;; https://github.com/technomancy/better-defaults
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
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; general ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package general
  :ensure t)

;; pos-tip ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package pos-tip
  :ensure t)

;; aggresssive-indent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'aggressive-indent-mode))

;; Lispy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lispy
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode))

;; Lispyville ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lispyville
  :ensure t
  :after evil lispy
  :config
  (add-hook 'lispy-mode-hook 'lispyville-mode)

  (evil-define-key 'normal global-map
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

  (evil-define-key 'visual global-map
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

  (evil-define-key 'insert global-map
    (kbd "ESC") 'lispyville-normal-state)

  (evil-leader/set-key
    "R" 'lispy-raise-sexp))

;; Auto-Complete ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package auto-complete
  :ensure t
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
  :ensure t)

;; Magit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :config
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "gs" 'magit-status)))

;; Evil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :ensure t
  :init
  (setq evil-extra-operator-eval-key (kbd "ge"))
  :config
  (add-hook 'prog-mode-hook 'evil-extra-operator-mode)

  (add-hook
   'evil-mode-hook
   (lambda ()
     (setq evil-shift-width 2)

     (define-key evil-normal-state-map (kbd ";") 'evil-ex)
     (define-key evil-normal-state-map (kbd ":") 'evil-repeat-find-char)

     (evil-define-key 'normal global-map
       (kbd "C-h") 'evil-window-left
       (kbd "C-j") 'evil-window-down
       (kbd "C-k") 'evil-window-up
       (kbd "C-l") 'evil-window-right
       (kbd "C-S-k") 'describe-thing-at-point
       (kbd "K") 'describe-thing-at-point-in-popup)))

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
    (global-evil-surround-mode t))

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "," 'mode-line-other-buffer
      "dk" 'describe-key
      "df" 'describe-function
      "dv" 'describe-variable
      "b" 'switch-to-buffer
      "w" 'save-buffer
      "q" 'evil-quit
      "hs" 'split-window-vertically
      "vs" 'split-window-horizontally)))
