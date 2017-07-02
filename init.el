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
;; http://raebear.net/comp/emacscolors.html
(use-package ample-theme
  :ensure t
  :defer t
  :init (load-theme 'ample t))

;; mode-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Depends on the diff-hl package for diff-hl-changes.  Adding this to the mode
;; line degrades performance.
(defun mpereira/-mode-line-git-modifications ()
  (let ((symbols '((insert . "+")
                   (change . "~")
                   (delete . "-")))
        (modifications (->> (diff-hl-changes)
                            (-group-by (lambda (item) (nth 2 item)))
                            (-map (lambda (group)
                                    (let* ((key (car group))
                                           (items (cdr group)))
                                      (-reduce-from (lambda (memo item)
                                                      (list key
                                                            (+ (nth 1 memo)
                                                               (nth 1 item))))
                                                    (list key 0)
                                                    items))))
                            (-map (lambda (aggregate)
                                    (apply 'cons aggregate))))))
    (->> symbols
         (-map (lambda (symbol)
                 (let* ((symbol* (cdr symbol))
                        (count (alist-get (car symbol) modifications)))
                   (cons symbol* count))))
         (-filter (lambda (item) (cdr item)))
         (-map (lambda (item)
                 (format "%s%s" (car item) (cdr item))))
         (s-join " "))))

(defvar mpereira/mode-line-git-modifications
  '(:eval
    (concat
     " "
     (propertize (mpereira/-mode-line-git-modifications)
                 'face '(:foreground "gray25" :weight light))
     " ")))

(defvar mpereira/mode-line-projectile
  '(:eval
    (if (projectile-project-name)
        (concat
         (propertize " "
                     'face '())
         (propertize (format "%s" (projectile-project-name)))
         (propertize " "
                     'face '()))
      "")))

(defun mpereira/mode-line-git ()
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize " "
                 'face '())
     (propertize (format "%s" branch)
                 'face '(:foreground "gray18" :weight bold))
     (propertize " "
                 'face '()))))

(defvar mpereira/mode-line-vc
  '(:eval
    (when vc-mode
      (cond
       ((string-match "Git[:-]" vc-mode) (mpereira/mode-line-git))
       (t (format "%s" vc-mode))))))

;; TODO: Make this show shortened directory prefixed to the buffer name if
;; outside the context of a projectile project.
(defvar mpereira/mode-line-buffer
  '(:eval
    (let ((modified-symbol (if (buffer-modified-p) "~" "")))
      (concat
       (propertize " " 'face '(:background "gray48"))
       (propertize
        "%b"
        'face '(:background "gray48" :foreground "gray18" :weight bold))
       (propertize
        modified-symbol
        'face '(:background "gray48" :foreground "gray28" :weight light))
       (propertize " " 'face '(:background "gray48"))))))

(defvar mpereira/mode-line-major-mode
  (concat
   (propertize " "
               'face '())
   (propertize "%m"
               'face '(:foreground "gray18" :weight bold))
   (propertize " "
               'face '())))

;; TODO: align this to the right.
(defvar mpereira/mode-line-buffer-position
  '(:propertize
    " %p %l,%c "
    face (:background "cornsilk4" :foreground "gray25" :weight light)))

(setq-default mode-line-format
              (list mpereira/mode-line-projectile
                    mpereira/mode-line-vc
                    mpereira/mode-line-buffer
                    ;; FIXME: Emacs gets too slow when this is turned on. Is
                    ;; there a more efficient way to get this information?
                    ;; mpereira/mode-line-git-modifications
                    mpereira/mode-line-major-mode
                    (format "%10s" "")
                    mpereira/mode-line-buffer-position
                    mode-line-end-spaces))

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

;; FIXME: popup is showing at random positions.
;; FIXME: help-xref-interned creates a help buffer.
(defun mpereira/describe-thing-at-point-in-popup ()
  (interactive)
  (let* ((thing (symbol-at-point))
         (content (save-window-excursion
                    (with-temp-buffer
                      (let* ((standard-output (current-buffer))
                             (help-xref-following t))
                        (help-mode)
                        (describe-symbol thing)
                        (buffer-string))))))
    ;; (message content)
    (if (null content)
        (message (concat "Can't describe \"" thing "\""))
      (pos-tip-show content nil nil nil 0))))

(require 'thingatpt) ;; for thing-at-point.
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

(defun indent-buffer ()
  "Indents the current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

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
(setq ring-bell-function 'ignore)

;; macOS modifiers.
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq mac-control-modifier 'control)
(setq ns-function-modifier 'hyper)

(fset 'yes-or-no-p 'y-or-n-p)

(require 'linum)
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'linum-mode))

;; Better unique names for similarly-named file buffers.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Remember point position between sessions.
(require 'saveplace)
(save-place-mode t)

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

(setq explicit-shell-file-name "/usr/local/bin/fish")

;; eshell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'eshell)

(setq eshell-scroll-to-bottom-on-input 'all)
(setq shell-error-if-no-glob t)
(setq shell-hist-ignoredups t)
(setq shell-save-history-on-exit t)
(setq shell-destroy-buffer-when-process-dies t)
;; `find` and `chmod` behave differently on eshell than unix shells. Prefer unix
;; behavior.
(setq shell-prefer-lisp-functions nil)

(add-hook 'eshell-mode-hook
          (lambda ()
            (add-to-list 'eshell-visual-commands "ssh")
            (add-to-list 'eshell-visual-commands "tail")))

;; s ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package s
  :ensure t)

;; dash ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dash
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
                          'face '(:background "gray13")) ;; ample/bg
              time-string)))))
  (minibuffer-line-mode t))

;; undo-tree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(diminish 'undo-tree-mode)

(dolist (hook '(undo-tree-mode-hook
                undo-tree-visualizer-mode-hook))
  (add-hook hook 'mpereira/setq-local-show-trailing-whitespace-nil))

(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)

;; js ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default js-indent-level 2)

;; org-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fontify code in code blocks.
(setq org-src-fontify-natively t)

(org-babel-do-load-languages 'org-babel-load-languages
                             '((sh . t)
                               (emacs-lisp . t)))

(setq org-confirm-babel-evaluate nil)

(setq org-todo-keywords '((sequence "TODO(t!)"
                                    "DOING(d!)"
                                    "WAITING(w@/!)"
                                    "|"
                                    "DONE(D!)")))

;; general ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package general
  :ensure t
  :config
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
   "C-S-k" 'mpereira/describe-thing-at-point
   "K" 'mpereira/describe-thing-at-point-in-popup)

  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   :states '(normal)
   :prefix mpereira/leader
   "ee" 'mpereira/eval-sexp-at-or-surrounding-pt
   "e(" 'eval-defun
   "eE" 'eval-buffer
   "e:" 'eval-expression)

  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   :states '(visual)
   :prefix mpereira/leader
   "ee" 'eval-region)

  (general-define-key
   "M-F" 'toggle-frame-fullscreen
   "M-+" 'text-scale-increase
   "M--" 'text-scale-decrease)

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
   "<escape>" 'minibuffer-keyboard-quit))

;; company-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; ansi-term ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make esc work from terminal emacs.
;; https://github.com/chrisdone/god-mode/issues/43#issuecomment-67193877
;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2013-07/msg00209.html
;; (defvar personal/fast-keyseq-timeout 200)

;; (defun mpereira/-tty-ESC-filter (map)
;;   (if (and (equal (this-single-command-keys) [?\e])
;;            (sit-for (/ mpereira/fast-keyseq-timeout 1000.0)))
;;       [escape] map))

;; (defun mpereira/-lookup-key (map key)
;;   (catch 'found
;;     (map-keymap (lambda (k b) (if (equal key k) (throw 'found b))) map)))

;; (defun mpereira/catch-tty-ESC ()
;;   "Setup key mappings of current terminal to turn a tty's ESC into `escape'."
;;   (when (memq (terminal-live-p (frame-terminal)) '(t pc))
;;     (let ((esc-binding (mpereira/-lookup-key input-decode-map ?\e)))
;;       (define-key input-decode-map
;;         [?\e] `(menu-item "" ,esc-binding :filter mpereira/-tty-ESC-filter)))))

;; (mpereira/catch-tty-ESC)

(defun term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))

(defun term-toggle-mode ()
  "Toggles term between line mode and char mode"
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

(general-define-key
 :keymaps '(term-mode-map term-raw-map)
 :states '(emacs)
 "<escape>" 'term-send-esc
 "C-t" 'term-toggle-mode
 ;; "<return>" 'term-send-foo
 )

(general-define-key
 :keymaps 'term-mode-map
 :states '(insert normal visual)
 "C-t" 'term-toggle-mode
 ;; "<return>" 'term-send-foo
 )

(general-define-key
 :keymaps 'term-raw-map
 :states '(insert normal visual)
 "C-t" 'term-toggle-mode
 ;; "<return>" 'term-send-foo
 )

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
   "gip" 'gist-region-or-buffer-private
   "gii" 'gist-region-or-buffer
   "gil" 'gist-list)

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

;; markdown-mode

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
  (projectile-global-mode)

  (setq projectile-enable-caching nil)
  (setq projectile-require-project-root nil)

  (general-define-key
   :states '(normal)
   :prefix mpereira/leader
   "sh" 'projectile-run-term
   "sc" 'projectile-run-async-shell-command-in-root))

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
   "pp" 'persp-switch-last))

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

  (setq ivy-height 20)
  (setq ivy-wrap t)

  ;; FIXME: the following does change ivy-occur buffers to wgrep mode but it is
  ;; nonfunctional.
  ;; (add-hook 'ivy-occur-grep-mode-hook 'ivy-wgrep-change-to-wgrep-mode)

  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line
   "C-f" 'ivy-scroll-up-command
   "C-b" 'ivy-scroll-down-command
   "C-o" 'ivy-occur
   "C-h" 'ivy-beginning-of-buffer
   "C-l" 'ivy-end-of-buffer
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
   "fb" 'ivy-switch-buffer
   "ff" 'counsel-find-file
   "fk" 'describe-keymap
   "fl" 'counsel-find-library
   "fn" 'counsel-describe-function
   "fp" 'package-list-packages-no-fetch
   "fv" 'counsel-describe-variable
   "fy" 'counsel-yank-pop))

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
   "ps" 'mpereira/ivy-persp-switch-project
   "pb" 'counsel-projectile-switch-to-buffer
   "pf" 'counsel-projectile-find-file
   "pg" 'counsel-projectile-ag)

  (general-define-key
   :states '(normal visual)
   "/" 'evil-search-forward
   "?" 'evil-search-backward)

  ;; (general-define-key
  ;;  :states '(normal visual)
  ;;  "/" 'swiper
  ;;  "?" 'swiper)

  ;; FIXME: n and N are reversed after swiper?
  ;; (general-define-key
  ;;  :states '(normal visual)
  ;;  "n" 'evil-search-previous
  ;;  "N" 'evil-search-next)
  )

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
   "pt" 'neotree-project-dir)

  (general-define-key
   :keymaps 'neotree-mode-map
   :states '(normal visual)
   :prefix mpereira/leader
   "pt" 'neotree-hide)

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
  :ensure t)

;; magit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :config
  (add-hook 'with-editor-mode-hook 'evil-insert-state)

  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)

  (general-define-key
   :states '(normal)
   :prefix mpereira/leader
   "gb" 'magit-blame
   "gc" 'magit-commit-popup
   "gd" 'magit-diff-buffer-file
   "gD" 'magit-diff-unstaged
   "gl" 'magit-log-buffer-file
   "gL" 'magit-log-all
   "gp" 'magit-push-popup
   "gs" 'magit-status
   "gw" 'magit-stage-file
   "gW" 'magit-stage-modified
   "g<" 'smerge-keep-mine
   "g>" 'smerge-keep-other)

  ;; FIXME: workaround for mode line vc branch information not updating with
  ;; branch changes.
  (defun mpereira/redraw ()
    (interactive)
    (vc-refresh-state)
    (redraw-modeline)
    (redraw-display))

  (general-define-key
   :states '(normal)
   "C-l" 'mpereira/redraw))

;; Doesn't seem to be working?
;; Update vc mode line information after checking out branches.
;; https://github.com/magit/magit/wiki/magit-update-uncommitted-buffer-hook
;;   (defvar magit--modified-files nil)

;;   (defun magit-maybe-cache-modified-files ()
;;     "Maybe save a list of modified files.
;; That list is later used by `magit-update-uncommitted-buffers',
;; provided it is a member of `magit-post-refresh-hook'.  If it is
;; not, then don't save anything here."
;;     (when (memq 'magit-update-uncommitted-buffers magit-post-refresh-hook)
;;       (setq magit--modified-files (magit-modified-files t))))

;;   (add-hook 'magit-pre-refresh-hook #'magit-maybe-cache-modified-files)
;;   (add-hook 'magit-pre-call-git-hook #'magit-maybe-cache-modified-files)
;;   (add-hook 'magit-pre-start-git-hook #'magit-maybe-cache-modified-files)

;;   (defun magit-update-uncommitted-buffers ()
;;     "Update some file-visiting buffers belonging to the current repository.
;; Run `magit-update-uncommitted-buffer-hook' for each buffer
;; which visits a file inside the current repository that had
;; uncommitted changes before running the current Magit command
;; and/or that does so now."
;;     (let ((topdir (magit-toplevel)))
;;       (dolist (file (delete-consecutive-dups
;;                      (sort (nconc (magit-modified-files t)
;;                                   magit--modified-files)
;;                            #'string<)))
;;         (--when-let (find-buffer-visiting (expand-file-name file topdir))
;;           (with-current-buffer it
;;             (run-hooks 'magit-update-uncommitted-buffer-hook))))))

;;   (add-hook 'magit-post-refresh-hook #'magit-update-uncommitted-buffers)
;;   (add-hook 'magit-update-uncommitted-buffer-hook 'vc-refresh-state))

;; magit-gh-pulls ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit-gh-pulls
  :ensure t
  :after magit
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

;; magithub ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Worse than magit-gh-pulls somehow. Check again in the future?

;; (use-package magithub
;;   :ensure nil
;;   :after magit
;;   :config (magithub-feature-autoinject t))

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
    (add-hook 'evil-org-mode-hook
              (lambda ()
                (evil-org-set-key-theme
                 '(operators navigation textobjects shift todo heading))
                ;; FIXME: evil-org-open-below doesn't work and
                ;; evil-org-open-above seems to do what evil-org-open-below
                ;; should be doing.
                (general-define-key
                 :keymaps 'evil-org-mode-map
                 :states '(normal)
                 "o" 'evil-open-below
                 "O" 'evil-open-above)))
    ;; FIXME: what's the mode for org todo notes?
    (evil-set-initial-state 'org-note-mode 'insert))

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
     "C-j" 'git-rebase-move-line-down
     "C-k" 'git-rebase-move-line-up))

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
    ;; highlighed with `diff-removed` face which is typically some red color (as
    ;; defined by the color theme) other faces such as `diff-added` will be used
    ;; for other actions.
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
