(setq debug-on-error t)

(add-hook 'after-init-hook
          (lambda ()
            (setq debug-on-error nil)))

(require 'package)

(setq package-enable-at-startup nil)

(package-initialize)

(setq package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ;; Mostly for `org-contrib'.
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))

;; NOTE: `eval-when-compile' because `use-package' is not longer needed at at
;; runtime.
;; https://github.com/jwiegley/use-package#use-packageel-is-no-longer-needed-at-runtime
(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)
(setq use-package-always-demand t)

(setenv "LSP_USE_PLISTS" "true")

(setq mpereira/leader ",")

(use-package general
  :custom
  (use-package-hook-name-suffix . nil))

(use-package which-key
  :diminish
  :custom
  (which-key-idle-delay 0.5)
  :config
  (which-key-mode 1))

(load-library (expand-file-name "secrets.el.gpg" user-emacs-directory))

(use-package no-littering)

(use-package emacs
  :custom
  (confirm-kill-emacs 'y-or-n-p)
  ;; REVIEW: I'm not sure if I need this, but it's a possible
  ;; workaround for an lsp-mode issue:
  ;; https://github.com/emacs-lsp/lsp-mode/issues/4112
  (backup-by-copying t)
  (use-short-answers t)
  (frame-resize-pixelwise t)
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  (inhibit-startup-screen t)
  ;; Don't play beep when quitting, etc.
  (ring-bell-function 'ignore)
  ;; By default Emacs thinks a sentence is a full-stop followed by 2
  ;; spaces. Make it a full-stop and 1 space.
  (sentence-end-double-space nil)
  ;; Keep cursor position when scrolling.
  (scroll-preserve-screen-position t)
  ;; Tab first tries to indent the current line, and if the line was
  ;; already indented, then try to complete the thing at point.
  (tab-always-indent 'complete)
  ;; Make TABs be displayed with a width of 2.
  (tab-width 2)
  ;; Force packages relying on this general indentation variable
  ;; (e.g., lsp-mode) to indent with 2 spaces.
  (standard-indent 2)
  ;; Show trailing whitespace.
  (show-trailing-whitespace t)
  ;; Make cursor the width of the character it is under e.g. full
  ;; width of a TAB.
  (x-stretch-cursor t)
  ;; Week start on monday.
  (calendar-week-start-day 1)
  ;; Don't pop up buffer showing native compilation warnings/errors.
  (native-comp-async-report-warnings-errors nil)
  (world-clock-list '(("Europe/Berlin" "Munich")
                      ("America/Sao_Paulo" "São Paulo")
                      ("Asia/Bangkok" "Bangkok")))

  :config
  (load custom-file 'noerror)
  (column-number-mode)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (blink-cursor-mode -1)
  ;; Highlight current line.
  (global-hl-line-mode t)
  ;; Remember file cursor positions.
  (save-place-mode 1)
  ;; Remember minibuffer history.
  (savehist-mode 1)
  ;; Prevent tabs from being inserted when formatting buffers.
  ;; https://www.gnu.org/software/emacs/manual/html_node/eintr/Indent-Tabs-Mode.html
  (setq-default indent-tabs-mode nil))

(defun mpereira/show-trailing-whitespace-maybe-disable ()
  "Disable `show-trailing-whitespace' in selected modes."
  (when (derived-mode-p 'shell-mode
                        'eshell-mode)
    (setq-local show-trailing-whitespace nil)))

(add-hook 'after-change-major-mode-hook
          'mpereira/show-trailing-whitespace-maybe-disable)

(use-package prot
  :defer t
  :vc (prot :url "https://github.com/protesilaos/dotfiles"
            :lisp-dir "emacs/.emacs.d/prot-lisp/"))

(require 'prot-common)
(require 'prot-modeline)

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(require 'mode-line-right-align)
(require 'mpereira-mode-line)
(require 'mpereira-html-escape-unescape)
(require 'mpereira-lisp-indent-function)
(mpereira-mode-line-mode 1)

(use-package modus-themes)

(setq mpereira/light-theme 'modus-operandi)
(setq mpereira/dark-theme 'modus-vivendi-tinted)
(setq mpereira/initial-theme mpereira/light-theme)

(defun mpereira/load-light-theme ()
  "Load the light theme specified by `mpereira/light-theme'."
  (interactive)
  (consult-theme mpereira/light-theme))

(defun mpereira/load-dark-theme ()
  "Load the dark theme specified by `mpereira/dark-theme'."
  (interactive)
  (consult-theme mpereira/dark-theme))

(add-hook 'after-init-hook
          (lambda () (consult-theme mpereira/initial-theme))
          'append)

(add-hook 'ns-system-appearance-change-functions
          (lambda (appearance)
            (pcase appearance
              ('light (mpereira/load-light-theme))
              ('dark (mpereira/load-dark-theme)))))

(require 'cl-lib)

(fset #'remove-from-list #'cl-delete)

(defun mpereira/symbol-at-point-as-string ()
  "Return current symbol at point as a string."
  (let ((s (thing-at-point 'symbol)))
    (and (stringp s)
         (if (string-match "\\`[`']?\\(.*?\\)'?\\'" s)
             (match-string 1 s)
           s))))

(defun mpereira/epoch-at-point-to-timestamp ()
  "Convert the epoch time at point to a human-readable timestamp.

The function reads the symbol at point, interprets it as
the number of seconds since the Unix epoch, converts it to
a timestamp in the format 'YYYY-MM-DD DDD HH:MM:SS',
places this timestamp in the kill ring, and displays it
in the echo area. If no valid symbol is found at point, do nothing."
  (interactive)
  (if-let (thing (mpereira/symbol-at-point-as-string))
      (let* ((seconds (string-to-number thing))
             (time (seconds-to-time seconds))
             (timestamp (format-time-string "%Y-%m-%d %a %H:%M:%S" time)))
        (kill-new timestamp)
        (message timestamp)
        timestamp)))

(defun eshell-p (buffer)
  "Return t if BUFFER is an Eshell buffer."
  (with-current-buffer buffer
    (eq major-mode 'eshell-mode)))

(defun mpereira/yank-buffer-file-name ()
  "Copy the absolute path of the current buffer's file or directory to the kill ring.

If the buffer is visiting a file, its absolute file path is yanked.
If the buffer is in Eshell, the Eshell working directory is yanked.
The copied path is displayed in the echo area."
  (interactive)
  (let ((buffer-file-name* (if (eshell-p (current-buffer))
                               (eshell/pwd)
                             (buffer-file-name))))
    (kill-new buffer-file-name*)
    (message buffer-file-name*)
    buffer-file-name*))

(defun mpereira/yank-buffer-name ()
  "Copy the name of the current buffer to the kill ring.

The buffer name is placed in the kill ring, displayed in the
echo area, and returned."
  (interactive)
  (let ((buffer-name* (buffer-name)))
    (kill-new buffer-name*)
    (message buffer-name*)
    buffer-name*))

;; URL encoding/decoding
;; https://www.blogbyben.com/2010/08/handy-emacs-function-url-decode-region.html

(defun mpereira/url-decode-region (start end)
  "Replace a region with the same contents, only URL decoded."
  (interactive "r")
  (let ((text (url-unhex-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

(defun mpereira/url-encode-region (start end)
  "Replace a region with the same contents, only URL encoded."
  (interactive "r")
  (let ((text (url-hexify-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

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

(defun mpereira/call-interactively-with-prefix-arg (prefix-arg func)
  (let ((current-prefix-arg prefix-arg))
    (call-interactively func)))

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
  "Toggle between horizontal and vertical split for exactly two windows."
  (interactive)
  (if (not (= (count-windows) 2))
      (message "Can only toggle window split for 2 windows")
    (let* ((window1 (selected-window))
           (window2 (next-window))
           (buffer1 (window-buffer window1))
           (buffer2 (window-buffer window2))
           (edges1 (window-edges window1))
           (edges2 (window-edges window2))
           (split-dir (if (= (car edges1) (car edges2))
                          'split-window-horizontally
                        'split-window-vertically)))
      (delete-other-windows)
      (let ((new-win (funcall split-dir)))
        (set-window-buffer window1 buffer1)
        (set-window-buffer new-win buffer2)
        (select-window window1)))))

(defun mpereira/narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun, whichever
applies first. Narrowing to org-src-block actually calls
`org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing command. Remove this
         ;; first conditional if you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(defun mpereira/posframe-show-world-clock ()
  "Toggle the display of the world clock using a posframe."
  (interactive)
  (save-window-excursion
    (world-clock))
  (let* ((buffer (get-buffer "*wclock*"))
         (window (get-buffer-window buffer t)))
    (if (and window
             (frame-visible-p (window-frame window)))
        (posframe-delete buffer)
      (posframe-show buffer
                     :poshandler 'posframe-poshandler-frame-center
                     :internal-border-width mpereira/posframe-default-internal-border-width
                     :internal-border-color mpereira/posframe-default-internal-border-color
                     :override-parameters mpereira/default-posframe-override-parameters))))

;; NOTE: for `unfill-toggle'.
(use-package unfill)

(use-package eldoc
  :config
  (global-eldoc-mode -1))

;; Line numbers.
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'display-line-numbers-mode))
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

(with-eval-after-load "consult"
  (defun mpereira/corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data))))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-min-width 30)
  (corfu-max-width 100)
  (corfu-count 15)
  (corfu-popupinfo-delay '(0.5 . 0.5))
  :general
  (:keymaps '(corfu-map)
   :states '(insert)
   "C-/" 'corfu-insert-separator
   "C-h" 'corfu-first
   "C-l" 'corfu-last
   "C-b" 'corfu-scroll-down
   "C-f" 'corfu-scroll-up
   "M-o" #'mpereira/corfu-move-to-minibuffer
   "<escape>" 'corfu-quit)
  :hook
  (corfu-mode-hook . corfu-popupinfo-mode)
  (corfu-mode-hook . corfu-history-mode)
  :config
  (global-corfu-mode))

(use-package marginalia
  :general
  (:keymaps '(minibuffer-local-map)
   "M-A" 'marginalia-cycle)
  :config
  (marginalia-mode 1))

(setq mpereira/lsp-disabled-modes
      '(emacs-lisp-mode
        bazel-build-mode
        lisp-data-mode
        ;; NOTE: Python has a special hook than just `lsp'.
        python-mode))

(defun mpereira/maybe-enable-lsp ()
  "Enable LSP if the current major mode is not in `mpereira/lsp-disabled-modes'."
  (interactive)
  (when (not (-contains? mpereira/lsp-disabled-modes major-mode))
    (lsp)))

(use-package flycheck
  :general
  (:keymaps '(flycheck-mode-map)
   :states '(normal visual)
   :prefix mpereira/leader
   :infix "1"
   "c" 'flycheck-buffer
   "C" 'flycheck-clear
   "e" 'flycheck-explain-error-at-point
   "h" 'flycheck-display-error-at-point
   "j" 'flycheck-next-error
   "k" 'flycheck-previous-error
   "l" 'consult-flycheck
   "n" 'flycheck-next-error
   "p" 'flycheck-previous-error
   "y" 'flycheck-copy-errors-as-kill
   "?" 'flycheck-describe-checker))

(use-package consult-flycheck)

(use-package flycheck-posframe
  :custom
  (flycheck-posframe-border-width 1)
  :hook
  (flycheck-mode-hook . flycheck-posframe-mode))

(defun mpereira/lsp-ui-doc-toggle ()
  "Toggle the display of the LSP documentation frame.
If the documentation frame is visible, it will be hidden.
Otherwise, it will be shown."
  (interactive)
  (if (lsp-ui-doc--frame-visible-p)
      (lsp-ui-doc-hide)
    (lsp-ui-doc-show)))

(use-package lsp-mode
  :diminish lsp-lens-mode
  :custom
  (lsp-completion-provider :none)
  (lsp-copilot-enabled nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-diagnostics-scope :file)
  (lsp-progress-prefix "⧖ ")
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (gc-cons-threshold (* 100 1024 1024))      ; 100MB.
  (read-process-output-max (* 10 1024 1024)) ; 10MB.
  :general
  (:keymaps '(lsp-mode-map)
   :states '(normal visual)
   "C-9" #'mpereira/lsp-ui-doc-toggle
   "K" 'lsp-describe-thing-at-point)
  (:keymaps '(lsp-mode-map)
   :states '(normal)
   :prefix mpereira/leader
   "F" #'lsp-format-buffer)
  (:keymaps '(lsp-mode-map)
   :states '(visual)
   :prefix mpereira/leader
   "F" #'lsp-format-region)
  (:keymaps '(lsp-mode-map)
   :states '(normal visual)
   :prefix "g"
   "a" 'lsp-execute-code-action
   "A" 'lsp-ui-sideline-apply-code-actions
   "d" 'lsp-find-definition
   "E" 'lsp-find-references
   "F" 'lsp-format-buffer
   "e" 'lsp-ui-peek-find-references
   "r" 'lsp-rename
   "S" 'consult-lsp-symbols
   "s" 'consult-lsp-file-symbols
   "n" 'lsp-organize-imports)
  :hook
  (prog-mode-hook . mpereira/maybe-enable-lsp)
  (lsp-mode-hook . mpereira/disable-eldoc)
  :config
  (add-to-list 'lsp-language-id-configuration '(makefile-bsdmake-mode . "make")))

(use-package lsp-ui
  :defer t
  :custom
  (lsp-ui-doc-border (face-background 'vertico-posframe-border))
  (lsp-ui-doc-include-signature nil)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-max-width 100)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-peek-peek-height 40)
  :general
  ;; NOTE: binding this on visual/normal state causes the peek view to
  ;; be aborted on keypress.
  (general-define-key
   :keymaps '(lsp-ui-peek-mode-map)
    "C-j" 'lsp-ui-peek--select-next
    "C-k" 'lsp-ui-peek--select-prev)
  :config
  (setq lsp-ui-doc-frame-parameters
        (map-merge 'alist lsp-ui-doc-frame-parameters mpereira/default-posframe-override-parameters))
  (add-to-list 'lsp-ui-doc-frame-parameters '(clojure-mode . "clojure"))
  (custom-set-faces
   '(lsp-ui-doc-background ((t (:inherit vertico-posframe))))
   '(lsp-ui-doc-header ((t (:inherit vertico-posframe-border)))))

  ;; lsp-ui-peek in posframe:
  ;; - https://github.com/emacs-lsp/lsp-ui/issues/441
  ;; - https://github.com/kassick/dotfiles/commit/f930e5e9cc268267073fc1878ee265a4a7cb89b4
  ;; - https://github.com/seagle0128/.emacs.d/commit/a73a46ef3630e2492f5a0d70c32a59fead6c2ed6
  (defun lsp-ui-peek--peek-display (src1 src2)
    (-let* ((win-width (frame-width))
            (lsp-ui-peek-list-width (/ (frame-width) 2))
            (string (-some--> (-zip-fill "" src1 src2)
                      (--map (lsp-ui-peek--adjust win-width it) it)
                      (-map-indexed 'lsp-ui-peek--make-line it)
                      (-concat it (lsp-ui-peek--make-footer)))))
      (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
      (posframe-show lsp-ui-peek--buffer
                     :string (mapconcat 'identity string "")
                     :min-width (frame-width)
                     :poshandler #'posframe-poshandler-frame-center)))

  (defun lsp-ui-peek--peek-destroy ()
    (when (bufferp lsp-ui-peek--buffer)
      (posframe-delete lsp-ui-peek--buffer))
    (setq lsp-ui-peek--buffer nil
          lsp-ui-peek--last-xref nil)
    (set-window-start (get-buffer-window) lsp-ui-peek--win-start))

  (advice-add #'lsp-ui-peek--peek-new :override #'lsp-ui-peek--peek-display)
  (advice-add #'lsp-ui-peek--peek-hide :override #'lsp-ui-peek--peek-destroy))

(use-package lsp-pyright
  :custom
  (lsp-pyright-langserver-command "pyright")
  :hook
  (python-mode-hook . (lambda ()
                        (require 'lsp-pyright)
                        (lsp))))

(defun mpereira/jupyter-clear-highlights-and-overlays ()
  "Clear all highlights and overlays in the current buffer."
  (interactive)
  (evil-ex-nohighlight)
  (when (fboundp 'jupyter-eval-remove-overlays)
    (jupyter-eval-remove-overlays)))

(use-package jupyter
  :custom (jupyter-eval-use-overlays t)
  :hook
  ;; NOTE: https://github.com/emacs-jupyter/jupyter/issues/344
  (jupyter-interaction-mode-hook . (lambda ()
                                     (remove-hook 'completion-at-point-functions
                                                  'jupyter-completion-at-point
                                                  t)))
  :general
  (:keymaps '(jupyter-repl-interaction-mode-map)
   :states '(normal visual)
   :prefix mpereira/leader
   :infix "e"
   "e" 'jupyter-eval-line-or-region
   "E" 'jupyter-eval-buffer
   "(" 'jupyter-eval-defun)
  (:keymaps '(jupyter-repl-interaction-mode-map)
   :states '(normal visual)
   "C-l" 'mpereira/jupyter-clear-highlights-and-overlays))

(use-package avy
  :custom
  (avy-all-windows nil))

(use-package consult)
(use-package consult-lsp)
(use-package consult-project-extra)
(use-package consult-git-log-grep
  :custom
  (consult-git-log-grep-open-function #'magit-show-commit))

(use-package elec-pair
  :config
  (electric-pair-mode 1)
  ;; NOTE: elec-pair unconditionally binds C-j to
  ;; `electric-newline-and-maybe-indent' globally, so we remove it.
  (define-key global-map "\C-j" nil))

(use-package perspective
  :custom
  (persp-show-modestring nil)
  (persp-suppress-no-prefix-key-warning t)
  :init
  (persp-mode))

(use-package prodigy
  :general
  (:keymaps '(override)
   "M-p" #'prodigy)
  (:keymaps '(prodigy-mode-map)
   :states '(normal)
   "gr" 'prodigy-refresh
   "RET" 'prodigy-display-process))

(use-package evil
  :general
  (:keymaps '(evil-normal-state-map)
   "C-l" #'evil-ex-nohighlight)
  (:keymaps '(evil-motion-state-map)
   ";" #'evil-ex
   ":" #'evil-command-window-ex)
  ;; Return to original cursor position when cancelling search.
  (:keymaps '(isearch-mode-map)
   "<escape>" #'isearch-cancel)
  :custom
  (evil-v$-excludes-newline t)
  (evil-undo-system 'undo-tree)
  (evil-shift-width 2)
  (evil-jumps-cross-buffers nil)
  (evil-want-C-u-scroll t)
  (evil-search-module 'evil-search)
  ;; FIXME: this correctly causes '*' to match on whole symbols (e.g.,
  ;; on a Clojure file pressing '*' on 'foo.bar' matches the whole
  ;; thing, instead of just 'foo' or 'bar', BUT, it won't match
  ;; 'foo.bar' in something like '(foo.bar/baz)', which I don't like.
  (evil-symbol-word-search t)
  :init
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-keybinding nil)
  :config
  ;; NOTE: make underscore a word character.
  ;; https://evil.readthedocs.io/en/latest/faq.html#underscore-is-not-a-word-character
  (modify-syntax-entry ?_ "w")

  (evil-ex-define-cmd "pwd" #'mpereira/yank-buffer-file-name)
  (evil-ex-define-cmd "bD" #'mpereira/delete-file-and-buffer)
  ;; NOTE: this is originally `evil-window-delete', but I want the
  ;; window to remain.
  (evil-ex-define-cmd "bd" #'kill-this-buffer)

  (evil-mode 1))

(use-package evil-org
  :after evil org
  :config
  ;; evil-org unconditionally remaps `evil-quit' to `org-edit-src-abort' which I
  ;; don't like because it results in `evil-quit' keybinding invocations to not
  ;; quit the window.
  ;; (when (command-remapping 'evil-quit nil org-src-mode-map)
  ;;   (define-key org-src-mode-map [remap evil-quit] nil))
  :hook
  (org-mode-hook . evil-org-mode)
  (evil-org-mode-hook . (lambda ()
                          (evil-org-set-key-theme '(operators
                                                    navigation
                                                    textobjects))))
  :config
  ;; NOTE: overriding default which includes newline:
  ;; `evil-org-end-of-line', even though `evil-v$-excludes-newline' is
  ;; set to `t'.
  (evil-define-key 'motion 'evil-org-mode
    (kbd "$") 'evil-end-of-line))

(use-package evil-multiedit
  :after evil
  :custom
  (evil-multiedit-follow-matches t)
  :general
  (:keymaps '(global-map)
   :states '(normal)
   "C-n" 'evil-multiedit-match-and-next
   "C-p" 'evil-multiedit-match-and-prev
   "C-S-n" 'evil-multiedit-match-all)

  (:keymaps '(global-map)
   :states '(visual)
   "C-k" 'evil-multiedit-prev
   "C-j" 'evil-multiedit-next
   "C-n" 'evil-multiedit-match-symbol-and-next
   "C-p" 'evil-multiedit-match-symbol-and-prev
   "C-S-n" 'evil-multiedit-match-all)

  (:keymaps '(evil-multiedit-mode-map)
   :states '(insert)
   ;; NOTE: this is originally
   ;; `evil-multiedit-toggle-or-restrict-region', but I want to be
   ;; able to insert newlines when using evil-multiedit.
   "RET" nil)

  (:keymaps '(evil-multiedit-mode-map)
   :states '(normal)
   "RET" 'evil-multiedit-toggle-or-restrict-region
   "C-n" 'evil-multiedit-match-symbol-and-next
   "C-p" 'evil-multiedit-match-symbol-and-prev
   "C-k" 'evil-multiedit-prev
   "C-j" 'evil-multiedit-next)
  :config
  ;; NOTE: make matching case-sensitive.
  ;; https://github.com/hlissner/evil-multiedit/issues/48#issuecomment-1011418580
  (defun make-evil-multiedit-case-sensitive (fn &rest args)
    (let ((case-fold-search (not iedit-case-sensitive)))
      (apply fn args)))

  (advice-add #'evil-multiedit-match-and-next
              :around #'make-evil-multiedit-case-sensitive))

(use-package evil-collection
  :diminish (evil-collection-unimpaired-mode)
  :config
  (evil-collection-init))

(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-install))

(use-package evil-quickscope
  :after evil
  :config
  (global-evil-quickscope-mode 1))

(use-package evil-goggles
  :diminish
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-lion
  :after evil
  :config
  (evil-lion-mode))

(use-package embark
  :bind
  (("M-o" . embark-act)
   ("M-O" . embark-dwim)
   ("C-h B" . embark-bindings))
  ;; :general
  ;; (:keymaps '(grep-mode-map)
  ;; :states '(normal)
  ;; NOTE: this is originally `evil-search-next', which for whatever
  ;; reason doesn't seem to work correctly.
  ;; "n" 'evil-ex-search-next
  ;; "gr" 'embark-rerun-collect-or-export)
  ;; :config
  ;; (add-to-list 'embark-indicators 'embark-which-key-indicator)
  ;; (remove-from-list 'embark-mixed-indicator embark-indicators :test #'equal)
  ;; (advice-add #'embark-completing-read-prompter
  ;;             :around #'embark-hide-which-key-indicator)
  )

(use-package embark-consult)

(defun mpereira/vertico-next-page ()
  (interactive)
  (vertico-scroll-up 1))

(defun mpereira/vertico-previous-page ()
  (interactive)
  (vertico-scroll-down 1))

;; NOTE: required for `minibuffer-keyboard-quit'.
(require 'delsel)

(use-package vertico
  :bind (:map vertico-map
         ("C-f" . mpereira/vertico-next-page)
         ("C-b" . mpereira/vertico-previous-page)
         ("C-S-j" . vertico-next-group)
         ("C-S-k" . vertico-previous-group)
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-h" . vertico-first)
         ("C-l" . vertico-last)
         ("C-/" . orderless-filter)
         ("<escape>" . minibuffer-keyboard-quit)
         :map minibuffer-local-map
         ("<C-backspace>" . backward-kill-word))
  :hook (minibuffer-setup-hook . vertico-repeat-save)
  :custom
  (vertico-cycle t)
  (vertico-count 20)
  :config
  (vertico-mode 1))

(use-package vertico-posframe
  :config
  ;; NOTE: using `vertico-multiform-mode' as a workaround to conditionally
  ;; disable posframe for `consult-line'.
  (vertico-multiform-mode 1)
  :custom
  (vertico-multiform-commands '((consult-line (:not posframe))
                                (consult-ripgrep (:not posframe))
                                (t posframe))))

(setq mpereira/posframe-default-internal-border-color
      (face-attribute 'vertico-posframe-border :background nil t))
(setq mpereira/posframe-default-internal-border-width vertico-posframe-border-width)
(setq mpereira/default-posframe-override-parameters
      `((left-fringe . 8)
        (right-fringe . 8)
        (internal-border-color . ,mpereira/posframe-default-internal-border-color)
        (internal-border-width . ,mpereira/posframe-default-internal-border-width)
        (background-color . ,(face-attribute 'vertico-posframe :background nil t))
        (foreground-color . ,(face-attribute 'vertico-posframe :foreground nil t))))

(use-package which-key-posframe
  :after vertico
  ;; :vc (:url "https://github.com/emacsorphanage/which-key-posframe"
  ;;      :rev :newest)
  :vc (:fetcher github
       :repo "emacsorphanage/which-key-posframe")
  :custom
  (which-key-max-display-columns 2)
  (which-key-max-description-length 40)
  (which-key-posframe-border-width mpereira/posframe-default-internal-border-width)
  (which-key-posframe-parameters mpereira/posframe-default-internal-border-color)
  (which-key-posframe-poshandler 'posframe-poshandler-point-bottom-left-corner)
  :config
  (which-key-posframe-mode 1))

;; macOS modifiers.
(setq mac-command-modifier 'meta)
;; Setting "Option" to nil allows me to type umlauts with "Option+u".
(setq mac-option-modifier nil)
(setq mac-control-modifier 'control)
(setq ns-function-modifier 'hyper)

(defun mpereira/toggle-maximize-window (&optional centered-p)
  "Toggle maximizing the current window or restoring the window config.

If CENTERED-P is non-nil, enables `olivetti-mode' to center the buffer content."
  (interactive)
  (if (= 1 (length (window-list)))
      (progn
        (jump-to-register '_)
        (when centered-p
          (call-interactively 'olivetti-mode)))
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows)
      (when centered-p
        (progn
          (setq-local olivetti-body-width 100)
          (call-interactively 'olivetti-mode))))))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
       :rev :newest
       :branch "main")
  :hook (prog-mode-hook . copilot-mode)
  :general (:keymaps '(copilot-mode-map)
            :states '(insert)
            "C-;" #'copilot-accept-completion
            "C-:" #'copilot-accept-completion-by-line
            "C-j" #'copilot-next-completion
            "C-k" #'copilot-previous-completion)
  :custom
  (add-to-list 'copilot-indentation-alist '(lisp-data-mode . 2)))

(use-package comint
  :ensure nil
  :general
  (:keymaps '(comint-mode-map)
   :states '(insert)
   "C-l" 'comint-clear-buffer
   "C-k" 'comint-previous-input
   "C-j" 'comint-next-input))

(use-package chatgpt-shell
  :custom
  (chatgpt-shell-openai-key mpereira/secret-openai-secret-api-key)
  (chatgpt-shell-google-key mpereira/secret-google-gemini-api-key)
  (chatgpt-shell-anthropic-key mpereira/secret-emacs-anthropic-api-key)
  :general
  ("M-;" #'chatgpt-shell)
  ;; FIXME: this keybinding isn't being set in `comint-mode-map'. Why?
  (:keymaps '(chatgpt-shell-mode-map)
   :states '(insert)
   "C-l" 'comint-clear-buffer)
  (:keymaps '(chatgpt-shell-mode-map)
   :states '(normal)
   "C-k" (lambda (n)
           (interactive "p")
           (comint-previous-prompt n)
           (evil-scroll-line-to-top (line-number-at-pos)))
   "C-j" (lambda (n)
           (interactive "p")
           (comint-next-prompt n)
           (evil-scroll-line-to-top (line-number-at-pos))))
  (:keymaps '(chatgpt-shell-prompt-compose-mode-map)
   :states '(normal)
   "<" 'chatgpt-shell-prompt-compose-previous-interaction
   ">" 'chatgpt-shell-prompt-compose-next-interaction
   "C-c '" 'chatgpt-shell-view-block-at-point
   "C-j" 'chatgpt-shell-prompt-compose-next-item
   "C-k" 'chatgpt-shell-prompt-compose-previous-item
   "M-RET" 'chatgpt-shell-prompt-compose-reply)
  (:keymaps '(chatgpt-shell-prompt-compose-view-mode-map)
   :states '(normal)
   "C-j" 'chatgpt-shell-prompt-compose-next-item
   "C-k" 'chatgpt-shell-prompt-compose-previous-item)
  (:keymaps '(chatgpt-shell-prompt-compose-mode-map)
   :states '(insert)
   "M-RET" 'chatgpt-shell-prompt-compose-send-buffer))

(use-package gptel
  :custom
  (gptel-api-key mpereira/secret-openai-secret-api-key)
  (gptel-model 'gpt-4o))

(use-package gptel-quick
  :vc (:url "https://github.com/karthink/gptel-quick"
       :rev :newest))

(require 'mpereira-whisper)

;; IMPORTANT.
;;
;; For microphone access to work from macOS:
;;
;; 1. The following should be added to the Emacs.app plist:
;;
;;     <key>NSMicrophoneUsageDescription</key>
;;     <string>Emacs requires access to the microphone for module functionality.</string>
;;
;; 2. Emacs.app should be codesigned. For example:
;;
;;    (compile
;;     "codesign \
;;        --force \
;;        --deep \
;;        --sign - \
;;        /opt/homebrew/Cellar/emacs-head@29/29.2_1/Emacs.app")

(use-package whisper
  :vc (:url "https://github.com/natrys/whisper.el"
       :rev :newest)
  :general
  (:keymaps '(minibuffer-local-map)
   "M-r" 'whisper-run)
  (:keymaps '(global-map)
   :states '(insert)
   "M-r" 'whisper-run)
  :hook
  (whisper-after-transcription-hook . mpereira-whisper-kill-transcription)
  (whisper-before-transcription-hook . mpereira-system-audio-mute)
  (whisper-after-transcription-hook . mpereira-system-audio-unmute)
  :custom
  (whisper-model "base")
  (whisper-language "en")
  (whisper-translate nil)
  (whisper--ffmpeg-input-device (mpereira-whisper-select-default-audio-device
                                 "MacBook Pro Microphone"))
  (whisper-use-threads (/ (num-processors) 2)))

(use-package orderless
  :custom
  (orderless-matching-styles '(orderless-literal orderless-regexp))
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package evil-nerd-commenter
  :after evil)

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode t))

(use-package evil-string-inflection
  :after evil)

(use-package undo-tree
  :diminish
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  :hook
  (evil-local-mode-hook . turn-on-undo-tree-mode)
  :config
  (global-undo-tree-mode 1))

(use-package helpful)

;; Inspired by Prot's.
(defun mpereira/eshell-switch-to-last-output-buffer ()
  "Produce a buffer with output of last `eshell' command."
  (interactive)
  (let ((eshell-output (kill-region (eshell-beginning-of-output)
                                    (eshell-end-of-output))))
    (with-current-buffer (get-buffer-create "*last-eshell-output*")
      (erase-buffer)
      ;; TODO: do it with `insert' and `delete-region'?
      (yank)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;; Inspired by Prot's.
(defun mpereira/eshell-complete-redirect-to-buffer ()
  "Complete the syntax for appending to a buffer via `eshell'."
  (interactive)
  (end-of-line)
  (insert
   (concat " >>> #<" (read-buffer-to-switch "Redirect to buffer:") ">")))

(use-package eros
  :config
  (eros-mode 1))

(require 'thingatpt)

(defun mpereira/eval-thing-at-or-around-point ()
  "Evaluate thing at or surrounding the point."
  (interactive)
  (save-excursion
    (let* ((string-thing (thing-at-point 'string t))
           (symbol-thing (thing-at-point 'symbol t))
           (sexp-thing (thing-at-point 'sexp t)))
      (cond
       (string-thing
        (let* ((bounds (bounds-of-thing-at-point 'string))
               (string-value (eval (read string-thing))))
          (message "string: %s → %s" string-thing string-value)
          (eros--eval-overlay string-value (cdr bounds))))
       (symbol-thing
        (let* ((bounds (bounds-of-thing-at-point 'symbol))
               (symbol-value (eval (intern symbol-thing))))
          (message "symbol: %s → %s" symbol-thing symbol-value)
          (eros--eval-overlay symbol-value (cdr bounds))))
       (sexp-thing
        (let* ((bounds (bounds-of-thing-at-point 'sexp))
               (value (eval (read sexp-thing))))
          (message "sexp: %s → %s" sexp-thing value)
          (eros--eval-overlay value (cdr bounds))))))))

(defun mpereira/eval-buffer ()
  "Evaluate buffer and show confirmation message."
  (interactive)
  (eval-buffer)
  (message "%s" (buffer-name)))

(use-package evil-cleverparens
  :init
  ;; NOTE: this is because "additional movement keys" maps H and L.
  (setq evil-cleverparens-use-additional-movement-keys nil)
  :general
  (:keymaps '(evil-cleverparens-mode-map)
   :states '(normal visual)
   "(" 'evil-cp-backward-up-sexp
   "M-C" 'sp-clone-sexp
   "M-s" nil ; originally `sp-splice-sexp', but I want the avy keybinding.
   ))

;; Emacs lisp.
(use-package emacs
  :hook
  (emacs-lisp-mode-hook . evil-cleverparens-mode)
  :general
  (:keymaps '(emacs-lisp-mode-map)
   :states '(normal)
   "C-]" 'xref-find-definitions-other-window
   "K" 'helpful-at-point)

  (:keymaps '(emacs-lisp-mode-map)
   :states '(normal)
   :prefix mpereira/leader
   :infix "e"
   "b" #'edebug-set-breakpoint
   "d" #'edebug-defun
   "e" #'mpereira/eval-thing-at-or-around-point
   "(" #'eval-defun
   "E" #'mpereira/eval-buffer)

  (:keymaps '(emacs-lisp-mode-map)
   :states '(visual)
   :prefix mpereira/leader
   :infix "e"
   "e" 'eval-region))

(defun mpereira/lisp-data-check-parens ()
  (let ((position (point)))
    (save-excursion
      (goto-char (point-min))
      (condition-case err
          (scan-sexps (point-min) (point-max))
        (scan-error
         (goto-char (nth 2 err))
         (posframe-show "*unbalanced-parens-error*"
                        :string (format "Unbalanced parentheses near line %d"
                                        (line-number-at-pos))
                        :position position
                        :timeout 2
                        :background-color "#aa2200"
                        :foreground-color "white"
                        :internal-border-width 2)
         (error "Unbalanced parentheses near line %d" (line-number-at-pos)))))))

(defun mpereira/lisp-data-check-parens-before-save-setup ()
  "Check for balanced parentheses after saving Lisp-data buffers."
  (add-hook 'after-save-hook #'mpereira/lisp-data-check-parens nil t))

(add-hook 'lisp-data-mode-hook
          'mpereira/lisp-data-check-parens-before-save-setup)

;; Clojure.
(use-package clojure-ts-mode
  :hook
  (clojure-ts-mode-hook . evil-cleverparens-mode))

(use-package cider
  :hook
  (clojure-ts-mode-hook . cider-mode))

(defun mpereira/treesit-tsx-goto-matching ()
  "Jump to matching TSX delimiters or keywords using tree-sitter.

This function uses tree-sitter to navigate between matching
elements in code.

When called with point on various syntax elements, it jumps to
their matching counterpart.

Supported patterns include:

JavaScript/TypeScript:
- JSX tags: <tag> </tag>
- JSX expressions: {expr}
- Parentheses: ( )
- Curly braces: { }
- Square brackets: [ ]
- Function definitions: beginning/end
- Control flow:
  * if -> else
  * try -> catch
  * while/for loop boundaries

HTML/XML:
- Opening/closing tags: <tag> </tag>

The function does nothing when point is not on a supported syntax element."
  (interactive)
  (when-let* ((node (treesit-node-at (point)))
              (parent (treesit-node-parent node)))
    (let ((node-type (treesit-node-type node))
          (parent-type (treesit-node-type parent)))
      (cond
       ;; JSX elements.
       ((or (string= "jsx_opening_element" parent-type)
            (string= "jsx_closing_element" parent-type))
        (let* ((jsx-element (treesit-node-parent parent))
               (element-start (treesit-node-start jsx-element))
               (element-end (treesit-node-end jsx-element)))
          (if (< (point) (/ (+ element-start element-end) 2))
              (goto-char (1- element-end))
            (goto-char element-start))))

       ;; JSX expression containers.
       ((and (or (string= "{" node-type)
                 (string= "}" node-type))
             (string= "jsx_expression" parent-type))
        (if (string= "{" node-type)
            (goto-char (1- (treesit-node-end parent)))
          (goto-char (treesit-node-start parent))))

       ;; HTML/XML elements.
       ((or (string= "start_tag" parent-type)
            (string= "end_tag" parent-type))
        (let* ((element (treesit-node-parent parent))
               (element-start (treesit-node-start element))
               (element-end (treesit-node-end element)))
          (if (< (point) (/ (+ element-start element-end) 2))
              (goto-char (1- element-end))
            (goto-char element-start))))

       ;; Parentheses.
       ((and (or (string= "(" node-type)
                 (string= ")" node-type))
             (member parent-type
                     '("parenthesized_expression"
                       "arguments"
                       "formal_parameters")))
        (if (string= "(" node-type)
            (goto-char (1- (treesit-node-end parent)))
          (goto-char (treesit-node-start parent))))

       ;; Curly braces.
       ((and (or (string= "{" node-type)
                 (string= "}" node-type))
             (member parent-type
                     '("statement_block" "object" "class_body" "function_body")))
        (if (string= "{" node-type)
            (goto-char (1- (treesit-node-end parent)))
          (goto-char (treesit-node-start parent))))

       ;; Square brackets.
       ((and (or (string= "[" node-type)
                 (string= "]" node-type))
             (member parent-type '("array" "subscript_expression")))
        (if (string= "[" node-type)
            (goto-char (1- (treesit-node-end parent)))
          (goto-char (treesit-node-start parent))))

       ;; Function definitions.
       ((member parent-type
                '("function_declaration" "method_definition" "arrow_function"))
        (let ((function-start (treesit-node-start parent))
              (function-end (treesit-node-end parent)))
          (if (< (point) (/ (+ function-start function-end) 2))
              (goto-char (1- function-end))
            (goto-char function-start))))

       ;; Try-catch statements.
       ((or (string= node-type "try")
            (string= node-type "catch")
            (string= parent-type "try_statement"))
        (let* ((try-statement (if (string= parent-type "try_statement")
                                  parent
                                (treesit-node-parent parent)))
               (try-clause (treesit-node-child-by-field-name try-statement "body"))
               (catch-clause (treesit-node-child-by-field-name try-statement "handler"))
               (try-start (treesit-node-start try-statement))
               (catch-start (treesit-node-start catch-clause)))
          (if (>= (point) catch-start)
              (goto-char try-start)
            (goto-char catch-start))))

       ;; If-else statements.
       ((or (string= "if" node-type)
            (and (string= "else" node-type)
                 (string= "if_statement" (treesit-node-type
                                          (treesit-node-parent parent)))))
        (let* ((if-statement (if (string= "if" node-type)
                                 parent
                               (treesit-node-parent parent)))
               (alternative (treesit-node-child-by-field-name if-statement "alternative")))
          (if (string= "if" node-type)
              (when alternative
                ;; Go to the start of alternative and find the "else" keyword
                (goto-char (treesit-node-start alternative))
                (search-forward "else" (+ (point) 5) t))
            (goto-char (treesit-node-start if-statement)))))

       ;; Control flow structures.
       ((member parent-type
                '("while_statement" "for_statement"))
        (let ((statement-start (treesit-node-start parent))
              (statement-end (treesit-node-end parent)))
          (if (< (point) (/ (+ statement-start statement-end) 2))
              (goto-char (1- statement-end))
            (goto-char statement-start))))

       ;; String literals (including template literals).
       ((member parent-type '("string" "template_string"))
        (let ((string-start (treesit-node-start parent))
              (string-end (treesit-node-end parent)))
          (if (< (point) (/ (+ string-start string-end) 2))
              (goto-char (1- string-end))
            (goto-char string-start))))))))

(evil-define-motion mpereira/treesit-tsx-goto-matching-motion ()
  "Motion for `mpereira/treesit-tsx-goto-matching'."
  :type inclusive
  :jump t
  :keep-visual t
  (mpereira/treesit-tsx-goto-matching))

(define-minor-mode mpereira/treesit-tsx-goto-matching-mode
  "Minor mode for jumping between matching TSX delimiters using tree-sitter."
  :lighter " TSX-Match"
  (if mpereira/treesit-tsx-goto-matching-mode
      (progn
        (evil-local-set-key 'normal "%" 'mpereira/treesit-tsx-goto-matching)
        (evil-local-set-key 'visual "%" 'mpereira/treesit-tsx-goto-matching-motion))

    (let ((jump-item-fn (if (bound-and-true-p evil-matchit-mode-map)
                            'evilmi-jump-items
                          'evil-jump-item)))
      (evil-local-set-key 'normal "%" jump-item-fn)
      (evil-local-set-key 'normal "%" jump-item-fn))))

;; TypeScript.
(use-package emacs
  :mode
  ("\\.[t|j]sx?$" . typescript-ts-mode)
  :hook
  (typescript-ts-base-mode-hook . turn-off-evil-matchit-mode)
  (typescript-ts-base-mode-hook . mpereira/treesit-tsx-goto-matching-mode))

;; (lsp-install-server nil 'eslint)

;; (lsp-install-server nil 'graphql-lsp)
;; ;; https://github.com/emacs-lsp/lsp-mode/issues/3672
;; (let ((command "/usr/local/bin/npm")
;;       (args `("-g" "--prefix" ,(f-join lsp-server-install-dir "npm" "graphql-language-service-cli")
;;               "install" "graphql-language-service-cli")))
;;   (apply 'call-process command nil nil nil args))

;; (lsp-install-server nil 'tailwindcss)

(use-package lsp-biome
  :vc (:url "https://github.com/cxa/lsp-biome"
       :rev :newest
       :branch "main"))

(use-package lsp-tailwindcss
  :custom
  (lsp-tailwindcss-server-path (executable-find "tailwindcss-language-server"))
  ;; NOTE: this package and/or `lsp-tailwindcss-server' do something
  ;; dumb for commpletions which causes the server to always return
  ;; huge responses with all possible tailwindcss classes. Increasing
  ;; `gc-cons-threshold' and `read-process-output-max' by a lot is the
  ;; only way to not get Emacs stuck on completion requests.
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 10 1024 1024)) ; 10mb.
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  (setq lsp-tailwindcss-skip-config-check t))

(use-package yaml-mode
  :mode ("\\.ya?ml(?:\\.j2)?\\'" . yaml-mode))

(defun mpereira/maybe-enable-ansible-mode ()
  "Enable `ansible' mode if the current buffer is a YAML file within
roles or playbooks directories."
  (when (and (buffer-file-name)
             (string-match-p "\\(roles\\|playbooks\\)\\/.*\\.ya?ml\\'" (buffer-file-name)))
    (ansible-mode)))

(use-package ansible
  :hook
  (yaml-mode-hook . lsp-deferred)
  (yaml-mode-hook . mpereira/maybe-enable-ansible-mode))

(defun mpereira/terraform-disable-lsp-conflicting-keybindings ()
  "Disable lsp-mode keybindings that conflict with terraform-mode."
  (general-define-key
   :keymaps '(lsp-mode-map)
   :states '(normal)
   "gd" nil))

(use-package terraform-mode
  :config
  (add-hook 'terraform-mode-hook
            'mpereira/terraform-disable-lsp-conflicting-keybindings
            nil
            t)
  :general
  (:keymaps '(terraform-mode-map)
   :states '(normal)
   "gd" #'terraform-open-doc))

(use-package docker)
(use-package bazel)
(use-package jinja2-mode)
(use-package nginx-mode)

(use-package buffer-move)
(use-package olivetti)

(use-package tree-sitter
  :hook
  (prog-mode-hook . tree-sitter-mode)
  :config
  (dolist (mode-pair '((python-ts-mode . python)
                       (typescript-ts-mode . tsx)
                       (tsx-ts-mode . tsx)
                       (clojure-ts-mode . clojure)))
    (add-to-list 'tree-sitter-major-mode-language-alist mode-pair)))

(use-package tree-sitter-langs)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))

(use-package treesit-fold
  :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold"
       :rev :newest)
  :general
  (:keymaps '(treesit-fold-mode-map)
   :states '(normal visual)
   "TAB" #'treesit-fold-open-recursively
   "S-<tab>" #'mpereira/treesit-fold-toggle-all))

(defun mpereira/treesit-fold-toggle-all ()
  "Toggle all folds in the current buffer."
  (interactive)
  (if-let ((nodes (treesit-fold--overlays-in 'invisible 'treesit-fold)))
      (treesit-fold-open-all)
    (treesit-fold-close-all)))

(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate"
       :rev :newest)
  :custom
  (combobulate-flash-node nil)
  :hook
  (prog-mode-hook . combobulate-mode)
  :general
  (:keymaps '(combobulate-key-map)
   :states '(normal visual)
   "+" #'combobulate-mark-node-dwim
   "{" #'combobulate-navigate-beginning-of-defun
   "}" #'combobulate-navigate-end-of-defun
   "(" #'combobulate-navigate-up
   ")" #'combobulate-navigate-down
   "B" #'combobulate-navigate-logical-previous
   "C-j" #'combobulate-navigate-next
   "C-k" #'combobulate-navigate-previous
   "E" #'combobulate-navigate-logical-next
   "W" #'combobulate-navigate-logical-next
   "C-S-j" #'combobulate-drag-down
   "C-S-k" #'combobulate-drag-up)

  (:keymaps '(combobulate-key-map)
   :states '(normal visual)
   :prefix mpereira/leader
   "r" #'combobulate-splice-up
   "m" #'combobulate-mark-node-dwim
   "R" #'combobulate-vanish-node
   "k" #'combobulate-kill-node-dwim
   "(" #'combobulate-envelop-tsx-ts-mode-wrap-parentheses
   "<" #'combobulate-envelop-tsx-ts-mode-tag
   "{" #'combobulate-envelop-tsx-ts-mode-expression
   "c" #'combobulate-clone-node-dwim))

(defun mpereira/magit-center-buffer-contents ()
  "Center the buffer contents by setting `olivetti-body-width` to
120 and enabling `olivetti-mode`."
  (interactive)
  (setq-local olivetti-body-width 120)
  (olivetti-mode))

(defun mpereira/magit-center-buffer-contents-and-toggle-margin-if-in-magit-log-mode ()
  "Center the buffer contents and toggle the margin if the current
mode is `magit-log-mode`."
  (interactive)
  (when (derived-mode-p 'magit-log-mode)
    (mpereira/magit-center-buffer-contents)
    (if (magit-buffer-margin-p)
        (magit-toggle-margin))))

(use-package magit
  :custom
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function 'magit-restore-window-configuration)
  ;; Show recent branches/tags on top on commands like `magit-push'
  ;; "elsewhere".
  ;; https://github.com/magit/magit/issues/2872#issuecomment-291011191
  (magit-list-refs-sortby "-creatordate")
  ;; Ediff starts by default in a new frame. Don't do that.
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :general
  (:keymaps '(magit-mode-map)
   :states '(normal)
   "(" 'magit-section-up
   "<" #'magit-go-backward
   ">" #'magit-go-forward
   "?" #'evil-ex-search-backward
   "C-j" 'magit-section-forward-sibling
   "C-k" 'magit-section-backward-sibling
   "H" 'evil-window-top
   "L" 'evil-window-bottom
   "TAB" 'magit-section-cycle     ; originally `magit-section-toggle'.
   "Z" #'magit-stash
   "zb" #'evil-scroll-line-to-bottom
   "zt" #'evil-scroll-line-to-top
   "zz" #'evil-scroll-line-to-center)
  (:keymaps '(magit-status-mode-map)
   "e" #'ignore ; originally `magit-ediff-dwim'. I keep pressing this accidentally.
   )
  :config
  (transient-bind-q-to-quit)

  (add-hook 'magit-setup-buffer-hook
            'mpereira/magit-center-buffer-contents-and-toggle-margin-if-in-magit-log-mode
            100)
  :hook
  (magit-status-mode-hook . mpereira/magit-center-buffer-contents)
  (git-commit-setup-hook . evil-insert-state))

;; NOTE: a fine-grained personal access token with the following
;; permissions seems to be sufficient:
;;
;; | Contents      | Read-only      |
;; | Issues        | Read and write |
;; | Metadata      | Read-only      |
;; | Pull requests | Read and write |

(use-package forge)

(use-package igist)

(use-package persp-project
  ;; :vc (:url "https://github.com/PauloPhagula/persp-project"
  ;;      :rev :newest)
  :vc (:fetcher github
       :repo "PauloPhagula/persp-project"))

(use-package dwim-shell-command)

(defun mpereira/dired-yank-file-path-at-point ()
  "Yank the absolute path of the file at point in dired."
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (when filename
      (let ((path (expand-file-name filename (dired-current-directory))))
        (message path)
        (kill-new path)))))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
  (dired-vc-rename-file t)
  :hook
  (dired-mode-hook . dired-hide-details-mode)
  :general
  (:keymaps '(dired-mode-map)
   :states '(normal)
   "(" 'dired-subtree-up
   ";" nil ; originally the first keystroke for encryption-related bindings.
   "C-9" 'dired-hide-details-mode
   "C-j" 'dired-subtree-next-sibling
   "C-k" 'dired-subtree-previous-sibling
   "M-c" 'dired-ranger-copy
   "M-v" 'dired-ranger-paste
   "o" 'hydra-dired-quick-sort/body)
  (:keymaps '(dired-mode-map)
   :states '(normal)
   :prefix mpereira/leader
   :infix "y"
   "y" 'mpereira/dired-yank-file-path-at-point))

(use-package dired-ranger)

(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar))

(use-package dired-quick-sort
  :config
  (dired-quick-sort-setup))

(defun mpereira/eshell-clear ()
  "Clears buffer while preserving input."
  (interactive)
  (let* ((inhibit-read-only t)
         (input (eshell-get-old-input)))
    (eshell/clear-scrollback)
    (eshell-emit-prompt)
    (insert input)))

(use-package wgrep)

(use-package rg
  :general (:keymaps '(rg-mode-map)
            :states '(normal)
            "<" 'rg-back-history
            ">" 'rg-forward-history
            "C-j" 'rg-next-file
            "C-k" 'rg-prev-file))


(defun mpereira/eshell-append-last-command-to-history ()
  "Append the last Eshell command to the Eshell history file.

This function ensures that the most recently executed command in
the Eshell session is written to the history file immediately
after execution. It creates a temporary history ring containing
only the latest command and writes it to the specified history
file."
  (when eshell-history-ring
    (let ((last-command-eshell-history-ring (make-ring 1)))
      (ring-insert last-command-eshell-history-ring
                   (car (ring-elements eshell-history-ring)))
      (let ((eshell-history-ring last-command-eshell-history-ring))
        (eshell-write-history eshell-history-file-name t)))))

(use-package eshell
  :custom
  (eshell-banner-message "")
  (eshell-hist-ignoredups t)
  (eshell-history-size 1000000)
  (eshell-buffer-maximum-lines 50000)
  :general
  (:keymaps 'eshell-mode-map
   "C-S-k" #'mpereira/eshell-switch-to-last-output-buffer
   "C-S-n" (lambda ()
             (interactive)
             (mpereira/call-interactively-with-prefix-arg
              '(4)
              'eshell-show-output)))
  (:keymaps 'eshell-mode-map
   :states '(insert)
   "C-." #'mpereira/eshell-complete-redirect-to-buffer
   "C-j" #'eshell-next-matching-input-from-input
   "C-k" #'eshell-previous-matching-input-from-input
   "C-l" #'mpereira/eshell-clear
   "C-/" #'consult-history)
  :config
  (require 'em-alias)
  (require 'em-hist)
  (eshell/alias "e" "find-file $1")

  ;; Make eshell append to history after each command.
  ;; https://emacs.stackexchange.com/questions/18564/merge-history-from-multiple-eshells
  ;;
  ;; I tried making eshell append to history via
  ;; `(eshell-write-history nil t)' when:
  ;;
  ;; 1. eshell buffers were killed (via `eshell-exit-hook') and
  ;; 2. Emacs exited (via `emacs-kill-hook')
  ;;
  ;; This strategy doesn't work when there are multiple eshell
  ;; buffers. When the first eshell buffer calls
  ;; `(eshell-write-history nil t)` things look good (i.e., the
  ;; buffer's commands are appended to the history file), but when the
  ;; second eshell buffer does the same, the history file's contents
  ;; gets duplicated.
  ;;
  ;; Just appending to the history file after every command seems to
  ;; be the best strategy.
  (setq eshell-save-history-on-exit nil)
  (add-hook 'eshell-pre-command-hook #'mpereira/eshell-append-last-command-to-history))

(use-package eat
  :hook
  (eshell-mode-hook . eat-eshell-mode))

(use-package hide-mode-line)

(use-package winner
  :config
  (winner-mode 1))

(use-package amx)

(setq mpereira/cloud-synced-directory
      (file-name-as-directory
       (expand-file-name
        "~/Library/Mobile Documents/com~apple~CloudDocs/")))

(use-package org-contrib
  :custom
  (org-expiry-inactive-timestamps t)
  :hook
  (org-insert-heading-hook . org-expiry-insert-created)
  ;; (org-capture-before-finalize-hook . org-expiry-insert-created)
  :config
  (require 'org-expiry)
  ;; REVIEW: disabled until this hits `org-contrib' because it breaks:
  ;; https://lists.sr.ht/~bzg/dev/%3C20241029052437.242454-2-me@bcc32.com%3E
  ;; (org-expiry-insinuate)
  )

(defun mpereira/org-insert-heading-before-current ()
  "`org-insert-heading' will break the current heading unless the
pointer is at the beginning of the line. This fixes that.

Also check out `org-insert-heading-respect-content'."
  (interactive)
  (move-beginning-of-line nil)
  (org-insert-heading))

(use-package org
  :hook
  (org-insert-heading-hook . org-id-get-create)
  :custom
  (org-startup-folded 'fold)
  (org-directory (expand-file-name "org" mpereira/cloud-synced-directory))
  ;; Shorter ellipsis than three dots.
  (org-ellipsis "…")
  ;; Indent content at the outline level.
  (org-adapt-indentation t)
  ;; Make images have a consistent width.
  (org-image-actual-width 640)
  ;; Don't indent src block content.
  (org-edit-src-content-indentation 0)
  ;; Open src buffer in current window. Why isn't this controlled by
  ;; `display-buffer-alist'?
  (org-src-window-setup 'current-window)
  ;; Org babel asks for confirmation by default.
  (org-confirm-babel-evaluate nil)
  ;; Open indirect buffer in the same window.
  (org-indirect-buffer-display 'current-window)
  (org-todo-keywords '((sequence "TODO(t!)"
                                 "DOING(d!)"
                                 "NEXT(n!)"
                                 "WAITING(w@/!)"
                                 "|"
                                 "SOMEDAY(s@/!)"
                                 "DONE(D!)"
                                 "CANCELLED(c@/!)")))
  :init
  (add-to-list 'org-modules 'org-expiry)
  ;; Expansion snippets like <s.
  (add-to-list 'org-modules 'org-tempo)
  :general
  (:keymaps '(org-mode-map)
   :states '(normal)
   "t" 'org-todo
   "T" 'mpereira/org-insert-heading-before-current
   "M-t" 'org-insert-heading-after-current
   "(" 'org-up-element
   ")" 'org-down-element
   "k" 'evil-previous-visual-line
   "j" 'evil-next-visual-line)
  (:keymaps '(org-mode-map)
   :states '(normal)
   :prefix mpereira/leader
   :infix "o"
   "b" 'org-tree-to-indirect-buffer)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t)
                                 (emacs-lisp . t)
                                 (python . t)
                                 (verb . t)
                                 (jupyter . t))))

(use-package verb
  :general
  (:keymaps '(org-mode-map)
   :states '(normal)
   :prefix mpereira/leader
   :infix "e"
   "e" verb-command-map)
  (:keymaps '(verb-command-map)
   :states '(normal)
   :prefix mpereira/leader
   :infix "e"
   "e" verb-command-map))

(use-package org-make-toc)

(use-package org-web-tools
  :general
  (:keymaps '(org-mode-map)
   :states '(normal insert)
   "M-w" 'org-web-tools-insert-link-for-url
   "M-W" 'org-web-tools-insert-web-page-as-entry))

(use-package ibuffer
  :custom
  (ibuffer-formats '((mark modified read-only locked " "
                           (name 50 50 :left :elide)
                           " "
                           (size 9 -1 :right)
                           " "
                           (mode 16 16 :left :elide)
                           " " filename-and-process)
                     (mark " "
                           (name 16 -1)
                           " " filename)))
  :general
  (:keymaps '(ibuffer-mode-map)
   :states '(normal)
   "," nil ; originally `ibuffer-toggle-sorting-mode' from evil-collection.
   "N" nil ; originally `ibuffer-do-shell-command-pipe-replace'.
   ))

(use-package emacs
  ;; NOTE: using keymap `override' so that these keybindings override
  ;; any existing keybindings on the current mode.
  ;; Check out `general-override-mode-map'.
  :general
  (:keymaps '(override)
   :states '(normal visual insert)
   "M-=" #'global-text-scale-adjust
   "M-H" #'buf-move-left
   "M-J" #'buf-move-down
   "M-K" #'buf-move-up
   "M-L" #'buf-move-right
   "M-h" #'evil-window-left
   "M-j" #'evil-window-down
   "M-k" #'evil-window-up
   "M-l" #'evil-window-right
   "M-q" 'unfill-toggle
   ;; "C-w =" #'balance-windows              ; this is the default keybinding.
   "<C-left>" #'winner-undo
   "<C-right>" #'winner-redo)


  ;; M-u for universal argument ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (:keymaps 'universal-argument-map
   "M-u" 'universal-argument-more)
  (:keymaps 'override
   :states '(normal motion emacs insert visual)
   "M-u" 'universal-argument)

  ;; ⌘ bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (:keymaps '(global-map)
   :states '(normal visual)
   "C-/" #'mpereira/posframe-show-world-clock
   "M-s" #'avy-goto-char-timer
   "M-M" #'mpereira/toggle-maximize-window
   "M-N" (lambda () (interactive) (mpereira/toggle-maximize-window t)))

  ;; Non-leader "master" bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (:keymaps '(global-map)
   "M-X" 'amx-major-mode-commands)

  ;; Non-leader "g" ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (:keymaps '(global-map)
   :states '(motion)
   :prefix "g"
   "-" #'evil-operator-string-inflection
   "c" #'evilnc-comment-operator)

  ;; Leader bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (:states '(normal visual)
   :prefix mpereira/leader
   "," #'evil-switch-to-windows-last-buffer
   "." #'vertico-repeat
   "/" #'consult-line
   "=" #'quick-calc
   "?" #'world-clock
   "B" #'ibuffer
   "b" #'switch-to-buffer
   "F" #'link-hint-open-link
   "hs" #'mpereira/split-window-below-and-switch
   "hv" #'mpereira/toggle-window-split
   "n" #'mpereira/narrow-or-widen-dwim
   "q" #'evil-quit
   "T" #'bm-toggle
   "u" #'undo-tree-visualize
   "yf" #'link-hint-copy-link
   "|" #'olivetti-mode
   "vh" #'mpereira/toggle-window-split
   "vs" #'mpereira/split-window-right-and-switch
   "w" #'save-buffer
   "yy" #'mpereira/yank-buffer-name
   "Y" #'mpereira/yank-buffer-file-name)

  ;; d -> describe ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Mnemonic transgressions:
  ;; - ",dd" for `dired-jump'
  ;; - ",do" for `docker'

  ;; REVIEW: "d" is too good of a key for these bindinds that I never use.
  (:keymaps '(global-map)
   :states '(normal visual)
   :prefix mpereira/leader
   :infix "d"
   "b" #'describe-buffer
   "d" #'dired-jump
   "f" #'find-function-on-key
   "k" #'describe-key
   "m" #'describe-mode
   "o" #'docker)

  ;; e -> evaluate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (:keymaps '(global-map)
   :states '(normal visual)
   :prefix mpereira/leader
   :infix "e"
   ":" #'eval-expression
   "v" #'set-variable
   "w" #'wolfram-alpha)

  ;; f -> find ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (:keymaps '(global-map)
   :states '(normal visual)
   :prefix mpereira/leader
   :infix "f"
   ;; ";" #'counsel-minibuffer-history  ;; WAITING(consult): consult alternative?
   ;; ":" #'counsel-command-history     ;; WAITING(consult): consult alternative?
   "/" #'consult-isearch-history
   "b" #'switch-buffer
   "f" #'find-file
   "g" #'google-this
   ;; "G" #'counsel-web-suggest         ;; WAITING(consult): consult alternative?
   "l" #'find-library
   "m" #'describe-keymap
   "n" #'helpful-function
   "o" #'consult-imenu
   "p" #'package-install
   "v" #'helpful-variable
   "y" #'consult-yank-pop)

  ;; g -> git ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (:keymaps '(global-map)
   :states '(normal)
   :prefix mpereira/leader
   :infix "g"
   "/" #'consult-git-log-grep
   "<" #'smerge-keep-mine
   ">" #'smerge-keep-other
   "[" #'git-timemachine-show-previous-revision
   "]" #'git-timemachine-show-next-revision
   "b" #'magit-blame-addition
   "c" #'magit-commit-popup
   "d" #'(lambda ()
           (interactive)
           (let ((display-buffer-alist (append display-buffer-alist
                                               '(("magit-diff.*"
                                                  (display-buffer-same-window))))))
             (magit-diff-buffer-file)))
   "D" #'magit-diff-unstaged
   "f" #'magit-find-file
   "g" #'magit-dispatch
   "ip" #'gist-region-or-buffer-private
   "ii" #'gist-region-or-buffer
   "il" #'gist-list
   "L" #'magit-log-all
   "l" #'magit-log-buffer-file
   "o" #'mpereira/browse-at-remote
   "r" #'diff-hl-revert-hunk
   "s" #'magit-status
   "S" #'mpereira/magit-status-same-window
   "t" #'git-timemachine-toggle)

  ;; l -> LLM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (:keymaps '(global-map)
   :states '(normal visual)
   :prefix mpereira/leader
   :infix "l"
   "d" 'gptel-quick
   "l" 'chatgpt-shell-quick-insert
   "c" 'chatgpt-shell-prompt-compose)

  ;; p -> project ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (:keymaps '(global-map)
   :states '(normal visual)
   :prefix mpereira/leader
   :infix "p"
   "B" 'project-ibuffer
   "b" 'project-switch-to-buffer
   "d" 'project-dired
   "D" 'project-find-directory
   "f" 'project-find-file
   "G" 'consult-ripgrep
   ;; NOTE: not using `project-find-regexp' because it uses xref which
   ;; doesn't support wgrep mode:
   ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=49208
   "g" 'rg-menu
   "p" 'dired-sidebar-toggle-sidebar
   "r" 'project-query-replace-regexp
   "s" 'project-persp-switch-project
   "t" 'persp-switch-last)

  ;; s -> shell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (:keymaps '(global-map)
   :states '(normal)
   :prefix mpereira/leader
   :infix "s"
   "c" 'project-async-shell-command
   "H" 'project-shell
   "h" 'project-eshell)

  ;; t -> toggle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (:keymaps '(global-map)
   :states '(normal visual)
   :prefix mpereira/leader
   :infix "t"
   "d" #'toggle-debug-on-error
   "e" #'toggle-debug-on-error
   "l" #'toggle-truncate-lines
   "n" #'mpereira/narrow-or-widen-dwim
   "q" #'toggle-debug-on-quit
   "r" #'toggle-read-only
   "t" #'mpereira/narrow-or-widen-dwim
   "w" #'delete-trailing-whitespace))

(defun mpereira/blamer-show-posframe-commit-info-interactive ()
  "Display commit information for the current line in a posframe.
If the 'd' key is pressed, shows the commit in magit.
If the 'y' key is pressed, yanks the commit hash.
If another key is pressed, execute its command."
  (interactive)
  (blamer-show-posframe-commit-info)
  (let* ((this-line-number (line-number-at-pos))
         (file-name (buffer-file-name))
         (command (append blamer--git-blame-cmd
                          (list (format "%s,%s" this-line-number this-line-number))))
         (commit-info (apply #'vc-git--run-command-string file-name command)))
    (unless commit-info
      (error "blamer: Could not retrieve commit information for this line"))
    (when (string-match "^\\([[:xdigit:]]+\\) " commit-info)
      (let ((commit-hash (match-string 1 commit-info))
            (key-sequence (read-key-sequence-vector nil)))
        (posframe-hide blamer--buffer-name)
        (cond
         ;; NOTE: "[100]" is the key sequence for pressing "d".
         ((equal key-sequence [100])
          (if (string= commit-hash "00000000")
              (message "No commit information for this line.")
            (magit-revision-setup-buffer commit-hash nil (list (buffer-file-name)))))
         ;; NOTE: "[121]" is the key sequence for pressing "y".
         ((equal key-sequence [121])
          (kill-new commit-hash))
         (t
          (let ((command (key-binding key-sequence)))
            (when (commandp command)
              (command-execute command)))))))))

(use-package blamer
  :custom
  (blamer-show-avatar-p nil)
  (blamer-max-commit-message-length 80)
  :general
  (:keymaps '(global-map)
   :states '(normal)
   "C-0" 'mpereira/blamer-show-posframe-commit-info-interactive)
  :config
  (custom-set-faces
   '(blamer-pretty-border-face ((t :inherit font-lock-variable-name-face
                                   :height unspecified)))
   '(blamer-pretty-commit-message-face ((t :inherit font-lock-string-face
                                           :height unspecified)))
   '(blamer-pretty-meta-keywords-face ((t :inherit font-lock-function-name-face
                                          :height unspecified)))
   '(blamer-pretty-meta-data-face ((t :inherit font-lock-variable-name-face
                                      :height unspecified)))))

;; Reload directory local variables when saving .dir-locals.el files.
;; Taken from https://emacs.stackexchange.com/a/13096.

(defun mpereira/reload-dir-locals-for-current-buffer ()
  "Reload directory local variables on the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun mpereira/reload-dir-locals-for-all-buffer-in-this-directory ()
  "Reload directory local variables on every buffer with the same
`default-directory' as the current buffer."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (mpereira/reload-dir-locals-for-current-buffer)))))

(defun mpereira/hack-dir-local-variables ()
  "Read per-directory local variables for the current buffer.
Store the directory-local variables in `dir-local-variables-alist'
and `file-local-variables-alist', without applying them."
  (let* ((items (hack-dir-local--get-variables nil))
         (dir-name (car items))
         (variables (cdr items)))
    (when variables
      ;; Clear both alists to prevent duplicates
      (setq dir-local-variables-alist nil
            file-local-variables-alist nil)
      (dolist (elt variables)
        (if (eq (car elt) 'coding)
            (unless hack-dir-local-variables--warned-coding
              (setq hack-dir-local-variables--warned-coding t)
              (display-warning 'files
                               "Coding cannot be specified by dir-locals"))
          (push elt dir-local-variables-alist)))
      (hack-local-variables-filter variables dir-name))))

;; The original `hack-dir-local-variables' function has an issue where
;; it accumulates duplicate entries in `dir-local-variables-alist'
;; when processing `eval' forms from .dir-locals.el. This happens
;; because:
;;
;; 1. The original function only removes existing entries
;; conditionally with: (unless (memq (car elt) '(eval mode)) ...)
;; 2. This means `eval' entries are never removed before new ones are
;; added
;; 3. Each time the function runs, it adds new entries without
;; cleaning up old ones
;;
;; This fixed version:

;; - Clears both `dir-local-variables-alist' and
;; `file-local-variables-alist' before processing new variables
;; - Ensures clean state for each evaluation of directory local
;; variables
;; - Prevents duplicate entries from accumulating
;; - Maintains the original functionality while fixing the duplication
;; issue
;;
;; Without this fix, multiple evaluations of directory local variables
;; (which can happen during normal Emacs operation) would cause
;; duplicate `eval' forms to pile up in the alists, potentially
;; leading to repeated evaluations of the same code.
;;
;; TODO: check if the emacs community if this is an actual bug, and
;; submit a patch if yes.
(advice-add 'hack-dir-local-variables :override #'mpereira/hack-dir-local-variables)

(require 'json)
(require 'url)

(use-package image
  :ensure nil
  :after (evil-collection)
  :general
  (:keymaps '(image-mode-map)
   :states '(normal)
   "," nil ; originally `image-previous-file'.
   ";" nil ; originally `image-next-frame'.
   ))

(defun mpereira/copy-image-to-clipboard ()
  "Copy the image at point to the clipboard."
  (interactive)
  (let* ((image-data (image--get-image))
         (image-file (if (plist-get (cdr image-data) :file)
                         (expand-file-name (plist-get (cdr image-data) :file))
                       (let ((temp-file (make-temp-file "image-copy" nil ".png")))
                         (with-temp-file temp-file
                           (insert (plist-get image-data :data)))
                         temp-file))))
    (cond
     ((eq system-type 'windows-nt)
      (message "Not supported yet."))
     ((eq system-type 'darwin)
      (do-applescript
       (format "set the clipboard to POSIX file \"%s\"" (expand-file-name image-file))))
     ((eq system-type 'gnu/linux)
      (call-process-shell-command
       (format "xclip -selection clipboard -t image/%s -i %s"
               (file-name-extension image-file)
               image-file))))))

(use-package pip-requirements
  :defer t
  :config
  (pip-requirements-fetch-packages))

(defun mpereira/pypi-package-latest-version ()
  "Get the latest version of a PyPI package.
Requires the `pip-requirements' package."
  (interactive)
  (let* ((package-name (completing-read "Package name: " pip-packages))
         (url (format "https://pypi.org/pypi/%s/json" package-name))
         (url-body (with-temp-buffer
                     (url-insert-file-contents url)
                     (let ((json-false :false))
                       (json-read))))
         (version (alist-get 'version (alist-get 'info url-body))))
    (message "%s" version)
    (kill-new version)))
