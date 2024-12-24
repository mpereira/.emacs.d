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

(use-package diminish
  :hook
  (after-init-hook . (lambda () (dolist (mode '(auto-revert-mode
                                                eldoc-mode))
                                  (diminish mode)))))

(use-package which-key
  :diminish
  :custom
  (which-key-idle-delay 0.5)
  :config
  (which-key-mode 1))

(with-eval-after-load 'vertico
  (setq mpereira/posframe-default-internal-border-color
        (face-attribute 'vertico-posframe-border :background nil t))
  (setq mpereira/posframe-default-internal-border-width vertico-posframe-border-width)
  (setq mpereira/default-posframe-override-parameters
        `((left-fringe . 8)
          (right-fringe . 8)
          (background-color . ,(face-attribute 'vertico-posframe :background nil t))
          (foreground-color . ,(face-attribute 'vertico-posframe :foreground nil t)))))

(use-package which-key-posframe
  :vc (:url "https://github.com/emacsorphanage/which-key-posframe"
       :rev :newest)
  :custom
  (which-key-max-display-columns 2)
  (which-key-max-description-length 40)
  (which-key-posframe-border-width mpereira/posframe-default-internal-border-width)
  (which-key-posframe-parameters mpereira/posframe-default-internal-border-color)
  (which-key-posframe-poshandler 'posframe-poshandler-point-bottom-left-corner)
  :config
  (which-key-posframe-mode 1))

(load-library (expand-file-name "secrets.el.gpg" user-emacs-directory))

(use-package emacs
  :custom
  (confirm-kill-emacs 'y-or-n-p)
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
  ;; Prevent tabs from being inserted when formatting buffers.
  ;; https://www.gnu.org/software/emacs/manual/html_node/eintr/Indent-Tabs-Mode.html
  (setq-default indent-tabs-mode nil))

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
  (lsp-headerline-breadcrumb-enable nil)
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (gc-cons-threshold (* 100 1024 1024)) ; 100MB.
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
  :config
  (add-to-list 'lsp-language-id-configuration '(makefile-bsdmake-mode . "make")))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-max-width 100)
  (lsp-ui-doc-max-height 30))

(use-package lsp-pyright
  :custom
  (lsp-pyright-langserver-command "pyright")
  :hook
  (python-mode-hook . (lambda ()
                        (require 'lsp-pyright)
                        (lsp))))

(use-package jupyter
  :custom (jupyter-eval-use-overlays t)
  :hook
  ;; NOTE: https://github.com/emacs-jupyter/jupyter/issues/344
  (jupyter-interaction-mode-hook . (lambda ()
                                     (remove-hook 'completion-at-point-functions
                                                  'jupyter-completion-at-point
                                                  t)))
  :general
  (:keymaps '(python-base-mode-map)
   :states '(normal visual)
   :prefix mpereira/leader
   :infix "e"
   "e" 'jupyter-eval-line-or-region
   "E" 'jupyter-eval-buffer
   "(" 'jupyter-eval-defun))

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
  (setq vertico-multiform-commands
        '((consult-line (:not posframe))
          (consult-ripgrep (:not posframe))
          (mpereira/consult-ripgrep-at-point (:not posframe))
          (t posframe))))

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
   ">" 'chatgpt-shell-prompt-compose-next-interaction
   "<" 'chatgpt-shell-prompt-compose-previous-interaction))

(use-package gptel
  :custom
  (gptel-api-key mpereira/secret-openai-secret-api-key)
  (gptel-model 'gpt-4o))

(use-package gptel-quick
  :vc (:url "https://github.com/karthink/gptel-quick"
       :rev :newest))

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

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "file-backups"))))
(setq tramp-backup-directory-alist `(("." . ,(concat user-emacs-directory "remote-file-backups"))))

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

;; Clojure.
(use-package clojure-ts-mode
  :hook
  (clojure-ts-mode-hook . evil-cleverparens-mode))

;; TypeScript.
(use-package emacs
  :mode
  ("\\.[t|j]sx?$" . typescript-ts-mode))

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

(use-package magit
  :custom
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function 'magit-restore-window-configuration)
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
   "TAB" 'magit-section-cycle ; originally `magit-section-toggle'.
   "Z" #'magit-stash
   "zb" #'evil-scroll-line-to-bottom
   "zt" #'evil-scroll-line-to-top
   "zz" #'evil-scroll-line-to-center)
  :config
  (transient-bind-q-to-quit)

  (defun mpereira/magit-center-buffer-contents ()
    (interactive)
    (setq-local olivetti-body-width 120)
    (olivetti-mode))
  :hook
  (magit-status-mode-hook . mpereira/magit-center-buffer-contents)
  (magit-log-mode-hook . mpereira/magit-center-buffer-contents)
  (git-commit-setup-hook . evil-insert-state))

;; NOTE: a fine-grained personal access token with the following
;; permissions seems to be sufficient:
;;
;; | Contents      | Read-only      |
;; | Issues        | Read and write |
;; | Metadata      | Read-only      |
;; | Pull requests | Read and write |

(use-package forge)

(use-package persp-project
  :vc (:url "https://github.com/PauloPhagula/persp-project"
       :rev :newest))

(use-package dwim-shell-command)

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
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
   "o" 'hydra-dired-quick-sort/body)) ; originally `dired-sort-toggle-or-edit'.

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
  (eshell/alias "e" "find-file $1"))

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
   "j" 'evil-next-visual-line))

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
   "l" 'chatgpt-shell-quick-insert)

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
   "C-0" 'mpereira/blamer-show-posframe-commit-info-interactive))

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

(defun mpereira/enable-autoreload-for-dir-locals ()
  (when (and (buffer-file-name)
             (equal dir-locals-file
                    (file-name-nondirectory (buffer-file-name))))
    (add-hook (make-variable-buffer-local 'after-save-hook)
              'mpereira/reload-dir-locals-for-all-buffer-in-this-directory)))

(add-hook 'emacs-lisp-mode-hook #'mpereira/enable-autoreload-for-dir-locals)

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

(defvar mpereira/html-entity-to-unicode-alist
  '(("Aacute" . "Á") ("aacute" . "á") ("Acirc" . "Â") ("acirc" . "â") ("acute" . "´") ("AElig" . "Æ") ("aelig" . "æ") ("Agrave" . "À") ("agrave" . "à") ("alefsym" . "ℵ") ("Alpha" . "Α") ("alpha" . "α") ("amp" . "&") ("and" . "∧") ("ang" . "∠") ("apos" . "'") ("aring" . "å") ("Aring" . "Å") ("asymp" . "≈") ("atilde" . "ã") ("Atilde" . "Ã") ("auml" . "ä") ("Auml" . "Ä") ("bdquo" . "„") ("Beta" . "Β") ("beta" . "β") ("brvbar" . "¦") ("bull" . "•") ("cap" . "∩") ("ccedil" . "ç") ("Ccedil" . "Ç") ("cedil" . "¸") ("cent" . "¢") ("Chi" . "Χ") ("chi" . "χ") ("circ" . "ˆ") ("clubs" . "♣") ("cong" . "≅") ("copy" . "©") ("crarr" . "↵") ("cup" . "∪") ("curren" . "¤") ("Dagger" . "‡") ("dagger" . "†") ("darr" . "↓") ("dArr" . "⇓") ("deg" . "°") ("Delta" . "Δ") ("delta" . "δ") ("diams" . "♦") ("divide" . "÷") ("eacute" . "é") ("Eacute" . "É") ("ecirc" . "ê") ("Ecirc" . "Ê") ("egrave" . "è") ("Egrave" . "È") ("empty" . "∅") ("emsp" . " ") ("ensp" . " ") ("Epsilon" . "Ε") ("epsilon" . "ε") ("equiv" . "≡") ("Eta" . "Η") ("eta" . "η") ("eth" . "ð") ("ETH" . "Ð") ("euml" . "ë") ("Euml" . "Ë") ("euro" . "€") ("exist" . "∃") ("fnof" . "ƒ") ("forall" . "∀") ("frac12" . "½") ("frac14" . "¼") ("frac34" . "¾") ("frasl" . "⁄") ("Gamma" . "Γ") ("gamma" . "γ") ("ge" . "≥") ("gt" . ">") ("harr" . "↔") ("hArr" . "⇔") ("hearts" . "♥") ("hellip" . "…") ("iacute" . "í") ("Iacute" . "Í") ("icirc" . "î") ("Icirc" . "Î") ("iexcl" . "¡") ("igrave" . "ì") ("Igrave" . "Ì") ("image" . "ℑ") ("infin" . "∞") ("int" . "∫") ("Iota" . "Ι") ("iota" . "ι") ("iquest" . "¿") ("isin" . "∈") ("iuml" . "ï") ("Iuml" . "Ï") ("Kappa" . "Κ") ("kappa" . "κ") ("Lambda" . "Λ") ("lambda" . "λ") ("lang" . "〈") ("laquo" . "«") ("larr" . "←") ("lArr" . "⇐") ("lceil" . "⌈") ("ldquo" . "“") ("le" . "≤") ("lfloor" . "⌊") ("lowast" . "∗") ("loz" . "◊") ("lrm" . "") ("lsaquo" . "‹") ("lsquo" . "‘") ("lt" . "<") ("macr" . "¯") ("mdash" . "—") ("micro" . "µ") ("middot" . "·") ("minus" . "−") ("Mu" . "Μ") ("mu" . "μ") ("nabla" . "∇") ("nbsp" . "") ("ndash" . "–") ("ne" . "≠") ("ni" . "∋") ("not" . "¬") ("notin" . "∉") ("nsub" . "⊄") ("ntilde" . "ñ") ("Ntilde" . "Ñ") ("Nu" . "Ν") ("nu" . "ν") ("oacute" . "ó") ("Oacute" . "Ó") ("ocirc" . "ô") ("Ocirc" . "Ô") ("OElig" . "Œ") ("oelig" . "œ") ("ograve" . "ò") ("Ograve" . "Ò") ("oline" . "‾") ("omega" . "ω") ("Omega" . "Ω") ("Omicron" . "Ο") ("omicron" . "ο") ("oplus" . "⊕") ("or" . "∨") ("ordf" . "ª") ("ordm" . "º") ("oslash" . "ø") ("Oslash" . "Ø") ("otilde" . "õ") ("Otilde" . "Õ") ("otimes" . "⊗") ("ouml" . "ö") ("Ouml" . "Ö") ("para" . "¶") ("part" . "∂") ("permil" . "‰") ("perp" . "⊥") ("Phi" . "Φ") ("phi" . "φ") ("Pi" . "Π") ("pi" . "π") ("piv" . "ϖ") ("plusmn" . "±") ("pound" . "£") ("Prime" . "″") ("prime" . "′") ("prod" . "∏") ("prop" . "∝") ("Psi" . "Ψ") ("psi" . "ψ") ("quot" . "\"") ("radic" . "√") ("rang" . "〉") ("raquo" . "»") ("rarr" . "→") ("rArr" . "⇒") ("rceil" . "⌉") ("rdquo" . "”") ("real" . "ℜ") ("reg" . "®") ("rfloor" . "⌋") ("Rho" . "Ρ") ("rho" . "ρ") ("rlm" . "") ("rsaquo" . "›") ("rsquo" . "’") ("sbquo" . "‚") ("scaron" . "š") ("Scaron" . "Š") ("sdot" . "⋅") ("sect" . "§") ("shy" . "") ("Sigma" . "Σ") ("sigma" . "σ") ("sigmaf" . "ς") ("sim" . "∼") ("spades" . "♠") ("sub" . "⊂") ("sube" . "⊆") ("sum" . "∑") ("sup" . "⊃") ("sup1" . "¹") ("sup2" . "²") ("sup3" . "³") ("supe" . "⊇") ("szlig" . "ß") ("Tau" . "Τ") ("tau" . "τ") ("there4" . "∴") ("Theta" . "Θ") ("theta" . "θ") ("thetasym" . "ϑ") ("thinsp" . " ") ("thorn" . "þ") ("THORN" . "Þ") ("tilde" . "˜") ("times" . "×") ("trade" . "™") ("uacute" . "ú") ("Uacute" . "Ú") ("uarr" . "↑") ("uArr" . "⇑") ("ucirc" . "û") ("Ucirc" . "Û") ("ugrave" . "ù") ("Ugrave" . "Ù") ("uml" . "¨") ("upsih" . "ϒ") ("Upsilon" . "Υ") ("upsilon" . "υ") ("uuml" . "ü") ("Uuml" . "Ü") ("weierp" . "℘") ("Xi" . "Ξ") ("xi" . "ξ") ("yacute" . "ý") ("Yacute" . "Ý") ("yen" . "¥") ("yuml" . "ÿ") ("Yuml" . "Ÿ") ("Zeta" . "Ζ") ("zeta" . "ζ") ("zwj" . "") ("zwnj" . ""))
  "Alist that maps HTML entity names to equivalent unicode characters")

(defvar mpereira/html-entity-to-unicode-hash nil
  "Hash table mapping HTML entities to unicode characters")

(defvar mpereira/html-unicode-to-entity-hash nil
  "Hash table mapping unicode characters to HTML entities")

(let ((entity-list-length (length mpereira/html-entity-to-unicode-alist)))
  (setq mpereira/html-entity-to-unicode-hash
        (make-hash-table :test 'equal :size entity-list-length))
  (loop for (key . val) in mpereira/html-entity-to-unicode-alist
        do (puthash key val mpereira/html-entity-to-unicode-hash))

  (setq mpereira/html-unicode-to-entity-hash
        (make-hash-table :test 'equal :size entity-list-length))
  (loop for (val . key) in mpereira/html-entity-to-unicode-alist
        do (puthash key val mpereira/html-unicode-to-entity-hash)))

(defun mpereira/html-entity-to-unicode (entity)
  (gethash entity mpereira/html-entity-to-unicode-hash))

(defun mpereira/html-unicode-to-entity (unicode)
  (gethash unicode mpereira/html-unicode-to-entity-hash))

(defun mpereira/html-string-entities-to-unicode (string)
  (replace-regexp-in-string "&[[:word:]]+;"
                            (lambda (s)
                              (or (mpereira/html-entity-to-unicode (substring s 1 -1)) s))
                            string))

(defun mpereira/html-string-unicode-to-entities (string)
  (replace-regexp-in-string "[^[:word:]]"
                            (lambda (s)
                              (or (mpereira/html-unicode-to-entity s) s))
                            string))

(defun mpereira/html-escape (&optional start end)
  "Escape characters to HTML entities in the current buffer between
start and end, which default to the whole buffer"
  (interactive "r")
  (unless start (setq start (point-min)))
  (unless end (setq end (point-max)))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "\\([^[:word:]]\\)" nil t)
        (let* ((match (match-string 1))
               (entity (mpereira/html-unicode-to-entity (match-string 1))))
          (when entity
            (setq entity (concat "&" entity ";")))
          (replace-match (or entity match) nil nil))))))

(defun mpereira/html-unescape (&optional start end)
  "Unescape HTML entities to characters in the current buffer
between start and end, which default to the whole buffer"
  (interactive "r")
  (unless start (setq start (point-min)))
  (unless end (setq end (point-max)))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "\\(&\\([[:word:]]+\\);\\)" nil t)
        (replace-match (or (mpereira/html-entity-to-unicode (match-string 2))
                           (match-string 1))
                       nil nil)))))

;; NOTE: got from Fuco1's config on 2024-12-13.
;; https://github.com/Fuco1/.emacs.d/blob/76e80dd07320b079fa26db3af6096d8d8a4f3bb9/site-lisp/my-redef.el
(eval-after-load "lisp-mode"
  '(defun lisp-indent-function (indent-point state)
     "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
     (let ((normal-indent (current-column))
           (orig-point (point)))
       (goto-char (1+ (elt state 1)))
       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
       (cond
        ;; car of form doesn't seem to be a symbol, or is a keyword
        ((and (elt state 2)
              (or (not (looking-at "\\sw\\|\\s_"))
                  (looking-at ":")))
         (if (not (> (save-excursion (forward-line 1) (point))
                     calculate-lisp-indent-last-sexp))
             (progn (goto-char calculate-lisp-indent-last-sexp)
                    (beginning-of-line)
                    (parse-partial-sexp (point)
                                        calculate-lisp-indent-last-sexp 0 t)))
         ;; Indent under the list or under the first sexp on the same
         ;; line as calculate-lisp-indent-last-sexp.  Note that first
         ;; thing on that line has to be complete sexp since we are
         ;; inside the innermost containing sexp.
         (backward-prefix-chars)
         (current-column))
        ((and (save-excursion
                (goto-char indent-point)
                (skip-syntax-forward " ")
                (not (looking-at ":")))
              (save-excursion
                (goto-char orig-point)
                (looking-at ":")))
         (save-excursion
           (goto-char (+ 2 (elt state 1)))
           (current-column)))
        (t
         (let ((function (buffer-substring (point)
                                           (progn (forward-sexp 1) (point))))
               method)
           (setq method (or (function-get (intern-soft function)
                                          'lisp-indent-function)
                            (get (intern-soft function) 'lisp-indent-hook)))
           (cond ((or (eq method 'defun)
                      (and (null method)
                           (> (length function) 3)
                           (string-match "\\`def" function)))
                  (lisp-indent-defform state indent-point))
                 ((integerp method)
                  (lisp-indent-specform method state
                                        indent-point normal-indent))
                 (method
                  (funcall method indent-point state)))))))))
