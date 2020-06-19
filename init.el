(setq debug-on-error t)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
          (lambda ()
            (garbage-collect)
            (setq debug-on-error nil)
            (setq gc-cons-threshold 100000000))) ;; 100MB

(require 'package)

(package-initialize)

(setq package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)
(setq use-package-always-demand t)

(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :mode (("\\.\\(org\\|org_archive\\)$" . org-mode)))

;; Prevent annoying local variable prompts.
(let ((enable-local-variables :all))
  (org-babel-load-file (expand-file-name "configuration.org" user-emacs-directory)))
