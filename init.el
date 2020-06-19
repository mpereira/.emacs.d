(setq debug-on-error t)

(setq mpereira/gc-cons-threshold-original gc-cons-threshold)
(setq mpereira/gc-cons-percentage-original gc-cons-percentage)

(lexical-let ((original-gc-cons-threshold gc-cons-threshold)
              (original-gc-cons-percentage gc-cons-percentage)
              (normal-gc-cons-threshold (* 512 1024 1024))
              (normal-gc-cons-percentage 0.1)
              (initial-gc-cons-threshold most-positive-fixnum)
              (initial-gc-cons-percentage 0.2))
  (setq gc-cons-threshold initial-gc-cons-threshold)
  (setq gc-cons-percentage initial-gc-cons-percentage)
  (add-hook 'after-init-hook
            (lambda ()
              (garbage-collect)

              (setq debug-on-error nil)

              (setq mpereira/gc-cons-threshold-normal normal-gc-cons-threshold)
              (setq mpereira/gc-cons-threshold-maximum #x40000000)
              (setq mpereira/gc-cons-percentage-normal normal-gc-cons-percentage)
              (setq mpereira/gc-cons-percentage-maximum normal-gc-cons-percentage)

              (setq gc-cons-threshold normal-gc-cons-threshold))))
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
