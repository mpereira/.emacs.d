(setq debug-on-error t)

(setq mpereira/gc-cons-percentage-original gc-cons-percentage)
(setq mpereira/gc-cons-threshold-original gc-cons-threshold)

(eval-when-compile
  (require 'cl))

(lexical-let ((gc-cons-percentage-initial 0.2)
              (gc-cons-percentage-normal 0.1)
              (gc-cons-threshold-initial most-positive-fixnum)
              (gc-cons-threshold-normal (* 512 1024 1024)))
  (setq gc-cons-threshold gc-cons-threshold-initial)
  (setq gc-cons-percentage gc-cons-percentage-initial)
  (add-hook 'after-init-hook
            (lambda ()
              (garbage-collect)
              (setq mpereira/gc-cons-percentage-maximum gc-cons-percentage-normal)
              (setq mpereira/gc-cons-percentage-normal gc-cons-percentage-normal)
              (setq mpereira/gc-cons-threshold-maximum #x40000000)
              (setq mpereira/gc-cons-threshold-normal gc-cons-threshold-normal)
              (setq gc-cons-threshold gc-cons-threshold-normal)
              (setq gc-cons-percentage gc-cons-percentage-normal)
              (setq debug-on-error nil))))

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
