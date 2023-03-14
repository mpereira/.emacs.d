;;; init.el --- -*- lexical-binding: t -*-

(setq debug-on-error t)

(let* ((gc-cons-percentage-original gc-cons-percentage)
       (gc-cons-threshold-original gc-cons-threshold)
       (gc-cons-percentage-initial 0.6)
       (gc-cons-threshold-initial most-positive-fixnum))
  (setq gc-cons-threshold gc-cons-threshold-initial)
  (setq gc-cons-percentage gc-cons-percentage-initial)
  (add-hook 'after-init-hook
            (lambda ()
              (garbage-collect)
              (setq gc-cons-threshold gc-cons-threshold-original)
              (setq gc-cons-percentage gc-cons-percentage-original)
              (setq debug-on-error nil))))

(require 'package)

(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

(require 'vc-use-package)

(setq use-package-always-ensure t)
(setq use-package-always-demand t)

(use-package org
  :pin nongnu
  :mode (("\\.\\(org\\|org_archive\\)$" . org-mode)))

(use-package org-contrib)

;; Prevent annoying local variable prompts.
(let ((enable-local-variables :all))
  (org-babel-load-file (expand-file-name "configuration.org" user-emacs-directory)))

(put 'list-threads 'disabled nil)
