(setq debug-on-error t)
(setq gc-cons-threshold (* 128 1024 1024))

(require 'package)

(package-initialize)

(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package org
  :ensure org-plus-contrib)

(org-babel-load-file (expand-file-name "configuration.org" user-emacs-directory))

(setq gc-cons-threshold (* 20 1024 1024))
(setq debug-on-error nil)
