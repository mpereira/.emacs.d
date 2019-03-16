(setq debug-on-error t)
(setq gc-cons-threshold (* 128 1024 1024))

(require 'package)

(package-initialize)

(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;; (setq package-archives
;;       '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
;;         ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
;;         ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package org
  :ensure org-plus-contrib
  :mode (("\\.\\(org\\|org_archive\\)$" . org-mode)))

;; Prevent annoying local variable prompts.
(setq enable-local-variables :all)
(org-babel-load-file (expand-file-name "configuration.org" user-emacs-directory))
(setq enable-local-variables t)

(setq gc-cons-threshold (* 20 1024 1024))
(setq debug-on-error nil)
(put 'erase-buffer 'disabled nil)
