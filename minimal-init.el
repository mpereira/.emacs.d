(setq debug-on-error t)

(add-hook 'after-init-hook
          (lambda ()
            (setq debug-on-error nil)))

(require 'package)

(setq package-enable-at-startup nil)

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

(use-package evil
  :config
  (evil-mode 1))

(use-package evil-collection
  :config
  (evil-collection-init))

(use-package ivy
  :config
  (ivy-mode 1))

;; macOS modifiers.
(setq mac-command-modifier 'meta)
;; Setting "Option" to nil allows me to type umlauts with "Option+u".
(setq mac-option-modifier nil)
(setq mac-control-modifier 'control)
(setq ns-function-modifier 'hyper)
