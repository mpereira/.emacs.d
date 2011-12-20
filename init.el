(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(when (load (expand-file-name "~/.emacs.d/packages/elpa/package.el"))
      (package-initialize))

(add-to-list 'load-path "~/.emacs.d/")
(require 'mpereira-defuns)
(require 'mpereira-evil)
(require 'mpereira-erlang)

;; Paredit.
(add-to-list 'load-path "~/.emacs.d/packages/paredit")
(require 'paredit)
(paredit-mode t)

;; Lose the UI.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Quiel startup.
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

(global-font-lock-mode t)

;; Show line numbers.
(global-linum-mode t)
;; Show column number.
(column-number-mode 1)
;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Give a boost to emacs's poor completion engine.
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
