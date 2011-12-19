(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(when (load (expand-file-name "~/.emacs.d/packages/elpa/package.el"))
      (package-initialize))

; Evil.
(add-to-list 'load-path "~/.emacs.d/packages/undo-tree")
(add-to-list 'load-path "~/.emacs.d/packages/evil")
(require 'evil)
(evil-mode 1)

; Evil surround.
(add-to-list 'load-path "~/.emacs.d/packages/evil-surround")
(require 'surround)
(global-surround-mode 1)

; Erlang.
(add-to-list
  'load-path
    (car (file-expand-wildcards "/usr/lib/erlang/lib/tools-*/emacs")))
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))

(require 'erlang-start)
(require 'erlang-flymake)

(add-to-list 'load-path "~/.emacs.d/packages/distel/elisp")
(require 'distel)
(distel-setup)

; Quiet startup.
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

; Show line numbers.
(global-linum-mode t)
; Show column number.
(column-number-mode 1)
; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

; Give a boost to emacs's poor completion engine.
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
