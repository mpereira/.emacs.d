(add-to-list 'load-path "~/.emacs.d/packages/evil")
(add-to-list 'load-path "~/.emacs.d/packages/evil-numbers")
(add-to-list 'load-path "~/.emacs.d/packages/undo-tree")

(setq evil-shift-width 2)
(setq evil-auto-indent t)

(require 'evil)
(evil-mode t)

(require 'evil-numbers)

(define-key evil-visual-state-map ",c" 'comment-or-uncomment-region)
(define-key evil-normal-state-map ",c" 'comment-line)
(define-key evil-normal-state-map (kbd "Y")
                                  '(lambda()
                                     (interactive)
                                     (evil-yank-characters (point) (point-at-eol))))
(define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt)

(add-to-list 'load-path "~/.emacs.d/packages/evil-surround")
(require 'surround)
(global-surround-mode 1)

(provide 'mpereira-evil)
