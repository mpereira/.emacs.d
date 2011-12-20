(add-to-list 'load-path
             (car (file-expand-wildcards "/usr/lib/erlang/lib/tools-*/emacs")))
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))

(require 'erlang-start)
(require 'erlang-flymake)

(add-hook
 'erlang-extended-mode-hook
 (lambda ()
    (define-key erlang-extended-mode-map [?\(] 'paredit-open-parenthesis)
    (define-key erlang-extended-mode-map [?\[] 'paredit-open-square)
    (define-key erlang-extended-mode-map [?\{] 'paredit-open-curly)
    (define-key erlang-extended-mode-map [?\)] 'paredit-close-parenthesis)
    (define-key erlang-extended-mode-map [?\}] 'paredit-close-curly)
    (define-key erlang-extended-mode-map [?\]] 'paredit-close-square)))

(add-to-list 'load-path "~/.emacs.d/packages/distel/elisp")
(require 'distel)
(distel-setup)

(provide 'mpereira-erlang)
