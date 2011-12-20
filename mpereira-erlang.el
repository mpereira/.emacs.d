(add-to-list 'load-path
             (car (file-expand-wildcards "/usr/lib/erlang/lib/tools-*/emacs")))
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))

(require 'erlang-start)
(require 'erlang-flymake)

(add-to-list 'load-path "~/.emacs.d/packages/distel/elisp")
(require 'distel)
(distel-setup)

(provide 'mpereira-erlang)
