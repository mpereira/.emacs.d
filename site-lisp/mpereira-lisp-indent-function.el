;;; mpereira-lisp-indent-function.el --- Custom implementation of lisp-indent-function  -*- lexical-binding: t; -*-

;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))
;; Author: mpereira
;; URL: https://github.com/mpereira/mpereira-lisp-indent-function
;; Keywords: lisp, indentation

;;; Commentary:

;; Provides a custom implementation of the `lisp-indent-function`, based on
;; the configuration from Fuco1's Emacs setup.

;;; Code:

(defun mpereira-lisp-indent-function (indent-point state)
  "Custom implementation of the Lisp indentation function.
This function replaces the default `lisp-indent-function' behavior.

INDENT-POINT is the position at which the line being indented begins.
STATE is the `parse-partial-sexp' state for the location."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond
         ((or (eq method 'defun)
              (and (null method)
                   (> (length function) 3)
                   (string-match "\\`def" function)))
          (lisp-indent-defform state indent-point))
         ((integerp method)
          (lisp-indent-specform method state indent-point normal-indent))
         (method
          (funcall method indent-point state))))))))

;;;###autoload
(with-eval-after-load 'lisp-mode
  (setq lisp-indent-function #'mpereira-lisp-indent-function))

(provide 'mpereira-lisp-indent-function)

;;; mpereira-lisp-indent-function.el ends here
