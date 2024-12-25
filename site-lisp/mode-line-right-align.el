;;; mode-line-right-align.el --- Right-align mode-line constructs  -*- lexical-binding: t; -*-

;; Author: Unknown
;; URL: https://lists.gnu.org/archive/html/bug-gnu-emacs/2023-06
;; Version: 0.1
;; Keywords: mode-line
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:

;; This package provides functionality to right-align constructs in
;; the mode-line using `mode-line-format-right-align', with
;; configurable alignment edge.

;;; Code:

(defgroup mode-line-right-align nil
  "Right-alignment for mode-line constructs."
  :group 'mode-line)

(defcustom mode-line-right-align-edge 'window
  "Where function `mode--line-format-right-align' should align to.
Internally, that function uses `:align-to' in display property,
so aligns to the left edge of the given area.  See info node
`(elisp)Pixel Specification'.

Must be set to a symbol.  Acceptable values are:
- `window': align to extreme right of window, regardless of margins
  or fringes
- `right-fringe': align to right-fringe
- `right-margin': align to right-margin"
  :type '(choice (const right-margin)
                 (const right-fringe)
                 (const window))
  :group 'mode-line-right-align)

(defun mode--line-format-right-align ()
  "Right-align all following mode-line constructs.

When the symbol `mode-line-format-right-align' appears in
`mode-line-format', return a string of one space, with a display
property to make it appear long enough to align anything after
that symbol to the right of the rendered mode line.  Exactly how
far to the right is controlled by `mode-line-right-align-edge'.

It is important that the symbol `mode-line-format-right-align' be
included in `mode-line-format' (and not another similar construct
such as `(:eval (mode-line-format-right-align)').  This is because
the symbol `mode-line-format-right-align' is processed by
`format-mode-line' as a variable."
  (let* ((rest (cdr (memq 'mode-line-format-right-align
			  mode-line-format)))
	 (rest-str (format-mode-line `("" ,@rest)))
	 (rest-width (string-pixel-width rest-str)))
    (propertize " " 'display
		;; The `right' spec doesn't work on TTY frames
		;; when windows are split horizontally (bug#59620)
		(if (and (display-graphic-p)
  (not (eq mode-line-right-align-edge 'window)))
		    `(space :align-to (- ,mode-line-right-align-edge
                                         (,rest-width)))
		  `(space :align-to (,(- (window-pixel-width)
                                         (window-scroll-bar-width)
                                         (window-right-divider-width)
                                         (* (or (cdr (window-margins)) 1)
                                            (frame-char-width))
                                         ;; Manually account for value of
                                         ;; `mode-line-right-align-edge' even
                                         ;; when display is non-graphical
                                         (pcase mode-line-right-align-edge
                                           ('right-margin
                                            (or (cdr (window-margins)) 0))
                                           ('right-fringe
                                            ;; what here?
                                            (or (cadr (window-fringes)) 0))
                                           (_ 0))
                                         rest-width)))))))

;;;###autoload
(defvar mode-line-format-right-align '(:eval (mode--line-format-right-align))
  "Mode line construct to right align all following constructs.")
;;;###autoload
(put 'mode-line-format-right-align 'risky-local-variable t)

(provide 'mode-line-right-align)

;;; mode-line-right-align.el ends here
