(defun comment-line (&optional arg)
  "If no region is selected and current line is not blank and we are not at the
  end of the line, comment current line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(provide 'mpereira-defuns)