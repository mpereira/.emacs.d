;;; mpereira-mode-line.el --- Custom mode line implementation -*- lexical-binding: t -*-

;; Author: Your Name
;; Keywords: mode-line
;; Package-Requires: ((emacs "27.1") (s "1.12.0"))
;; Version: 1.0

;;; Commentary:
;; A customizable mode line implementation with project and buffer information.

;;; Code:

(require 'project)
(require 's)
(require 'tramp)

;;;###autoload
(define-minor-mode mpereira-mode-line-mode
  "Toggle custom mode line appearance."
  :global t
  :group 'mode-line
  (if mpereira-mode-line-mode
      (mpereira-mode-line-enable)
    (mpereira-mode-line-disable)))

(defgroup mpereira-mode-line nil
  "Customization group for mpereira-mode-line."
  :group 'mode-line)

(defcustom mpereira-mode-line-max-directory-length 30
  "Maximum length for directory display in mode line."
  :type 'integer
  :group 'mpereira-mode-line)

(defvar mpereira-mode-line--original-format mode-line-format
  "Store the original mode-line-format.")

;; Helper functions
(defun mpereira-mode-line--remote-p ()
  "Check if current buffer is remote."
  (tramp-tramp-file-p default-directory))

(defun mpereira-mode-line--buffer-project-directory (project-root-directory
                                                     buffer-directory
                                                     &optional max-length)
  "Returns a possibly left-truncated relative directory for a project buffer.
PROJECT-ROOT-DIRECTORY is the root of the project.
BUFFER-DIRECTORY is the directory to process.
Optional MAX-LENGTH is the maximum length of the output."
  (let* ((truncation-string (if (char-displayable-p ?…) "…/" ".../"))
         (relative-directory (s-chop-prefix project-root-directory buffer-directory))
         (abbreviated-directory (abbreviate-file-name relative-directory))
         (max-length (or max-length 1.0e+INF)))
    (if (and max-length
             (<= (string-width abbreviated-directory) max-length))
        abbreviated-directory
      (let ((path (reverse (split-string abbreviated-directory "/")))
            (output ""))
        (when (and path (equal "" (car path)))
          (setq path (cdr path)))
        (let ((max (- max-length (string-width truncation-string))))
          (while (and path (<= (string-width (concat (car path) "/" output))
                               max))
            (setq output (concat (car path) "/" output))
            (setq path (cdr path))))
        (when path
          (setq output (concat truncation-string output)))
        output))))

(defun mpereira-mode-line--short-directory-path (directory &optional max-length)
  "Returns a potentially trimmed-down version of the directory DIRECTORY.
Optional MAX-LENGTH specifies maximum length of the output."
  (let* ((components (split-string (abbreviate-file-name directory) "/"))
         (max-length (or max-length 1.0e+INF))
         (len (+ (1- (length components))
                 (cl-reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-length)
                (cdr components))
      (setq str (concat str
                        (cond ((= 0 (length (car components))) "/")
                              ((= 1 (length (car components)))
                               (concat (car components) "/"))
                              (t
                               (if (string= "."
                                            (string (elt (car components) 0)))
                                   (concat (substring (car components) 0 2)
                                           "/")
                                 (string (elt (car components) 0) ?/)))))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

(defface mpereira-mode-line-active-buffer-face
  '((default :inherit (bold prot-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#BFD9EA" :foreground "black")
    (((class color) (min-colors 88) (background dark))
     :background "#77d0ff" :foreground "black")
    (t :background "light blue" :foreground "black"))
  "Face for modeline indicators with a light blue background."
  :group 'prot-modeline-faces)

(defvar mpereira-mode-line--selected-window nil)

(defun mpereira-mode-line--update-selected-window ()
  "Update selected window (before mode-line is active)"
  (setq mpereira-mode-line--selected-window (selected-window)))

(add-hook 'post-command-hook #'mpereira-mode-line--update-selected-window)

(defun mpereira-mode-line--strip-uniquify-suffix (buf-name)
  "Remove uniquify suffixes from BUF-NAME."
  (replace-regexp-in-string "<[0-9]+>$" "" buf-name))

(defvar-local mpereira-mode-line--buffer-segment
    '(:eval
      (let* ((modified-or-ro-symbol (cond
                                     ((and buffer-file-name
                                           (buffer-modified-p))
                                      "~")
                                     (buffer-read-only ":RO")
                                     (t "")))
             (active (eq mpereira-mode-line--selected-window
                         (get-buffer-window (current-buffer))))
             (buffer-name* (mpereira-mode-line--strip-uniquify-suffix (buffer-name)))
             (buffer-name** (if (buffer-file-name)
                               (file-name-nondirectory (buffer-file-name))
                             buffer-name*))
             (directory-face 'font-lock-comment-face)
             (buffer-name-face (if active
                                   'mpereira-mode-line-active-buffer-face
                                 'bold))
             (directory (if (mpereira-mode-line--remote-p)
                            ""
                          (if-let* ((project (project-current))
                                      (project-root* (expand-file-name (project-root project))))
                            (if buffer-file-name
                                (mpereira-mode-line--short-directory-path
                                 (mpereira-mode-line--buffer-project-directory
                                  project-root*
                                  default-directory)
                                 mpereira-mode-line-max-directory-length)
                              "")
                            ""))))
        (concat
         (when (not (string-blank-p directory))
           (propertize " " 'face directory-face))
         (propertize (format "%s" directory) 'face directory-face)
         (propertize " " 'face buffer-name-face)
         (propertize (format "%s" buffer-name**) 'face buffer-name-face)
         (propertize modified-or-ro-symbol 'face buffer-name-face)
         (propertize " " 'face buffer-name-face))))
  "Return the buffer segment for the mode line.")

(defvar-local mpereira-mode-line--buffer-position-segment
  '(:eval
    (unless eshell-mode
      (propertize " %p %l,%c " 'face 'font-lock-comment-face))))

(defvar-local mpereira-mode-line--flycheck-mode-line
  '(:eval
    (when flycheck-mode
      (let ((flycheck-status (flycheck-mode-line-status-text)))
        (propertize (format " %s " flycheck-status)
                    'face 'mode-line-buffer-id
                    'mouse-face 'mode-line-highlight)))))

(dolist (segment '(mpereira-mode-line--tramp-segment
                   mpereira-mode-line--buffer-segment
                   mpereira-mode-line--buffer-position-segment
                   mpereira-mode-line--flycheck-mode-line))
  (put segment 'risky-local-variable t))

(defun mpereira-mode-line-enable ()
  "Enable custom mode line."
  (unless mpereira-mode-line--original-format
    (setq mpereira-mode-line--original-format mode-line-format))

  (setq-default mode-line-format
                '("%e"
                  prot-modeline-kbd-macro
                  prot-modeline-narrow
                  prot-modeline-input-method
                  mpereira-mode-line--tramp-segment
                  mpereira-mode-line--buffer-segment
                  mode-line-format-right-align
                  mpereira-mode-line--buffer-position-segment
                  prot-modeline-process
                  prot-modeline-align-right
                  mpereira-mode-line--flycheck-mode-line
                  " "
                  prot-modeline-misc-info
                  " "
                  prot-modeline-major-mode
                  " ")))

(defun mpereira-mode-line-disable ()
  "Disable custom mode line and restore original."
  (when mpereira-mode-line--original-format
    (setq-default mode-line-format mpereira-mode-line--original-format)))

;;;###autoload
(define-minor-mode mpereira-mode-line-mode
  "Minor mode to get a custom mode line.

When called interactively, toggle
`mpereira-mode-line-mode'.  With prefix ARG, enable
`mpereira-mode-line-mode' if ARG is positive, otherwise
disable it.

When called from Lisp, enable `mpereira-mode-line-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `mpereira-mode-line-mode'.
Otherwise behave as if called interactively."
  :init-value nil
  :keymap nil
  :lighter ""
  :group 'mpereira-mode-line
  :global t
  (if mpereira-mode-line-mode
      (progn
        (mpereira-mode-line-enable))
    (progn
      (mpereira-mode-line-disable))))

(provide 'mpereira-mode-line)

;;; mpereira-mode-line.el ends here
