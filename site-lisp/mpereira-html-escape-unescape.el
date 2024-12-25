;;; mpereira-html-escape-unescape.el --- HTML entity escape/unescape utilities -*- lexical-binding: t -*-

;; Copyright (C) 2024 Your Name

;; Author: Your Name <your.email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: html, entities, unicode
;; URL: https://github.com/yourusername/mpereira-html-escape-unescape

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functions to escape and unescape HTML entities.

;;; Code:

(defvar mpereira-html-escape-unescape--entity-to-unicode-alist
  '(("Aacute" . "Á") ("aacute" . "á") ("Acirc" . "Â") ("acirc" . "â") ("acute" . "´") ("AElig" . "Æ") ("aelig" . "æ") ("Agrave" . "À") ("agrave" . "à") ("alefsym" . "ℵ") ("Alpha" . "Α") ("alpha" . "α") ("amp" . "&") ("and" . "∧") ("ang" . "∠") ("apos" . "'") ("aring" . "å") ("Aring" . "Å") ("asymp" . "≈") ("atilde" . "ã") ("Atilde" . "Ã") ("auml" . "ä") ("Auml" . "Ä") ("bdquo" . "„") ("Beta" . "Β") ("beta" . "β") ("brvbar" . "¦") ("bull" . "•") ("cap" . "∩") ("ccedil" . "ç") ("Ccedil" . "Ç") ("cedil" . "¸") ("cent" . "¢") ("Chi" . "Χ") ("chi" . "χ") ("circ" . "ˆ") ("clubs" . "♣") ("cong" . "≅") ("copy" . "©") ("crarr" . "↵") ("cup" . "∪") ("curren" . "¤") ("Dagger" . "‡") ("dagger" . "†") ("darr" . "↓") ("dArr" . "⇓") ("deg" . "°") ("Delta" . "Δ") ("delta" . "δ") ("diams" . "♦") ("divide" . "÷") ("eacute" . "é") ("Eacute" . "É") ("ecirc" . "ê") ("Ecirc" . "Ê") ("egrave" . "è") ("Egrave" . "È") ("empty" . "∅") ("emsp" . " ") ("ensp" . " ") ("Epsilon" . "Ε") ("epsilon" . "ε") ("equiv" . "≡") ("Eta" . "Η") ("eta" . "η") ("eth" . "ð") ("ETH" . "Ð") ("euml" . "ë") ("Euml" . "Ë") ("euro" . "€") ("exist" . "∃") ("fnof" . "ƒ") ("forall" . "∀") ("frac12" . "½") ("frac14" . "¼") ("frac34" . "¾") ("frasl" . "⁄") ("Gamma" . "Γ") ("gamma" . "γ") ("ge" . "≥") ("gt" . ">") ("harr" . "↔") ("hArr" . "⇔") ("hearts" . "♥") ("hellip" . "…") ("iacute" . "í") ("Iacute" . "Í") ("icirc" . "î") ("Icirc" . "Î") ("iexcl" . "¡") ("igrave" . "ì") ("Igrave" . "Ì") ("image" . "ℑ") ("infin" . "∞") ("int" . "∫") ("Iota" . "Ι") ("iota" . "ι") ("iquest" . "¿") ("isin" . "∈") ("iuml" . "ï") ("Iuml" . "Ï") ("Kappa" . "Κ") ("kappa" . "κ") ("Lambda" . "Λ") ("lambda" . "λ") ("lang" . "〈") ("laquo" . "«") ("larr" . "←") ("lArr" . "⇐") ("lceil" . "⌈") ("ldquo" . "\"") ("le" . "≤") ("lfloor" . "⌊") ("lowast" . "∗") ("loz" . "◊") ("lrm" . "") ("lsaquo" . "‹") ("lsquo" . "'") ("lt" . "<") ("macr" . "¯") ("mdash" . "—") ("micro" . "µ") ("middot" . "·") ("minus" . "−") ("Mu" . "Μ") ("mu" . "μ") ("nabla" . "∇") ("nbsp" . "") ("ndash" . "–") ("ne" . "≠") ("ni" . "∋") ("not" . "¬") ("notin" . "∉") ("nsub" . "⊄") ("ntilde" . "ñ") ("Ntilde" . "Ñ") ("Nu" . "Ν") ("nu" . "ν") ("oacute" . "ó") ("Oacute" . "Ó") ("ocirc" . "ô") ("Ocirc" . "Ô") ("OElig" . "Œ") ("oelig" . "œ") ("ograve" . "ò") ("Ograve" . "Ò") ("oline" . "‾") ("omega" . "ω") ("Omega" . "Ω") ("Omicron" . "Ο") ("omicron" . "ο") ("oplus" . "⊕") ("or" . "∨") ("ordf" . "ª") ("ordm" . "º") ("oslash" . "ø") ("Oslash" . "Ø") ("otilde" . "õ") ("Otilde" . "Õ") ("otimes" . "⊗") ("ouml" . "ö") ("Ouml" . "Ö") ("para" . "¶") ("part" . "∂") ("permil" . "‰") ("perp" . "⊥") ("Phi" . "Φ") ("phi" . "φ") ("Pi" . "Π") ("pi" . "π") ("piv" . "ϖ") ("plusmn" . "±") ("pound" . "£") ("Prime" . "″") ("prime" . "′") ("prod" . "∏") ("prop" . "∝") ("Psi" . "Ψ") ("psi" . "ψ") ("quot" . "\"") ("radic" . "√") ("rang" . "〉") ("raquo" . "»") ("rarr" . "→") ("rArr" . "⇒") ("rceil" . "⌉") ("rdquo" . "\"") ("real" . "ℜ") ("reg" . "®") ("rfloor" . "⌋") ("Rho" . "Ρ") ("rho" . "ρ") ("rlm" . "") ("rsaquo" . "›") ("rsquo" . "'") ("sbquo" . "‚") ("scaron" . "š") ("Scaron" . "Š") ("sdot" . "⋅") ("sect" . "§") ("shy" . "") ("Sigma" . "Σ") ("sigma" . "σ") ("sigmaf" . "ς") ("sim" . "∼") ("spades" . "♠") ("sub" . "⊂") ("sube" . "⊆") ("sum" . "∑") ("sup" . "⊃") ("sup1" . "¹") ("sup2" . "²") ("sup3" . "³") ("supe" . "⊇") ("szlig" . "ß") ("Tau" . "Τ") ("tau" . "τ") ("there4" . "∴") ("Theta" . "Θ") ("theta" . "θ") ("thetasym" . "ϑ") ("thinsp" . " ") ("thorn" . "þ") ("THORN" . "Þ") ("tilde" . "˜") ("times" . "×") ("trade" . "™") ("uacute" . "ú") ("Uacute" . "Ú") ("uarr" . "↑") ("uArr" . "⇑") ("ucirc" . "û") ("Ucirc" . "Û") ("ugrave" . "ù") ("Ugrave" . "Ù") ("uml" . "¨") ("upsih" . "ϒ") ("Upsilon" . "Υ") ("upsilon" . "υ") ("uuml" . "ü") ("Uuml" . "Ü") ("weierp" . "℘") ("Xi" . "Ξ") ("xi" . "ξ") ("yacute" . "ý") ("Yacute" . "Ý") ("yen" . "¥") ("yuml" . "ÿ") ("Yuml" . "Ÿ") ("Zeta" . "Ζ") ("zeta" . "ζ") ("zwj" . "") ("zwnj" . ""))
  "Alist that maps HTML entity names to equivalent unicode characters.")

(defvar mpereira-html-escape-unescape--entity-to-unicode-hash nil
  "Hash table mapping HTML entities to unicode characters.")

(defvar mpereira-html-escape-unescape--unicode-to-entity-hash nil
  "Hash table mapping unicode characters to HTML entities.")

(let ((entity-list-length (length mpereira-html-escape-unescape--entity-to-unicode-alist)))
  (setq mpereira-html-escape-unescape--entity-to-unicode-hash
        (make-hash-table :test 'equal :size entity-list-length))
  (dolist (pair mpereira-html-escape-unescape--entity-to-unicode-alist)
    (puthash (car pair) (cdr pair) mpereira-html-escape-unescape--entity-to-unicode-hash))

  (setq mpereira-html-escape-unescape--unicode-to-entity-hash
        (make-hash-table :test 'equal :size entity-list-length))
  (dolist (pair mpereira-html-escape-unescape--entity-to-unicode-alist)
    (puthash (cdr pair) (car pair) mpereira-html-escape-unescape--unicode-to-entity-hash)))

;;;###autoload
(defun mpereira-html-escape-unescape--entity-to-unicode (entity)
  "Convert HTML entity to unicode character."
  (gethash entity mpereira-html-escape-unescape--entity-to-unicode-hash))

;;;###autoload
(defun mpereira-html-escape-unescape--unicode-to-entity (unicode)
  "Convert unicode character to HTML entity."
  (gethash unicode mpereira-html-escape-unescape--unicode-to-entity-hash))

;;;###autoload
(defun mpereira-html-escape-unescape--string-entities-to-unicode (string)
  "Convert HTML entities to unicode characters in STRING."
  (replace-regexp-in-string "&[[:word:]]+;"
                            (lambda (s)
                              (or (mpereira-html-escape-unescape--entity-to-unicode (substring s 1 -1)) s))
                            string))

;;;###autoload
(defun mpereira-html-escape-unescape--string-unicode-to-entities (string)
  "Convert unicode characters to HTML entities in STRING."
  (replace-regexp-in-string "[^[:word:]]"
                            (lambda (s)
                              (or (mpereira-html-escape-unescape--unicode-to-entity s) s))
                            string))

;;;###autoload
(defun mpereira-html-escape (&optional start end)
  "Escape characters to HTML entities in region between START and END.
If no region is selected, operate on the whole buffer."
  (interactive "r")
  (unless start (setq start (point-min)))
  (unless end (setq end (point-max)))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "\\([^[:word:]]\\)" nil t)
        (let* ((match (match-string 1))
               (entity (mpereira-html-escape-unescape--unicode-to-entity match)))
          (when entity
            (setq entity (concat "&" entity ";")))
          (replace-match (or entity match) nil nil))))))

;;;###autoload
(defun mpereira-html-unescape (&optional start end)
  "Unescape HTML entities to characters in region between START and END.
If no region is selected, operate on the whole buffer."
  (interactive "r")
  (unless start (setq start (point-min)))
  (unless end (setq end (point-max)))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "\\(&\\([[:word:]]+\\);\\)" nil t)
        (replace-match (or (mpereira-html-escape-unescape--entity-to-unicode (match-string 2))
                           (match-string 1))
                       nil nil)))))

(provide 'mpereira-html-escape-unescape)

;;; mpereira-html-escape-unescape.el ends here

