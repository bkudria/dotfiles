;;; color-theme-scanner-brightly.el --- A color theme by Benjamin Kudria

;; Copyright © 2008 Benjamin Kudria <ben@kudria.net>

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING, or type `C-h C-c'. If not,
;; write to the Free Software Foundation at this address:

;;	 Free Software Foundation
;;	 51 Franklin Street, Fifth Floor
;;	 Boston, MA 02110-1301
;;	 USA

;;; Commentary:

;;; Code:

(require 'color-theme)

;; Placate the byte compiler and elint
(defvar window-system)
(defvar max-lisp-eval-depth)
(defvar max-specpdl-size)

;; Ensuring `max-lisp-eval-depth' and `max-specpdl-size' are big enough
;; for the backquote in `color-theme-scanner-brightly', as well as for running
;; `elint-current-buffer' on this file.
(when (< max-lisp-eval-depth 1500)
  (setq max-lisp-eval-depth 1500))
(when (< max-specpdl-size 3000)
  (setq max-specpdl-size 3000))


(defun color-theme-scanner-brightly ()
  "Color theme by Benjamin Kudria <ben@kudria.net>."
  (interactive)
  (setq-default hl-paren-colors '("#bb6400" "#afbb00" "#4bbb00" "#00bb19" "#00bb7d" "#0096bb" "#0032bb" "#3200bb" "#9600bb" "#bb007d" "#bb0019" "#bb0000"))
  (color-theme-install
   `(color-theme-scanner-brightly
	 ;; Frame parameters
	 ((foreground-color . "#1d1d1d")
	  (background-color . "white")
	  (mouse-color		. "#ff00ff")
	  (cursor-color		. "black")
	  (border-color		. "#ff00ff")
	  (background-mode	. light))
	 ;; Face variables
	 ((Man-overstrike-face . font-lock-variable-name-face)
	  (Man-underline-face  . font-lock-string-face)

	  (dired-header-face	. dired-header)
	  (dired-mark-face		. dired-mark)
	  (dired-marked-face	. dired-marked)
	  (dired-flagged-face	. dired-flagged)
	  (dired-warning-face	. dired-warning)
	  (dired-directory-face . dired-directory)
	  (dired-symlink-face	. dired-symlink)
	  (dired-ignored-face	. dired-ignored)

	  (filladapt-debug-indentation-face-1 . highlight)
	  (filladapt-debug-indentation-face-2 . secondary-selection)
	  (filladapt-debug-paragraph-face	  . bold)

	  (ibuffer-deletion-face . font-lock-warning-face)
	  (ibuffer-filter-group-name-face . bold)
	  (ibuffer-marked-face . font-lock-comment-face)
	  (ibuffer-title-face . dired-header)

	  (list-matching-lines-face				. bold)
	  (list-matching-lines-buffer-name-face . font-lock-type-face)

	  (hl-line-face . hl-line)

	  (view-highlight-face . highlight)

	  (widget-mouse-face . highlight))
	 ;; Faces
	 (default ((t (:background "white" :foreground "#1d1d1d"
							   :stipple nil :box nil :strike-through nil :overline nil
							   :underline nil :slant normal :weight normal
							   :inverse-video nil))))

	 (bold ((t (:bold t))))
	 (bold-italic ((t (:bold t :slant italic))))
	 (italic ((t (:slant italic))))
	 (underline ((t (:underline t))))

	 (border ((t nil)))

;;;		 (change-log-date-face ((t (:foreground "lemon chiffon"))))
;;;		 (change-log-name-face ((t (:foreground "khaki"))))
;;;		 (change-log-email-face ((t (:foreground "indian red"))))
;;;		 (change-log-file-face ((t (:foreground "cadet blue"))))
;;;		 (change-log-list-face ((t (:foreground "LightSteelBlue4"))))
;;;		 (change-log-conditionals-face ((t (:foreground "indian red"))))
;;;		 (change-log-function-face ((t (:foreground "indian red"))))
;;;		 (change-log-acknowledgement-face ((t (:background "cadet blue" :foreground "gray4"))))

;;;		 (comint-highlight-input ((t (:bold t))))
;;;		 (comint-highlight-prompt ((t (:foreground "pale violet red"))))

;;;		 (compilation-warning-face ((t (:background "indian red" :foreground "white"))))
;;;		 (compilation-warning ((t (:background "indian red" :foreground "white"))))
;;;		 (compilation-info-face ((t (:foreground "dark orchid"))))
;;;		 (compilation-info ((t (:foreground "dark orchid"))))
;;;		 (compilation-error-face ((t (:foreground "dark orchid"))))
;;;		 (compilation-error ((t (:foreground "dark orchid"))))

;;;		 (css-selector ((t (:foreground "#FF0DC3"))))
;;;		 (css-property ((t (:foreground "#FF5F0F"))))

	 (cursor ((t (:background "#ff00ff" :foreground "yellow"))))

;;;		 (custom-button-face ((t (:foreground "gray4" :background "light gray" :bold t :underline t))))
;;;		 (custom-button-pressed-face ((t (:foreground "gray4" :background "light gray" :bold t
;;;													  :underline t))))
;;;		 (custom-face-tag-face ((t (:bold t))))
;;;		 (custom-variable-tag-face ((t (:bold t))))
;;;		 (custom-state-face ((t (:foreground "medium sea green"))))

;;;		 (diff-hunk-header-face ((t (:background "gray30" :foreground "white"))))

	 (dired-directory ((t (:foreground "#088480"))))
	 (diredp-dir-priv ((t (:backgreound nil :foreground "#088480"))))
	 (dired-face-directory ((t (:foreground "#088480"))))
	 (dired-face-boring ((t (:foreground: "white"))))
	 (dired-face-executable ((t (:foreground "#BB0B0B"))))
	 (dired-face-flagged ((t (:foreground "red" :bold t))))
	 (dired-header ((t (:background "#bbbbbb"))))
	 (dired-face-header ((t (:background "#bbbbbb"))))
	 (dired-mark ((t (:bold t))))
	 (diredp-flag-mark ((t (:bold t))))
	 (dired-marked ((t (:bold t))))
	 (dired-face-marked ((t (:bold t))))
	 (diredp-flag-mark-line ((t (:background "#444444" :bold t))))
	 (dired-face-permissions ((t (:foreground "#777777"))))
	 (diredp-other-priv ((t (:foreground: "#000000" :background "#FF0F0F" :underline t))))
	 (dired-face-setuid ((t (:foreground "#000000" :underline t))))
	 (dired-face-socket ((t (:foreground "#33DD0D"))))
	 (dired-symlink ((t (:foreground "#BB0B86" :italic t))))
	 (diredp-link-priv ((t (:background "#BB0B86" :foreground "black" :italic t))))
	 (diredp-symlink ((t (:foreground "#BB0B86" :italic t))))
	 (dired-face-symlink ((t (:foreground "#BB0B86" :italic t))))
	 (dired-ignored ((t (:foreground "#555555"))))
	 (diredp-ignored-file-name ((t (:foreground "#555555"))))
	 (diredp-write-priv ((t (:background: "#FF5F0F"))))
	 (dired-perm-write ((t (:foreground: "#FF5F0F"))))
	 (dired-warning ((t (:foreground "#96BD0B" :bold t))))
	 (diredp-compressed-file-suffix ((t (:foreground "#97B00A"))))
	 (diredp-date-time ((t (:italic t :foreground "#666666"))))
	 (diredp-deletion ((t (:foreground "red" :bold t))))
	 (diredp-deletion-file-name ((t (:foreground "red" :bold t))))
	 (diredp-dir-heading ((t (:bold t :italic t :foreground "#0E79EB"))))
	 (diredp-display-msg ((t (:background "#00ff00"))))
	 (diredp-exec-priv ((t (:foreground "black" :background "#D40C3B"))))
	 (diredp-executable-tag ((t (:foreground "#D40C3B"))))
	 (diredp-file-name ((t (:foreground "white"))))
	 (diredp-file-suffix ((t (:foreground "#999999"))))
	 (diredp-no-priv ((t (:background "white" :foreground "#aaaaaa"))))
	 (diredp-rare-priv ((t (:background "#ff00ff"))))
	 (diredp-read-priv ((t (:foreground "black" :background "#0fff77"))))


;;;		 (ediff-current-diff-face-A ((t (:background "cadet blue" :foreground "gray4"))))
;;;		 (ediff-current-diff-face-Ancestor ((t (:background "IndianRed1"
;;;															:foreground "light gray"))))
;;;		 (ediff-current-diff-face-B ((t (:background "lemon chiffon" :foreground "gray30"))))
;;;		 (ediff-current-diff-face-C ((t (:background "medium sea green"
;;;													 :foreground "light gray"))))
;;;		 (ediff-even-diff-face-A ((t (:background "cadet blue" :foreground "gray4" :bold t))))
;;;		 (ediff-even-diff-face-Ancestor ((t (:background "IndianRed1"
;;;														 :foreground "light gray"
;;;														 :bold t))))
;;;		 (ediff-even-diff-face-B ((t (:background "lemon chiffon" :foreground "gray30"
;;;												  :bold t))))
;;;		 (ediff-even-diff-face-C ((t (:background "medium sea green" :foreground "light gray"
;;;												  :bold t))))
;;;		 (ediff-fine-diff-face-A ((t (:background "steel blue" :foreground "gray4"))))
;;;		 (ediff-fine-diff-face-Ancestor ((t (:background "orange red"
;;;														 :foreground "gray4"))))
;;;		 (ediff-fine-diff-face-B ((t (:background "LightSteelBlue4" :foreground "white"))))
;;;		 (ediff-fine-diff-face-C ((t (:background "DarkGreen"
;;;												  :foreground "gray4"))))
;;;		 (ediff-odd-diff-face-A ((t (:background "steel blue" :foreground "gray4"
;;;												 :bold t))))
;;;		 (ediff-odd-diff-face-Ancestor ((t (:background "orange red"
;;;														:foreground "gray4"
;;;														:bold t))))
;;;		 (ediff-odd-diff-face-B ((t (:background "LightSteelBlue4" :foreground "white" :bold t))))
;;;		 (ediff-odd-diff-face-C ((t (:background "DarkGreen"
;;;												 :foreground "gray4" :bold t))))

	 (file-name-shadow ((t (:foreground: "#444444"))))
;;;		 (font-latex-bold-face ((t (:bold t :foreground "medium sea green"))))
;;;		 (font-latex-italic-face ((t (:slant italic :foreground "medium sea green"))))
;;;		 (font-latex-math-face ((t (:foreground "sandy brown"))))
;;;		 (font-latex-sectioning-0-face ((t (:foreground "khaki"))))
;;;		 (font-latex-sectioning-1-face ((t (:foreground "khaki"))))
;;;		 (font-latex-sectioning-2-face ((t (:foreground "khaki"))))
;;;		 (font-latex-sectioning-3-face ((t (:foreground "khaki"))))
;;;		 (font-latex-sectioning-4-face ((t (:foreground "khaki"))))
;;;		 (font-latex-sedate-face ((t (:foreground: "#444444"))))
;;;		 (font-latex-string-face ((t (:foreground "lemon chiffon"))))
;;;		 (font-latex-subscript-face ((t ())))
;;;		 (font-latex-title-1-face ((t (:foreground "LightSteelBlue4"))))
;;;		 (font-latex-title-2-face ((t (:foreground "LightSteelBlue4"))))
;;;		 (font-latex-title-3-face ((t (:foreground "LightSteelBlue4"))))
;;;		 (font-latex-title-4-face ((t (:foreground "LightSteelBlue4"))))
;;;		 (font-latex-verbatim-face ((t ())))
;;;		 (font-latex-warning-face ((t (:background "indian red" :foreground "white"))))


	 (font-lock-builtin-face ((t (:foreground "#ff00ff"))))
	 (font-lock-color-constant-face ((t (:foreground "#CBFF0E"))))
	 (font-lock-comment-face ((t (:foreground "#9b9b9b" :italic t))))
	 (font-lock-comment-delimiter-face ((t (:foreground "#555555"))))
	 (font-lock-constant-face ((t (:foreground "#98bb0a"))))
	 (font-lock-doc-face ((t (:foreground "#777777"))))
	 (font-lock-doc-string-face ((t (:foreground "#777777"))))
	 (font-lock-function-name-face ((t (:foreground "#0b75bb"))))
	 (font-lock-keyword-face ((t (:foreground "#808080" :bold t))))
	 (font-lock-preprocessor-face ((t (:foreground "#ff00ff"))))
	 (font-lock-reference-face ((t (:foreground "#ff00ff"))))
	 (font-lock-string-face ((t (:foreground "#BB450B" :italic t))))
	 (font-lock-type-face ((t (:foreground "#ff00ff"))))
	 (font-lock-variable-name-face ((t (:foreground "#0BBBB5"))))
	 (font-lock-warning-face ((t (:foreground "#bb0b0b"))))
	 (font-lock-negation-char-face ((t (:foreground "#099191"))))
	 (font-lock-other-type-face ((t (:foreground "#808080" :italic t))))
	 (font-lock-regexp-grouping-construct	 ((t (:foreground "#97B009"))))
	 (font-lock-special-keyword-face ((t (:foreground "#0B0BBB"))))
	 (font-lock-exit-face ((t (:foreground "#861506"))))
	 (font-lock-other-emphasized-face	 ((t (:foreground "#999999" :bold t :italic t))))
	 (font-lock-regexp-grouping-backslash ((t (:foreground "#0bbb0b"))))
	 (font-lock-special-comment-face ((t (:foreground "#0bbb0b"))))

	 (fringe ((t (:background "#eeeeee" :foreground nil))))

;;;		 (header-line ((t (:box (:line-width 2
;;;											 :color "gray8"
;;;											 :style nil)
;;;								:background "gray8" :foreground "white"))))

;;;		 (help-argument-name ((t (:foreground "IndianRed1"))))

;;;		 (html-helper-bold-face ((t ())))
;;;		 (html-helper-italic-face ((t ())))
;;;		 (html-helper-underline-face ((t ())))
;;;		 (html-helper-strikethrough-face ((t ())))
;;;		 (html-helper-link-face ((t ())))
;;;		 (html-helper-significant-tag-face ((t (:foreground "cadet blue"))))

;;;		 (hyper-apropos-documentation ((t (:foreground "white"))))
;;;		 (hyper-apropos-hyperlink ((t (:underline t))))
;;;		 (hyper-apropos-major-heading ((t (:foreground "lemon chiffon"))))
;;;		 (hyper-apropos-section-heading ((t (:foreground "khaki"))))
;;;		 (hyper-apropos-apropos-heading ((t (:foreground "sandy brown"))))
;;;		 (hyper-apropos-apropos-warning ((t (:background "indian red" :foreground "white"))))


	 (highlight ((t (:background "#d5d5d5" :foreground nil))))

	 (highline-face ((t (:background "#dddddd" :foreground "black"))))
	 (highline-vertical-face ((t (:background "#dddddd" :foreground "black"))))

	 ;; flymake
	 (flymake-errline ((t (:background "#F9CCCA"))))
	 (flymake-warnline ((t (:background "#DFBFFF"))))

;;;		 (Info-title-1-face ((t (:foreground "IndianRed1"))))
;;;		 (Info-title-2-face ((t (:foreground "IndianRed1"))))
;;;		 (Info-title-3-face ((t (:foreground "IndianRed1"))))
;;;		 (Info-title-4-face ((t (:foreground "IndianRed1"))))
;;;		 (info-header-node ((t (:foreground "IndianRed1"))))
;;;		 (info-header-xref ((t (:foreground "cadet blue"))))
;;;		 (info-menu-5 ((t ())))
;;;		 (info-menu-star ((t ())))
;;;		 (info-menu-header ((t (:foreground "IndianRed1"))))
;;;		 (info-node ((t (:foreground "IndianRed1"))))
;;;		 (info-title-1 ((t (:foreground "IndianRed1"))))
;;;		 (info-title-2 ((t (:foreground "IndianRed1"))))
;;;		 (info-title-3 ((t (:foreground "IndianRed1"))))
;;;		 (info-title-4 ((t (:foreground "IndianRed1"))))
;;;		 (info-xref ((t (:foreground "cadet blue"))))

;;;		 (isearch ((t (:background "steel blue" :foreground "lemon chiffon"))))
;;;		 (isearch-lazy-highlight-face ((t (:background "DarkGreen"
;;;													   :foreground "lemon chiffon"))))

;;;		 (lazy-highlight ((t (:background "indian red" :foreground "lemon chiffon"))))
;;;		 (isearch-secondary ((t (:background "indian red" :foreground "lemon chiffon"))))
;;;		 (isearch-fail ((t (:background "indian red" :foreground "lemon chiffon"))))


;;;		 (jde-bug-breakpoint-cursor ((t (:background "cyan"))))
;;;		 (jde-db-active-breakpoint-face ((t (:background "cyan"))))
;;;		 (jde-db-requested-breakpoint-face ((t (:background "cyan"))))
;;;		 (jde-db-spec-breakpoint-face ((t (:background "cyan"))))
;;;		 (jde-java-font-lock-api-face ((t (:background "cyan"))))
;;;		 (jde-java-font-lock-bold-face ((t (:bold t :foreground "dark orchid"))))
;;;		 (jde-java-font-lock-code-face ((t (:foreground "indian red"
;;;														:background "cadet blue"))))
;;;		 (jde-java-font-lock-constant-face ((t (:foreground "pale violet red"))))
;;;		 (jde-java-font-lock-doc-tag-face ((t (:foreground "khaki"
;;;														   :background "cadet blue"))))
;;;		 (jde-java-font-lock-italic-face ((t (:slant italic :foreground "dark orchid"))))
;;;		 (jde-java-font-lock-link-face ((t (:background "cyan"))))
;;;		 (jde-java-font-lock-modifier-face ((t (:foreground "LightSteelBlue4"))))
;;;		 (jde-java-font-lock-number-face ((t (:foreground "khaki"))))
;;;		 (jde-java-font-lock-operator-face ((t (:background "cyan"))))
;;;		 (jde-java-font-lock-package-face ((t (:foreground "khaki"))))
;;;		 (jde-java-font-lock-pre-face ((t (:background "cyan"))))
;;;		 (jde-java-font-lock-private-face ((t (:background "cyan"))))
;;;		 (jde-java-font-lock-protected-face ((t (:background "cyan"))))
;;;		 (jde-java-font-lock-public-face ((t (:background "cyan"))))
;;;		 (jde-java-font-lock-underline-face ((t (:underline t :foreground "dark orchid"))))

;;;		 (js2-builtin-face ((t (:foreground "sandy brown"))))
;;;		 (js2-comment-face ((t (:foreground "dark orchid"))))
;;;		 (js2-constant-face ((t (:foreground "pale violet red"))))
;;;		 (js2-error-face ((t (:background "indian red" :foreground "white" :bold t))))
;;;		 (js2-external-variable-face ((t (:foreground "indian red"))))
;;;		 (js2-function-name-face ((t (:foreground "cadet blue"))))
;;;		 (js2-function-param-face ((t (:foreground "IndianRed1"))))
;;;		 (js2-instance-member-face ((t (:foreground "IndianRed1"))))
;;;		 ;; Copied from `nxml-tag-delimiter'
;;;		 (js2-jsdoc-html-tag-delimiter-face ((t (:foreground: "#444444"))))
;;;		 ;; Copied from `nxml-element-local-name'
;;;		 (js2-jsdoc-html-tag-name-face ((t (:foreground "cadet blue"))))
;;;		 (js2-jsdoc-tag-face ((t (:foreground "medium orchid"))))
;;;		 (js2-jsdoc-type-face ((t (:foreground "medium orchid"))))
;;;		 (js2-jsdoc-value-face ((t (:foreground "medium orchid"))))
;;;		 (js2-keyword-face ((t (:foreground "steel blue"))))
;;;		 (js2-private-function-call-face ((t (:foreground "cadet blue"))))
;;;		 (js2-private-member-face ((t (:foreground "IndianRed1"))))
;;;		 (js2-regexp-face ((t (:foreground "khaki"))))
;;;		 (js2-string-face ((t (:foreground "lemon chiffon"))))
;;;		 (js2-type-face ((t (:foreground "medium sea green"))))
;;;		 (js2-variable-name-face ((t (:foreground "IndianRed1"))))
;;;		 (js2-warning-face ((t (:background "indian red" :foreground "white"))))

;;;		 (match ((t (:background "DodgerBlue3" :foreground "light gray"))))

;;;		 (menu ((t (:background "light gray" :foreground "gray4"))))

	 (minibuffer-prompt ((t (:foreground "#00494D" :bold t))))

	 (mode-line ((t (:foreground "#00494D" :background "#dddddd" :overline "#999999"))))
	 (modeline ((t (:foreground "#00494D" :background "#dddddd" :overline "#999999"))))

	 (mode-line-inactive ((t (:foreground "#00494D" :background "#dddddd" :overline "white"))))
	 (mode-line-highlight ((t (:foreground "#434343" :background "#aaaaaa"))))
	 (modeline-buffer-id ((t (:bold t :foreground "#00316E" :background nil))))
	 (modeline-mousable ((t (:foreground "#aaaaaa" :background "dark slate blue"))))
	 (modeline-mousable-minor-mode ((t (:foreground "#aaaaaa" :background "dark slate blue"))))
	 (mouse ((t (:foreground "#aaaaaa" :background "gray4"))))

	 (multi-region-face ((t (:foreground "#543B00"))))

	 ;; (nxml-heading ((t ())))
	 ;; (nxml-outline-indicator ((t ())))
	 ;; (nxml-outline-active-indicator ((t ())))
	 ;; (nxml-outline-ellipsis ((t ())))
	 ;; (nxml-delimited-data ((t ())))
	 ;; (nxml-name ((t ())))
	 ;; (nxml-ref ((t ())))
	 ;; (nxml-delimiter ((t ())))
	 ;; (nxml-text ((t ())))
	 ;;		 (nxml-comment-content ((t (:foreground "dark orchid"))))
	 ;;		 (nxml-comment-delimiter ((t (:foreground "medium orchid"))))
	 ;;		 (nxml-processing-instruction-delimiter ((t (:foreground: "#444444"))))
	 ;;		 (nxml-processing-instruction-target ((t (:foreground "steel blue"))))
	 ;;		 (nxml-processing-instruction-content ((t ())))
	 ;;		 (nxml-cdata-section-delimiter ((t (:foreground: "#444444"))))
	 ;;		 (nxml-cdata-section-CDATA ((t (:foreground "cadet blue"))))
	 ;;		 (nxml-cdata-section-content ((t ())))
	 ;;		 (nxml-char-ref-number ((t (:foreground "IndianRed1"))))
	 ;;		 (nxml-char-ref-delimiter ((t (:foreground "IndianRed1"))))
	 ;;		 (nxml-entity-ref-name ((t (:foreground "IndianRed1"))))
	 ;;		 (nxml-entity-ref-delimiter ((t (:foreground "IndianRed1"))))
	 ;;		 (nxml-tag-delimiter ((t (:foreground: "#444444"))))
	 ;;		 (nxml-tag-slash ((t (:foreground: "#444444"))))
	 ;;		 (nxml-element-prefix ((t (:foreground "steel blue"))))
	 ;;		 (nxml-element-colon ((t (:foreground "steel blue"))))
	 ;;		 (nxml-element-local-name ((t (:foreground "cadet blue"))))
	 ;;		 (nxml-attribute-prefix ((t (:foreground "sandy brown"))))
	 ;;		 (nxml-attribute-colon ((t (:foreground "sandy brown"))))
	 ;;		 (nxml-attribute-local-name ((t (:foreground "sandy brown"))))
	 ;;		 (nxml-namespace-attribute-xmlns ((t (:foreground "sandy brown"))))
	 ;;		 (nxml-namespace-attribute-colon ((t (:foreground "sandy brown"))))
	 ;;		 (nxml-namespace-attribute-prefix ((t (:foreground "sandy brown"))))
	 ;;		 (nxml-attribute-value ((t (:foreground "lemon chiffon"))))
	 ;;		 (nxml-attribute-value-delimiter ((t (:foreground "lemon chiffon"))))
	 ;; (nxml-namespace-attribute-value ((t ())))
	 ;; (nxml-namespace-attribute-value-delimiter ((t ())))
;;;		 (nxml-prolog-literal-delimiter ((t (:foreground "lemon chiffon"))))
;;;		 (nxml-prolog-literal-content ((t (:foreground "lemon chiffon"))))
;;;		 (nxml-prolog-keyword ((t (:foreground "cadet blue"))))
;;;		 (nxml-markup-declaration-delimiter ((t (:foreground: "#444444"))))
	 ;; (nxml-hash ((t ())))
	 ;; (nxml-glyph ((t ())))

	 ;; (linum ((t (:foreground "#FFBB00"))))

	 (hl-line ((t (:background "#f2f2f2" :foreground nil))))
	 (highline-face ((t (:background "#f2f2f2" :foreground nil))))
	 (highlight-current-line-face ((t (:background "#f2f2f2" :foreground nil))))

	 (paren-face ((t (:background nil :foreground "#dddddd" :bold ))))
	 (hl-paren-face ((t (:background nil :foreground "yellow"))))

	 (paren-face-match ((t (:background nil :foreground "#009966" :bold t))))
	 (show-paren-match ((t (:background nil :foreground "#009966" :bold t))))
	 (show-paren-match-face ((t (:background nil :foreground "#009966" :bold t))))
	 (paren-match ((t (:background nil :foreground "purple"))))

	 (paren-face-mismatch ((t (:background nil :foreground nil :underline "red" :bold t))))
	 (show-paren-mismatch ((t (:background nil :foreground nil :underline "red" :bold t))))
	 (show-paren-mismatch-face ((t (:background nil :foreground nil :underline "red" :bold t))))
	 (paren-mismatch ((t (:background nil :foreground nil :underline "red" :bold t))))

	 (paren-face-no-match ((t (:background nil :foreground nil :underline "red" :bold t))))
	 (paren-no-match-face ((t (:background nil :foreground nil :underline "red" :bold t))))


	 (whitespace-empty ((t (:foreground "#bb8c00"))))
	 (whitespace-hspace ((t (:foreground "#dddddd"))))
	 (whitespace-indentation ((t (:foreground "#bb8c00"))))
	 (whitespace-line ((t (:background nil :foreground nil :underline "#888888"))))
	 (whitespace-space ((t (:foreground "#dddddd"))))
	 (whitespace-space-after-tab ((t (:foreground "#bb8c00"))))
	 (whitespace-space-before-tab ((t (:foreground "#803e00" :weight bold))))
	 (whitespace-tab ((t (:foreground "#dddddd"))))
	 (whitespace-trailing ((t (:background "white" :foreground "#bf0000" :weight bold))))



	 (primary-selection ((t (:background "#a8dee0" :foreground nil))))



	 ;;	 (svn-status-marked-face ((t (:background "cadet blue" :foreground "gray4"))))
	 ;;		 (svn-status-marked-popup-face ((t (:background "indian red" :foreground "white" :bold t))))
	 ;;		 (svn-status-update-available-face ((t (:background "indian red" :foreground "white"))))
	 ;;		 (svn-status-directory-face ((t (:foreground "khaki"))))
	 ;;		 (svn-status-filename-face ((t (:foreground "cadet blue"))))
	 ;;		 (svn-status-symlink-face ((t (:foreground "cadet blue"))))
	 ;;		 (svn-status-locked-face ((t (:foreground "dark orchid"))))
	 ;;		 (svn-status-switched-face ((t (:foreground "lemon chiffon"))))
	 ;;		 (svn-status-blame-highlight-face ((t (:background "DarkGreen" :foreground "light gray" :underline t))))
	 ;;		 (svn-status-blame-rev-number-face ((t (:background "cadet blue" :foreground "gray4"))))


	 (region ((t (:background "#a8dee0" :foreground nil))))
	 (rng-error ((t (:foreground: "red" :bold t))))


	 (scroll-bar ((t (:background "light gray" :foreground "#543B00"))))

	 (secondary-selection ((t (:background "#a8de00" :foreground nil))))

;;;		 (sgml-namespace-face ((t (:foreground "steel blue"))))

	 (shadow ((t (:foreground: "#444444"))))
	 ;;	 (sh-heredoc-face ((t (:foreground "khaki"))))
	 ;;		 (sh-quoted-exec ((t (:foreground "khaki"))))

	 (show-tabs-space-face ((t (:background "IndianRed1" :foreground "light gray"))))
	 (show-tabs-tab-face ((t (:background "IndianRed1" :foreground "light gray"))))

;;;		 (slime-error-face ((t (:background "indian red" :foreground "white" :bold t))))
;;;		 (slime-warning-face ((t (:background "indian red" :foreground "white"))))
;;;		 (slime-style-warning-face ((t (:background "indian red" :foreground "white"))))
;;;		 (slime-note-face ((t (:background "indian red" :foreground "white"))))
;;;		 (slime-highlight-face ((t (:background "indian red" :foreground "white"))))

;;;		 (slime-repl-prompt-face ((t (:foreground "pale violet red"))))
;;;		 (slime-repl-output-face ((t (:foreground "dark orchid"))))
;;;		 (slime-repl-input-face ((t (:foreground "pale violet red"))))
;;;		 (slime-repl-result-face ((t (:foreground "medium orchid"))))

;;;		 (slime-inspector-topline-face ((t (:background "gray30" :foreground "white"))))
;;;		 (slime-inspector-label-face ((t (:foreground "indian red"))))
;;;		 (slime-inspector-value-face ((t (:foreground "light pink"))))
;;;		 (slime-inspector-action-face ((t (:background "cyan"))))
;;;		 (slime-inspector-type-face ((t (:foreground "light sea green"))))

;;;		 (slime-reader-conditional-face ((t (:foreground: "#444444"))))

;;;		 (speedbar-button-face ((t (:foreground "medium sea green"))))
;;;		 (speedbar-directory-face ((t (:foreground "khaki"))))
;;;		 (speedbar-file-face ((t (:foreground "cadet blue"))))
;;;		 (speedbar-highlight-face ((t (:background "DarkGreen" :foreground "light gray" :underline t))))
;;;		 (speedbar-selected-face ((t (:background "cadet blue" :foreground "gray4"))))
;;;		 (speedbar-tag-face ((t (:foreground "khaki"))))

;;;		 (subscript ((t ())))
;;;		 (superscript ((t ())))
;;;		 (tex-math-face ((t (:foreground "sandy brown"))))
;;;		 (tex-math ((t (:foreground "sandy brown"))))
;;;		 (tex-verbatim-face ((t ())))
;;;		 (tex-verbatim ((t ())))

;;;		 (tool-bar ((t (:background "light gray"
;;;									:foreground "gray4"
;;;									:inverse-video nil
;;;									:underline t))))

;;;		 (trailing-whitespace ((t (:background "IndianRed1" :foreground "light gray"))))




	 (whitespace-highlight-face ((t (:background "IndianRed1" :foreground "light gray"))))
	 (widget-button-face ((t (:bold t :underline t))))
	 (widget-button-pressed-face ((t (:bold t :underline t))))
	 (widget-documentation-face ((t (:foreground "medium sea green"))))
	 (widget-field-face ((t (:background "gray30" :foreground "white"))))
	 (widget-inactive-face ((t (:foreground: "#444444"))))
	 (widget-single-line-field-face ((t (:background "gray30" :foreground "white")))))))

(provide 'color-theme-scanner-brightly)

;;; color-theme-scanner-brightly.el ends here
