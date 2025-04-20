;;; better-grubox.el -*- lexical-binding: t; -*-
;;; https://github.com/thriveth/Gruvbox-goodies/blob/master/Emacs/gruvbox-conf.el

(custom-theme-set-faces! '(doom-gruvbox doom-gruvbox-light)
  `(cursor                  :background ,(doom-color 'orange))
  `(shadow                  :background ,(doom-color 'bg) :foreground ,(doom-color 'base6))
  `(line-number             :background ,(doom-color 'bg) :foreground ,(doom-color 'base4))
  `(link        :overline t :background ,(doom-color 'bg) :foreground ,(doom-color 'green))

  `(org-block-end-line        :inherit org-block-begin-line)
  `(org-document-info-keyword :inherit shadow)
  `(org-meta-line             :inherit shadow)
  `(org-target :height 0.7    :inherit shadow)
  `(org-indent                :inherit org-hide)
  `(org-indent                :inherit (org-hide fixed-pitch))
  `(org-ref-label-face        :inherit shadow :box t)
  `(org-drawer                :inherit shadow)
  `(org-property-value        :inherit org-document-info t)
  `(org-tag                   :inherit shadow)
  `(org-quote                 :inherit org-block :slant italic)
  `(org-ellipsis              :inherit shadow :extend t :height 1.0 :weight extra-light)
  `(org-verbatim              :inherit org-block    :background ,(doom-color 'bg) :foreground ,(doom-color 'base5))
  `(org-code                  :inherit org-verbatim :background ,(doom-color 'bg) :foreground ,(doom-color 'orange))

  `(org-document-title   :extend t :height 1.4        :background ,(doom-color 'bg) :foreground ,(doom-color 'base8) :weight ultraheavy)
  `(org-block-begin-line :extend t :inherit org-block :background ,(doom-color 'bg) :foreground ,(doom-color 'base4))
  `(org-block            :extend t                    :background ,(doom-color 'bg) :foreground ,(doom-color 'base6))

  `(org-headline-done :background ,(doom-color 'bg) :foreground ,(doom-color 'fg)    :overline nil :weight extra-light :extend t)
  `(org-link          :background ,(doom-color 'bg) :foreground ,(doom-color 'green)   :overline nil)
  `(org-footnote      :background ,(doom-color 'bg) :foreground ,(doom-color 'cyan)    :overline nil)
  `(org-ref-cite-face :background ,(doom-color 'bg) :foreground ,(doom-color 'yellow)  :overline nil)
  `(org-ref-ref-face  :background ,(doom-color 'bg) :foreground ,(doom-color 'blue)    :overline nil)
  `(org-date          :background ,(doom-color 'bg) :foreground ,(doom-color 'blue)    :underline t)
  `(org-document-info :background ,(doom-color 'bg) :foreground ,(doom-color 'base5)   :weight bold)
  `(org-table         :background ,(doom-color 'bg) :foreground ,(doom-color 'base5))

  `(org-level-1       :extend t :height 1.0 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color 'magenta) :weight ultraheavy)
  `(org-level-2       :extend t :height 1.0 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color 'blue)    :weight heavy)
  `(org-level-3       :extend t :height 1.0 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color 'cyan)    :weight extrabold)
  `(org-level-4       :extend t :height 1.0 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color 'green)   :weight bold)
  `(org-level-5       :extend t :height 1.0 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color 'yellow)  :weight demi)
  `(org-level-6       :extend t :height 1.0 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color 'orange)  :weight medium)
  `(org-level-7       :extend t :height 1.0 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color 'red)     :weight normal)

  ;; `(+org-todo-active               :height 1.0 :background ,(doom-color 'bg) :foreground ,(doom-color 'magenta) :weight normal)
  ;; `(+org-todo-cancel               :height 1.0 :background ,(doom-color 'bg) :foreground ,(doom-color 'magenta) :weight normal)
  ;; `(+org-todo-onhold               :height 1.0 :background ,(doom-color 'bg) :foreground ,(doom-color 'magenta) :weight normal)
  ;; `(+org-todo-project              :height 1.0 :background ,(doom-color 'bg) :foreground ,(doom-color 'magenta) :weight normal)

  ;; `(org-headline-todo              :height 1.0 :background ,(doom-color 'magenta) :foreground ,(doom-color 'cyan) :weight normal)
  ;; `(org-todo                       :height 1.0 :background ,(doom-color 'magenta) :foreground ,(doom-color 'cyan) :weight normal)
  ;; `(org-done                       :height 1.0 :background ,(doom-color 'magenta) :foreground ,(doom-color 'cyan) :weight normal)
  ;; `(org-checkbox-statistics-done   :height 1.0 :background ,(doom-color 'magenta) :foreground ,(doom-color 'cyan) :weight normal)
  ;; `(org-checkbox-statistics-todo   :height 1.0 :background ,(doom-color 'magenta) :foreground ,(doom-color 'cyan) :weight normal)
  ;; `(org-checkbox                   :height 1.0 :background ,(doom-color 'magenta) :foreground ,(doom-color 'cyan) :weight normal)

  `(org-modern-label               :height 1.0 :weight bold)

  `(org-modern-progress-incomplete :height 1.0 :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'orange) :weight semi-light)
  `(org-modern-done                :height 1.0 :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'green) :weight semi-light)

  `(org-modern-todo                :height 1.0 :background ,(doom-color 'bg) :foreground ,(doom-color 'orange) :weight bold :inverse-video t)
  `(org-modern-progress-complete   :height 1.0 :background ,(doom-color 'bg) :foreground ,(doom-color 'green) :weight bold :inverse-video t)

  ;; `(org-archived                   :height 1.0 :background ,(doom-color 'magenta) :foreground ,(doom-color 'cyan) :weight normal)
  ;; `(org-default                    :height 1.0 :background ,(doom-color 'magenta) :foreground ,(doom-color 'cyan) :weight normal)
  ;; `(org-modern-symbol              :height 1.0 :background ,(doom-color 'magenta) :foreground ,(doom-color 'cyan) :weight normal)
  ;; `(org-modern-tag                 :height 1.0 :background ,(doom-color 'magenta) :foreground ,(doom-color 'cyan) :weight normal)
  ;; `(org-special-keyword            :height 1.0 :background ,(doom-color 'magenta) :foreground ,(doom-color 'cyan) :weight normal)
  ;; `(org-warning                    :height 1.0 :background ,(doom-color 'magenta) :foreground ,(doom-color 'cyan) :weight normal)
  )


(use-package! org-modern
  :config
  (defun bk/set-org-modern-todo-faces ()
    "Set org-modern-todo-faces using doom-color."
    (setq org-modern-todo-faces
          `(
            ("TODO"    :background ,(doom-color 'bg) :foreground ,(doom-color 'faded-yellow) :inverse-video t :weight normal)
            ("DOING"   :background ,(doom-color 'bg) :foreground ,(doom-color 'blue) :inverse-video t :weight heavy)
            ("UNCLEAR" :background ,(doom-color 'bg) :foreground ,(doom-color 'violet) :inverse-video t)
            )))

  ;; Call it after both org-modern and doom-themes are loaded
  (after! doom-themes
    (bk/set-org-modern-todo-faces))
  ;; Also add it to doom-load-theme-hook to ensure it updates when theme changes
  (add-hook 'doom-load-theme-hook #'bk/set-org-modern-todo-faces))


;; 01 [0 thin]
;; 02 [40 ultralight ultra-light extralight extra-light]
;; 03 [50 light]
;; 04 [55 semilight demilight semi-light]
;; 05 [80 regular normal unspecified book]
;; 06 [100 medium]
;; 07 [180 demi semibold semi-bold demibold demi-bold]
;; 08 [200 bold]
;; 09 [205 extrabold extra-bold ultrabold ultra-bold]
;; 10 [210 black heavy]
;; 11 [250 ultraheavy ultra-heavy]

;; ‘ultra-condensed’, ‘extra-condensed’, ‘condensed’,
;; ‘semi-condensed’, ‘normal’, ‘regular’, ‘medium’, ‘semi-expanded’,
;; ‘expanded’, ‘extra-expanded’, or ‘ultra-expanded’.

;; [
;; [50 ultracondensed ultra-condensed]
;; [63 extracondensed extra-condensed]
;; [75 condensed compressed narrow]
;; [87 semicondensed demicondensed semi-condensed]
;; [100 normal medium regular unspecified]
;; [113 semiexpanded demiexpanded semi-expanded]
;; [125 expanded]
;; [150 extraexpanded extra-expanded]
;; [200 ultraexpanded ultra-expanded wide]]
