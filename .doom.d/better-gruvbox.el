;;; better-grubox.el -*- lexical-binding: t; -*-
;;; https://github.com/thriveth/Gruvbox-goodies/blob/master/Emacs/gruvbox-conf.el

(custom-theme-set-faces! 'doom-gruvbox
  '(cursor                 :foreground "#928374")
  '(org-block              :foreground "#ebdbb2":background "#1c2021" :extend t)
  '(org-block-begin-line   :inherit org-block :background "#1d2021" :foreground "#665c54" :extend t)
  '(org-block-end-line     :inherit org-block-begin-line)
  '(org-document-info      :foreground "#d5c4a1" :weight bold)
  '(org-document-info-keyword    :inherit shadow)
  '(org-document-title     :foreground "#fbf1c7" :weight bold :height 1.4)
  '(org-meta-line          :inherit shadow)
  '(org-target             :height 0.7 :inherit shadow)
  '(org-link               :foreground "#b8bb26" :background "#32302f" :overline nil)  ;;
  '(org-indent             :inherit org-hide)
  '(org-indent             :inherit (org-hide fixed-pitch))
  '(org-footnote           :foreground "#8ec07c" :background "#32302f" :overline nil)
  '(org-ref-cite-face      :foreground "#fabd2f" :background "#32302f" :overline nil)  ;;
  '(org-ref-ref-face       :foreground "#83a598" :background "#32302f" :overline nil)
  '(org-ref-label-face     :inherit shadow :box t)
  '(org-drawer             :inherit shadow)
  '(org-property-value     :inherit org-document-info t)
  '(org-tag                :inherit shadow)
  '(org-date               :foreground "#83a598" :underline t)
  '(org-verbatim           :inherit org-block :background "#3c3836" :foreground "#d5c4a1")
  '(org-code               :inherit org-verbatim :background "#3c3836" :foreground "#fe8019")
  '(org-quote              :inherit org-block :slant italic)
  '(org-level-1            :foreground "#fabd2f" :background "#282828" :weight bold :height 1.1 :overline nil :extend t) ;; Yellow
  '(org-level-2            :foreground "#fe8019" :background "#282828" :weight bold :height 1.1 :overline nil :extend t) ;; Orange
  '(org-level-3            :foreground "#fb4934" :background "#282828" :weight bold :height 1.1 :overline nil :extend t) ;; Red
  '(org-level-4            :foreground "#d3869b" :background "#282828" :weight bold :height 1.1 :overline nil :extend t) ;; Blue
  '(org-level-5            :foreground "#83a598" :background "#282828" :weight bold :height 1.1 :overline nil :extend t) ;; Blue
  '(org-level-6            :foreground "#8ec07c" :background "#282828" :weight bold :height 1.1 :overline nil :extend t) ;; Aqua
  '(org-level-7            :foreground "#b8bb26" :background "#282828" :weight bold :height 1.1 :overline nil :extend t) ;; Green
  '(org-headline-done      :foreground "#928374" :background "#282828" :weight bold :overline nil :extend t) ;; Gray
  '(org-ellipsis           :inherit shadow :height 1.0 :weight bold :extend t)
  '(org-table              :foreground "#d5c4a1" :background "#3c3836")

  '(link                              :foreground "#b8bb26" :overline t)
  '(line-number                       :background "#32302f" :foreground "#665c54")
  )
