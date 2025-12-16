;;; better-grubox.el -*- lexical-binding: t; -*-
;;; https://github.com/thriveth/Gruvbox-goodies/blob/master/Emacs/gruvbox-conf.el

;; Headline color sequences - each index corresponds to headline level (0=level-1, etc.)
(defvar bk/headline-colors-dark
  '(
    yellow
    orange
    red
    magenta
    blue
    cyan
    green
    )
  "Color sequence for headlines in dark gruvbox mode (levels 1-7).")

(defvar bk/headline-colors-light
  '(
    blue
    cyan
    green
    yellow
    orange
    red
    magenta
    )
  "Color sequence for headlines in light gruvbox mode (levels 1-7).")

(custom-theme-set-faces! 'doom-gruvbox
  `(line-number             :background ,(doom-color 'bg) :foreground ,(doom-color 'base4))
  `(link        :overline t :background ,(doom-color 'bg) :foreground ,(doom-color 'green))

  `(markdown-code-face        :background ,(doom-color 'bg-alt))

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
  `(org-modern-label               :height 1.0 :weight bold)

  `(org-modern-progress-incomplete :height 1.0 :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'orange) :weight semi-light)
  `(org-modern-done                :height 1.0 :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'green) :weight semi-light)

  `(org-modern-todo                :height 1.0 :background ,(doom-color 'bg) :foreground ,(doom-color 'orange) :weight bold :inverse-video t)
  `(org-modern-progress-complete   :height 1.0 :background ,(doom-color 'bg) :foreground ,(doom-color 'green) :weight bold :inverse-video t)

  `(cursor                  :background ,(doom-color 'yellow))
  `(shadow                  :background ,(doom-color 'bg) :foreground ,(doom-color 'base7) :weight ultralight)

  `(org-headline-done :background ,(doom-color 'bg) :foreground ,(doom-color 'green) :overline nil :weight ultralight :extend t)
  `(org-headline-todo :background ,(doom-color 'bg) :foreground ,(doom-color 'orange) :overline nil :weight light :extend t)

  `(org-level-1       :extend t :height 1.13 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 0 bk/headline-colors-dark)) :weight ultraheavy)
  `(org-level-2       :extend t :height 1.08 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 1 bk/headline-colors-dark)) :weight heavy)
  `(org-level-3       :extend t :height 1.05 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 2 bk/headline-colors-dark)) :weight extrabold)
  `(org-level-4       :extend t :height 1.03 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 3 bk/headline-colors-dark)) :weight bold)
  `(org-level-5       :extend t :height 1.02 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 4 bk/headline-colors-dark)) :weight demi)
  `(org-level-6       :extend t :height 1.01 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 5 bk/headline-colors-dark)) :weight medium)
  `(org-level-7       :extend t :height 1.0 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 6 bk/headline-colors-dark)) :weight normal)

  `(org-modern-progress-incomplete :height 1.0 :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'orange) :weight semi-light)
  `(org-modern-done                :height 1.0 :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'green) :weight semi-light)

  `(org-modern-todo                :height 1.0 :background ,(doom-color 'bg) :foreground ,(doom-color 'orange) :weight bold :inverse-video t)
  `(org-modern-progress-complete   :height 1.0 :background ,(doom-color 'bg) :foreground ,(doom-color 'green) :weight bold :inverse-video t)

  `(markdown-header-face-1       :extend t :height 1.8 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 0 bk/headline-colors-dark)) :weight ultraheavy)
  `(markdown-header-face-2       :extend t :height 1.5 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 1 bk/headline-colors-dark)) :weight heavy)
  `(markdown-header-face-3       :extend t :height 1.3 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 2 bk/headline-colors-dark)) :weight extrabold)
  `(markdown-header-face-4       :extend t :height 1.2 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 3 bk/headline-colors-dark)) :weight bold)
  `(markdown-header-face-5       :extend t :height 1.1 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 4 bk/headline-colors-dark)) :weight demi)
  `(markdown-header-face-6       :extend t :height 1.0 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 5 bk/headline-colors-dark)) :weight medium)
  `(markdown-header-face-6       :extend t :height 1.0 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 5 bk/headline-colors-dark)) :weight medium)
  )

(custom-theme-set-faces! 'doom-gruvbox-light
  `(line-number             :background ,(doom-color 'bg) :foreground ,(doom-color 'base4))
  `(link        :overline t :background ,(doom-color 'bg) :foreground ,(doom-color 'green))

  `(markdown-code-face        :background ,(doom-color 'bg-alt))

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

  `(org-level-1       :extend t :height 1.13 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 0 bk/headline-colors-light)) :weight ultraheavy)
  `(org-level-2       :extend t :height 1.08 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 1 bk/headline-colors-light)) :weight heavy)
  `(org-level-3       :extend t :height 1.05 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 2 bk/headline-colors-light)) :weight extrabold)
  `(org-level-4       :extend t :height 1.03 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 3 bk/headline-colors-light)) :weight bold)
  `(org-level-5       :extend t :height 1.02 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 4 bk/headline-colors-light)) :weight demi)
  `(org-level-6       :extend t :height 1.01 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 5 bk/headline-colors-light)) :weight medium)
  `(org-level-7       :extend t :height 1.0 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 6 bk/headline-colors-light)) :weight normal)

  `(org-modern-label               :height 1.0 :weight bold)

  `(org-modern-progress-incomplete :height 1.0 :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'orange) :weight semi-light)
  `(org-modern-done                :height 1.0 :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'green) :weight semi-light)

  `(org-modern-todo                :height 1.0 :background ,(doom-color 'bg) :foreground ,(doom-color 'orange) :weight bold :inverse-video t)
  `(org-modern-progress-complete   :height 1.0 :background ,(doom-color 'bg) :foreground ,(doom-color 'green) :weight bold :inverse-video t)

  `(cursor                  :background ,(doom-color 'teal))
  `(shadow                  :background ,(doom-color 'bg) :foreground ,(doom-color 'base7) :weight ultralight)

  `(org-headline-done :overline nil :weight light :extend t)
  `(org-headline-todo :overline nil :weight light :extend t)

  `(org-modern-progress-incomplete :height 1.0 :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'orange) :weight semi-light)
  `(org-modern-done                :height 1.0 :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'green) :weight semi-light)

  `(org-modern-todo                :height 1.0 :background ,(doom-color 'bg) :foreground ,(doom-color 'orange) :weight bold :inverse-video t)
  `(org-modern-progress-complete   :height 1.0 :background ,(doom-color 'bg) :foreground ,(doom-color 'green) :weight bold :inverse-video t)

  `(markdown-header-face-1       :extend t :height 1.8 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 0 bk/headline-colors-light)) :weight ultraheavy)
  `(markdown-header-face-2       :extend t :height 1.5 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 1 bk/headline-colors-light)) :weight heavy)
  `(markdown-header-face-3       :extend t :height 1.3 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 2 bk/headline-colors-light)) :weight extrabold)
  `(markdown-header-face-4       :extend t :height 1.2 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 3 bk/headline-colors-light)) :weight bold)
  `(markdown-header-face-5       :extend t :height 1.1 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 4 bk/headline-colors-light)) :weight demi)
  `(markdown-header-face-6       :extend t :height 1.0 :overline nil :background ,(doom-color 'bg) :foreground ,(doom-color (nth 5 bk/headline-colors-light)) :weight medium)
  )


(use-package! org-modern
  :config
  (defun bk/set-org-modern-todo-faces ()
    "Set org-modern-todo-faces using doom-color."
    (setq org-modern-todo-faces
          `(
            ("TODO"    :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'orange) :weight light)
            ("DONE"    :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'green)  :weight ultralight)
            ("DOING"   :background ,(doom-color 'bg) :foreground ,(doom-color 'blue) :inverse-video t :weight heavy)

            ("DECIDED" :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'magenta))
            ("UNCLEAR" :background ,(doom-color 'bg) :foreground ,(doom-color 'red) :inverse-video t)
            ("IDEATED" :background ,(doom-color 'bg) :foreground ,(doom-color 'orange) :inverse-video t)
            )))

  (defun bk/org-modern--todo-fixed-width ()
    "Override org-modern--todo to use fixed width labels."
    (advice-add 'org-modern--todo :override
                (lambda ()
                  "Prettify todo keywords with fixed width matching progress bars."
                  (let* ((todo (match-string-no-properties 1))
                         (beg (match-beginning 1))
                         (end (match-end 1))
                         (width org-modern-progress)
                         (todo-len (length todo))
                         (pad-len (max 0 (- width todo-len)))
                         (left-pad (/ pad-len 2))
                         (right-pad (- pad-len left-pad))
                         (padded-todo (concat (make-string left-pad ?\s)
                                              todo
                                              (make-string right-pad ?\s))))
                    (put-text-property beg end 'display padded-todo)
                    (put-text-property beg end 'face
                                       (if-let ((face (or (cdr (assoc todo org-modern-todo-faces))
                                                          (cdr (assq t org-modern-todo-faces)))))
                                           `(:inherit (,face org-modern-label))
                                         (if (string-match-p org-not-done-regexp todo)
                                             'org-modern-todo 'org-modern-done)))))))

  ;; Apply the fixed-width advice
  (bk/org-modern--todo-fixed-width)

  ;; Call it after both org-modern and doom-themes are loaded
  (after! doom-themes (bk/set-org-modern-todo-faces))
  ;; Also add it to doom-load-theme-hook to ensure it updates when theme changes
  (add-hook 'doom-load-theme-hook #'bk/set-org-modern-todo-faces))

;; Weights {{{
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
;; }}}

;; Gruvbox Dark {{{
;;  (bg                      "#32302f" "#323232" nil)
;;  (bg-alt                  "#282828" "#282828" nil)
;;  (bg-alt2                 "#504945" "#504945" "brown")
;;
;;  (base0                   "#0d1011" "black" "black")
;;  (base1                   "#1d2021" "#1d1d1d" "brightblack")
;;  (base2                   "#282828" "#282828" "brightblack")
;;  (base3                   "#3c3836" "#383838" "brightblack")
;;  (base4                   "#665c54" "#5c5c5c" "brightblack")
;;  (base5                   "#7c6f64" "#6f6f6f" "brightblack")
;;  (base6                   "#928374" "#909090" "brightblack")
;;  (base7                   "#d5c4a1" "#cccccc" "brightblack")
;;  (base8                   "#fbf1c7" "#fbfbfb" "brightwhite")
;;
;;  (fg-alt                  "#d5c4a1" "#cccccc" "brightwhite")
;;  (fg                      "#ebdbb2" "#dfdfdf" "brightwhite")
;;
;;  (grey                    "#928374" "#909090" "brightblack")
;;  (my-black                "#37302f" "#37302f" "black")
;;
;;  (red                     "#fb4934" "#e74c3c" "red")
;;  (orange                  "#fe8019" "#fd971f" "orange")
;;  (yellow                  "#fabd2f" "#fabd2f" "yellow")
;;  (dark-yellow             "#d79921" "#fabd2f" "yellow")
;;  (green                   "#b8bb26" "#b8bb26" "green")
;;  (dark-green              "#98971a" "#98971a" "green")
;;  (teal                    "#8ec07c" "#8ec07c" "green")
;;  (cyan                    "#8ec07c" "#8ec07c" "brightcyan")
;;  (dark-cyan               "#689d6a" "#689d6a" "cyan")
;;  (blue                    "#83a598" "#83a598" "brightblue")
;;  (dark-blue               "#458588" "#458588" "blue")
;;  (violet                  "#d3869b" "#d3869b" "brightmagenta")
;;  (magenta                 "#b16286" "#b16286" "magenta")
;;
;;  (builtin                 "#fe8019" "#fd971f" "orange")
;;  (comments                "#928374" "#909090" "brightblack")
;;  (constants               "#d3869b" "#d3869b" "brightmagenta")
;;  (doc-comments            "#dfd2b8" "#d8d8d8" "brightwhite")
;;  (error                   "#fb4934" "#e74c3c" "red")
;;  (functions               "#b8bb26" "#b8bb26" "green")
;;  (highlight               "#fabd2f" "#fabd2f" "yellow")
;;  (keywords                "#fb4934" "#e74c3c" "red")
;;  (methods                 "#b8bb26" "#b8bb26" "green")
;;  (modeline-bg             "#504945" "#504945" "brown")
;;  (modeline-fg             "#dfd2b8" "#d8d8d8" "brightwhite")
;;  (modeline-inactive-bg    "#443e3a" "#443e3a" "brown")
;;  (modeline-inactive-fg    "#928374" "#909090" "brightblack")
;;  (numbers                 "#d3869b" "#d3869b" "brightmagenta")
;;  (operators               "#ebdbb2" "#dfdfdf" "brightwhite")
;;  (org-quote               "#3c3a39" "#1f1f1f")
;;  (region                  "#504945" "#504945" "brown")
;;  (selection               "#504945" "#504945" "brown")
;;  (strings                 "#b8bb26" "#b8bb26" "green")
;;  (success                 "#b8bb26" "#b8bb26" "green")
;;  (type                    "#fabd2f" "#fabd2f" "yellow")
;;  (variables               "#83a598" "#83a598" "brightblue")
;;  (vc-added                "#9c9e20" "#9c9e20" "green")
;;  (vc-deleted              "#d53e2c" "#c44032" "red")
;;  (vc-modified             "#78a369" "#78a369" "brightcyan")
;;  (vertical-bar            "#504945" "#504945" "brown")
;;  (warning                 "#fabd2f" "#fabd2f" "yellow")
;; }}}

;; Gruvbox Light {{{
;;  (bg                      "#f9f5d7" "#ffffd7" nil)
;;  (bg-alt                  "#fbf1c7" "#ffffd7" nil)
;;
;;  (base0                   "#f0f0f0" "#f0f0f0" "white")
;;  (base1                   "#ebdbb2" "#ffffaf" "brightblack")
;;  (base2                   "#d5c4a1" "#d7d6af" "brightblack")
;;  (base3                   "#bdae93" "#afaf87" "brightblack")
;;  (base4                   "#a89984" "#afafaf" "brightblack")
;;  (base5                   "#504945" "#4e4e4e" "brightblack")
;;  (base6                   "#3c3836" "#3a3a3a" "brightblack")
;;  (base7                   "#282828" "#262626" "brightblack")
;;  (base8                   "#1d2021" "#1c1c1c" "black")
;;
;;  (fg                      "#282828" "#262626" "black")
;;  (fg-alt                  "#1c1c1c" "#1c1c1c" "brightblack")
;;  (grey                    "#928374" "#8a8a8a" "grey")
;;  (light3                  "#665c54" "#626262" "grey")
;;  (light4                  "#7c6f64" "#767676" "grey")
;;
;;  (burlywood4              "#BBAA97" "#aafaf87")
;;
;;  (red                     "#9d0006" "#870000" "red")
;;  (dark-red                "#421E1E" "#5f0000")
;;  (faded-red               "#cc241d" "#d75f5f" "red")
;;  (orange                  "#af3a03" "#af5f00" "brightred")
;;  (faded-orange            "#d65d0e" "#ff8700" "brightorange")
;;  (sienna                  "#dd6f48" "d7875f")
;;  (yellow                  "#b57614" "#af8700" "yellow")
;;  (faded-yellow            "#d79921" "#ffaf00" "yellow")
;;  (green                   "#79740e" "#878700" "green")
;;  (faded-green             "#98971a" "#afaf00" "green")
;;  (aquamarine4             "#83af98" "#87af87")
;;  (faded-aqua              "#689d6a" "#87af87" "brightcyan")
;;  (dark-aqua               "#36473A" "#005f5f")
;;  (turquoise4              "#61ACBB" "#5fafaf" "brightblue")
;;  (cyan                    "#427b58" "#5f8787" "brightcyan")
;;  (dark-cyan               "#36473a" "#005f5f" "cyan")
;;  (teal                    "#4db5bd" "#44b9b1" "brightgreen")
;;  (lightblue4              "#66999D" "#5fafaf" "brightblue")
;;  (blue                    "#076678" "#005f87" "brightblue")
;;  (dark-blue               "#2b3c44" "#000087" "blue")
;;  (faded-blue              "#458588" "#87afaf" "blue")
;;  (magenta                 "#b16286" "#d75f87" "magenta")
;;  (violet                  "#8f3f71" "#875f87" "brightmagenta")
;;
;;  (builtin                 "#af3a03" "#af5f00" "brightred")
;;  (comments                "#a89984" "#afafaf" "brightblack")
;;  (constants               "#8f3f71" "#875f87" "brightmagenta")
;;  (delimiter-3             "#8ec07c" "#87af87")
;;  (doc-comments            "#79740e" "#878700" "green")
;;  (error                   "#9d0006" "#870000" "red")
;;  (functions               "#b57614" "#af8700" "yellow")
;;  (highlight               "#a89984" "#afafaf" "brightblack")
;;  (keywords                "#9d0006" "#870000" "red")
;;  (methods                 "#427b58" "#5f8787" "brightcyan")
;;  (modeline-bg             "#ebdbb2" "#ffffaf" "brightblack")
;;  (modeline-bg-inactive    "#e0dcc1" "#e5e5c1" nil)
;;  (modeline-bg-inactive-l  "#eee4bd" "#ffffaf" "brightblack"))
;;  (modeline-bg-l           "#d5c4a1" "#d7d6af" "brightblack")
;;  (modeline-fg-alt         "#a38780" "#a79fa7" "brightmagenta")
;;  (numbers                 "#8f3f71" "#875f87" "brightmagenta")
;;  (operators               "#076678" "#005f87" "brightblue")
;;  (region                  "#e1d8b3" "#a8a8a8" "white")
;;  (selection               "#bdae93" "#afaf87" "brightblack")
;;  (strings                 "#79740e" "#878700" "green")
;;  (success                 "#79740e" "#878700" "green")
;;  (type                    "#8f3f71" "#875f87" "brightmagenta")
;;  (variables               "#076678" "#005f87" "brightblue")
;;  (vc-added                "#79740e" "#878700" "green")
;;  (vc-deleted              "#9d0006" "#870000" "red")
;;  (vc-modified             "#af3a03" "#af5f00" "brightred")
;;  (vertical-bar            "#d3c5a0" "#e5e59d" "brightblack")
;;  (warning                 "#af3a03" "#af5f00" "brightred")
;; }}}
