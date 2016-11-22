(deftheme gruvbox
  "Created 2016-11-21.")

(custom-theme-set-variables
 'gruvbox
 '(ansi-color-names-vector ["#3c3836" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"]))

(custom-theme-set-faces
 'gruvbox
 '(cursor ((t (:background "#fdf4c1"))))
 '(mode-line ((t (:box nil :background "#504945" :foreground "#d5c4a1"))))
 '(mode-line-inactive ((t (:box nil :background "#3c3836" :foreground "#a89984"))))
 '(fringe ((t (:background "#282828"))))
 '(linum ((t (:background "#282828" :foreground "#7c6f64"))))
 '(hl-line ((t (:background "#3c3836"))))
 '(region ((t (:background "#504945"))))
 '(secondary-selection ((t (:background "#3c3836"))))
 '(minibuffer-prompt ((t (:background "#282828" :foreground "#b8bb26" :bold t))))
 '(vertical-border ((t (:foreground "#504945"))))
 '(link ((t (:foreground "#076678" :underline t))))
 '(shadow ((t (:foreground "#7c6f64"))))
 '(font-lock-builtin-face ((t (:foreground "#fe8019"))))
 '(font-lock-constant-face ((t (:foreground "#d3869b"))))
 '(font-lock-comment-face ((t (:foreground "#7c6f64" :slant oblique))))
 '(font-lock-function-name-face ((t (:foreground "#fabd2f"))))
 '(font-lock-keyword-face ((t (:foreground "#fb4934"))))
 '(font-lock-string-face ((t (:foreground "#b8bb26" :slant oblique))))
 '(font-lock-variable-name-face ((t (:foreground "#83a598"))))
 '(font-lock-type-face ((t (:foreground "#d3869b"))))
 '(font-lock-warning-face ((t (:foreground "#fb4934" :bold t))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#458588"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#b16286"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#8ec07c"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#d65d0e"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#458588"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#b16286"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#8ec07c"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#d65d0e"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#458588"))))
 '(rainbow-delimiters-unmatched-face ((t (:background nil :foreground "#fdf4c1"))))
 '(sp-pair-overlay-face ((t (:background "#504945"))))
 '(sp-show-pair-match-face ((t (:background "#504945"))))
 '(sp-show-pair-mismatch-face ((t (:background "#fb4934"))))
 '(diff-changed ((t (:background nil :foreground "#ebdbb2"))))
 '(diff-added ((t (:background nil :foreground "#b8bb26"))))
 '(diff-removed ((t (:background nil :foreground "#fb4934"))))
 '(diff-indicator-changed ((t (:inherit diff-changed))))
 '(diff-indicator-added ((t (:inherit diff-added))))
 '(diff-indicator-removed ((t (:inherit diff-removed))))
 '(company-scrollbar-bg ((t (:background "#3c3836"))))
 '(company-scrollbar-fg ((t (:background "#32302f"))))
 '(company-tooltip ((t (:background "#32302f"))))
 '(company-tooltip-annotation ((t (:foreground "#b8bb26"))))
 '(company-tooltip-selection ((t (:foreground "#d3869b"))))
 '(company-tooltip-common ((t (:foreground "#83a598" :underline t))))
 '(company-tooltip-common-selection ((t (:foreground "#83a598" :underline t))))
 '(company-preview-common ((t (:foreground "#d3869b"))))
 '(message-header-to ((t (:inherit font-lock-variable-name-face))))
 '(message-header-cc ((t (:inherit font-lock-variable-name-face))))
 '(message-header-subject ((t (:foreground "#fe8019" :weight bold))))
 '(message-header-newsgroups ((t (:foreground "#fabd2f" :weight bold))))
 '(message-header-other ((t (:inherit font-lock-variable-name-face))))
 '(message-header-name ((t (:inherit font-lock-keyword-face))))
 '(message-header-xheader ((t (:foreground "#076678"))))
 '(message-separator ((t (:inherit font-lock-comment-face))))
 '(message-cited-text ((t (:inherit font-lock-comment-face))))
 '(message-mml ((t (:foreground "#79740e" :weight bold))))
 '(isearch ((t (:foreground "#000000" :background "#fe8019"))))
 '(lazy-highlight ((t (:foreground "#000000" :background "#fabd2f"))))
 '(isearch-fail ((t (:foreground "#fdf4c1" :background "#fb4933"))))
 '(anzu-mode-line ((t (:foreground "#fabd2f" :weight bold))))
 '(anzu-match-1 ((t (:background "#b8bb26"))))
 '(anzu-match-2 ((t (:background "#b57614"))))
 '(anzu-match-3 ((t (:background "#83A598"))))
 '(anzu-replace-to ((t (:foreground "#fabd2f"))))
 '(anzu-replace-highlight ((t (:inherit isearch))))
 '(show-paren-match ((t (:background "#665c54" :weight bold))))
 '(show-paren-mismatch ((t (:background "#fb4933" :foreground "#665c54" :weight bold))))
 '(default ((t (:background "#282828" :foreground "#fdf4c1")))))

(provide-theme 'gruvbox)
