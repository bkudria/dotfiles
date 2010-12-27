;; Disable the scroll bar
(scroll-bar-mode -1)

(setq-default cursor-type '(bar . 2)) ; Set the cursor type to a vertical bar
(setq-default frame-title-format (list "%* %b" )) ; set the frame title to buffer title and status indicator
(setq-default x-select-enable-clipboard t) ; Enable X pasting support
(setq-default interprogram-paste-function 'x-cut-buffer-or-selection-value)
(setq-default column-number-mode t) ; Display the column in the modeline
(setq-default mouse-autoselect-window 0.5) ; select the window the pointer is in after 0.5 sec
(setq-default scalable-fonts-allowed t) ; allow scalable fonts

(add-hook 'after-make-frame-functions 'fit-frame)
(add-hook 'temp-buffer-show-hook 'fit-frame-if-one-window 'append)

(setq-default fit-frame-empty-height 80) ; Set default frame resize options
(setq-default fit-frame-empty-width 85)
(setq-default fit-frame-min-height 40)
(setq-default fit-frame-min-width 105)
(setq-default fit-frame-max-height 170)
(setq-default fit-frame-max-width 130)

(global-hl-line-mode t)

;; Disable empty-line indicator
(setq-default indicate-empty-lines nil)

(add-hook 'after-make-frame-functions 'color-theme-scanner-brightly)

(setq font-use-system-font t)

