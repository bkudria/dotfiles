; Set the scrollbar to the right-hand side
(set-scroll-bar-mode 'right)

(setq-default cursor-type '(bar . 1)) ; Set the cursor type to a horizontal bar
(setq-default frame-title-format (list "%* %b" )) ; set the frame title to buffer title and status indicator
(setq-default x-select-enable-clipboard t) ; Enable X pasting support
(setq-default interprogram-paste-function 'x-cut-buffer-or-selection-value)
(setq-default column-number-mode t) ; Display the column in the modeline
(setq-default mouse-autoselect-window 0.5) ; select the window the pointer is in after 0.5 sec
(setq-default scalable-fonts-allowed t) ; allow scalable fonts

(display-battery-mode t) ; Display battery and temp in modeline

(add-hook 'after-make-frame-functions 'fit-frame)
(add-hook 'temp-buffer-show-hook 'fit-frame-if-one-window 'append)

(setq-default fit-frame-empty-height 40) ; Set default frame resize options
(setq-default fit-frame-empty-width 100)
(setq-default fit-frame-min-height 40)
(setq-default fit-frame-min-width 60)
(setq-default fit-frame-max-height 55)
(setq-default fit-frame-max-width 140)

(global-hl-line-mode t)

(setq-default indicate-empty-lines t)

(global-linum-mode 1)

(mouse-avoidance-mode 'animate) ; Animate the mouse away from where we are typing
