;; Hide toolbar and menubar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(show-paren-mode 1) ; Turn on matching of parens
(setq-default show-paren-delay 0.1) ; Highlight matching parens quickly
(setq-default show-paren-ring-bell-on-mismatch t) ; Bell on mismatched parens
(setq-default show-paren-style 'mixed) ; Set paren-matching mode to mixed

(setq inhibit-startup-message t) ; Get rid of the welcome message
(setq initial-major-mode 'text-mode) ; Set the initial mode to text mode
(setq major-mode 'text-mode) ; Set the default mode to text mode
(setq-default tab-width 2) ; Set default tab width to 2
(setq-default standard-indent 4) ; Set the indent-size to 4
(setq-default tab-stop-list '(0 4 8 12 16 20 24 28 32)) ; Set the tab-stop list to multiples of 4
(setq-default visible-bell 1) ; Set a visual bell, don't beep
(setq-default mouse-wheel-scroll-amount '(1)) ; Mouse wheel scrolls 1 line
(setq-default idle-update-delay 0.1)
(setq-default inhibit-startup-echo-area-message "")
(setq-default initial-buffer-choice t)
(setq-default initial-scratch-message "")
(setq-default remote-shell-program "ssh") ; use ssh, not rsh
(setq-default make-backup-files nil) ; Don't make those pesky backup files
(setq-default sentence-end-double-space nil) ; Sentences end with one space

(put 'narrow-to-region 'disabled nil)
(setq enable-recursive-minibuffers t)

;; don't scroll funny
(setq-default mouse-wheel-progressive-speed nil)
(setq-default mouse-wheel-scroll-amount '(1))

;; Breaks dtrt-indent if t
(setq-default indent-tabs-mode nil)

(color-theme-lite-brite)

(defalias 'yes-or-no-p 'y-or-n-p)
