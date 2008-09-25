;; Delete key should delete forward
(global-set-key [delete] 'delete-char)

;; Remap standard Alt+X to Ctrl- shortcuts
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Ctrl+W to kill word, along with Alt+Del
(global-set-key "\C-w" 'backward-kill-word)

;; Rebind kill-region since we overrode it above:
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key "\C-x\C-q" 'save-buffers-kill-emacs)
(global-set-key "\C-c\C-q" 'save-buffers-kill-emacs)

;; Hide toolbar, menubar, and scrollbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))


(setq inhibit-startup-message t) ;; Get rid of the welcome message
(setq initial-major-mode 'text-mode) ;; Set the initial mode to text mode
(setq-default tab-width 4) ;; Set default tab width to 4
(setq-default x-stretch-cursor t) ;; Stretch the cursor on longer characters (eg. TAB)
(setq-default cursor-type 'hbar) ;; Set the cursor type to a horizontal bar
(setq-default tab-stop-list '(0 4 8 12 16 20 24 28 32)) ;; Set the tab-stop list to multiples of 4
(setq-default visible-bell 1) ;; Set a visual bell, don't beep

(ido-mode t) ;; Enable ido-mode
(icomplete-mode 99) ;; Enable icomplete mode - show completion matches
(cua-mode) ;; Load CUA mode

;; Add .emacs.d/elisp dir to load-path:
(add-to-list 'load-path "~/.emacs.d/elisp")