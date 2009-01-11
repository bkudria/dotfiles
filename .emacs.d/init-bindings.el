;; Delete key should delete forward
(global-set-key [delete] 'delete-char)

;; Backspace shouldn't untabify
(global-set-key [backspace] 'backward-delete-char)

;; F2 to extended-execute shortcuts
(global-set-key [f2] 'ido-execute)
;; Reset a habit
(global-set-key "\M-x" #'(lambda () (interactive) (message "Press F2 instead!!!")))

;; F3 to switch between buffers
(global-set-key [f3] 'ido-switch-buffer)

;; F4 refreshes and reindents the buffer
(global-set-key [f4] 'clean-buffer-or-region)

;; F5 key reverts the buffer
(global-set-key [f5] #'(lambda () (interactive) (revert-buffer nil t) (fit-frame)))

(global-set-key "\C-w" 'kill-buffer-and-window)

;; Rebind kill-region since we overrode it above:
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key "\C-x\C-q" 'save-buffers-kill-emacs)
(global-set-key "\C-c\C-q" 'save-buffers-kill-emacs)

;; Highlight changes in a file since last save
(global-set-key "\C-c\C-c" 'highlight-changes-mode)

;; Bind M-g to goto-line
(global-set-key "\M-g" 'goto-line)

(global-set-key "\C-f" 'isearch-forward)
(global-set-key "\C-s" 'ido-save-or-write-file)
(global-set-key "\C-o" 'ido-find-file)
(define-key ido-file-completion-map (kbd "C-o") 'ido-recentf)

(global-set-key "\C-d" 'comment-or-uncomment-region)

(global-set-key "\C-q" 'delete-frame)

(global-set-key (kbd "C-<tab>") 'dabbrev-expand)

;; Bind C-c C-a to select entire buffer
(global-set-key (kbd "C-S-a") 'mark-whole-buffer)


(global-set-key [?\e ?\M-x] 'lacarte-execute-menu-command) ; Bind ESC M-x to the LaCarte menu command

(global-set-key "\C-c\C-e" 'flymake-goto-next-error)

(global-set-key "\C-x\C-r" 'eval-and-replace)

(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-SPC") 'highlight-symbol-at-point)

(global-set-key (kbd "C-`") #'(lambda () (interactive) (dired (getenv "HOME"))))

;; Sane beginning-of-line behavior.
(global-set-key "\C-a" 'sane-beginning-of-line)
(global-set-key [home] 'sane-beginning-of-line)