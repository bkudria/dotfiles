;; Delete key should delete forward
(global-set-key [delete] 'delete-char)

;; Backspace shouldn't untabify
(global-set-key [backspace] 'backward-delete-char)

;; F2 to extended-execute shortcuts
(global-set-key [f2] 'smex)

;; Reset a habit
(global-set-key "\M-x" #'(lambda () (interactive) (message "Press F2 instead!!!")))

;; F3 to switch between buffers
(global-set-key [f3] 'ido-switch-buffer)

;; C-F3 to show buffers using ibuffer
(global-set-key (kbd "S-<f3>") 'ibuffer)

;; F4 refreshes and reindents the buffer
(global-set-key [f4] 'clean-buffer-or-region)

;; F5 key reverts the buffer
(global-set-key [f5] #'(lambda () (interactive) (revert-buffer nil t) (fit-frame)))

(global-set-key "\C-w" 'kill-buffer-and-window)

(global-set-key "\M-w" 'nuke-all-buffers)

(global-set-key "\M-q" #'(lambda () (interactive) (nuke-all-buffers) (delete-frame)))

;; Rebind kill-region since we overrode it above:
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key "\C-x\C-q" 'save-buffers-kill-emacs)
(global-set-key "\C-c\C-q" 'save-buffers-kill-emacs)

;; Highlight changes in a file since last save
(global-set-key "\C-c\C-c" 'highlight-changes-mode)

;; Bind M-g to goto-line
(global-set-key "\M-g" 'goto-line)

;; Having to hit escape 3 times is silly and slow
(global-set-key [escape] 'keyboard-escape-quit)

(global-set-key "\C-f" 'isearch-forward)
(define-key isearch-mode-map [down] 'isearch-repeat-forward)
(define-key isearch-mode-map [up] 'isearch-repeat-backward)
(define-key isearch-mode-map [backspace] 'isearch-del-char)
(define-key isearch-mode-map [return] 'isearch-exit)
(define-key isearch-mode-map [escape] 'isearch-exit)
(define-key isearch-mode-map [backspace] 'isearch-del-char)
(define-key isearch-mode-map (kbd "\C-r") 'isearch-query-replace-regexp)

(global-set-key "\C-s" 'ido-save-or-write-file)
(global-set-key "\C-o" 'ido-find-file)
(define-key ido-file-completion-map (kbd "C-o") 'ido-recentf)

(global-set-key "\C-d" 'comment-or-uncomment-region)

(global-set-key "\C-q" 'delete-frame)

(global-set-key (kbd "C-<tab>") 'dabbrev-expand)

;; Bind C-c C-a to select entire buffer
(global-set-key (kbd "C-S-a") 'mark-whole-buffer)

(global-set-key "\C-c\C-e" 'flymake-goto-next-error)

(global-set-key "\C-x\C-r" 'eval-and-replace)

(global-set-key (kbd "RET") 'align-newline-and-indent)

(global-set-key (kbd "C-SPC") 'highlight-symbol-at-point)

(global-set-key (kbd "C-`") #'(lambda () (interactive) (dired (getenv "HOME"))))

;; Sane beginning-of-line behavior.
(global-set-key "\C-a" 'dwim-home)
(global-set-key [home] 'dwim-home)

;; Use ido to find tags
(global-set-key "\M-." 'ido-find-tag)

;; Jump to tag using etags-select
(global-set-key "\C-g" 'etags-select-find-tag)

(global-set-key (kbd "<f10>") 'magit-status)
(define-key magit-mode-map (kbd "\C-w") 'kill-buffer-and-window)

;; Zap-back-to-char
(global-set-key "\C-\M-z" #'(lambda (arg char) (interactive "p\ncZap to char: ") (zap-to-char (- arg) char)))

;; Rebind keys for macros
(global-set-key [f7] 'kmacro-start-macro-or-insert-counter)
(global-set-key [f8] 'kmacro-end-or-call-macro)

;; multi-term
(global-set-key (kbd "<f9> t") 'multi-term)
(global-set-key (kbd "<f9> n") 'multi-term-next)
(global-set-key (kbd "<f9> p") 'multi-term-prev)

(global-set-key [down-mouse-3] 'mouse-drag-drag)

(add-hook 'haskell-mode-hook
          '(lambda ()
             (define-key haskell-mode-map (kbd "\C-y") 'search-hayoo)))

(global-set-key "\C-x\C-d" 'ido-dired)

(define-key dired-mode-map [backspace] 'dired-up-directory)

(global-set-key [f11] 'toggle-fullscreen)