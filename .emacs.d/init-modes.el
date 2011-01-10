;; Modes
(cua-mode t)
(savehist-mode t)
(icomplete-mode t)
(smooth-scroll-mode t)
(size-indication-mode t)
(autopair-global-mode t)
(auto-compression-mode 1)

;; Variables
(setq-default save-place t)
(setq-default autopair-autowrap t)
;; Don't try to tell me about new mail
(setq-default display-time-mail-file nil)
(setq-default display-time-mail-string "")
(setq-default display-time-mail-function nil)


;; Hooks
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)



;; Highlight Parentheses
(define-globalized-minor-mode global-highlight-parentheses-mode highlight-parentheses-mode highlight-parentheses-mode :group 'highlight-parentheses)
(global-highlight-parentheses-mode t)



;; IDO Mode
(ido-mode t)
(setq-default ido-create-new-buffer 'always)
(setq-default ido-default-buffer-method 'raise-frame)
(setq-default ido-default-file-method 'raise-frame)
(setq-default ido-enable-flex-matching t)
(setq-default ido-everywhere t)
(setq-default ido-decorations (quote ("(" ")" " | " " | ..." "[" "]" " [None]" " [Matched]" " [Not readable]" " [Too big]")))
(setq-default ido-max-prospects 20)
(setq-default ido-rotate-file-list-default t)
(setq-default ido-max-directory-size 1000000)
(setq-default ido-use-filename-at-point nil)
(setq-default ido-use-url-at-point nil)



;; Org mode
(setq-default org-completion-use-ido t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))



;; Ruby Mode
(add-hook 'ruby-mode-hook
          '(lambda ()
             (ruby-electric-mode)
             ))

(add-hook 'ruby-electric-mode-hook
          '(lambda ()
             (setq-default ruby-electric-expand-delimiters-list '(96 124))
             ))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq-default ruby-indent-tabs-mode t)
             (setq-default ruby-indent-level 2)
             ))

;; Rakefiles are Ruby too
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$"  . ruby-mode))



;; PHP Mode
(add-hook 'php-mode-hook
          '(lambda ()
             (flymake-mode t)
             ))

(add-hook 'php-mode-hook
          '(lambda ()
             (c-set-style "php")
             ))

(c-add-style "php" '((c-basic-offset               . 4)
                     (c-comment-only-line-offset 0 . 0)
                     (c-offsets-alist
                      (inline-open                 . 0)
                      (topmost-intro-cont          . +)
                      (statement-block-intro       . +)
                      (knr-argdecl-intro           . 5)
                      (substatement-open           . +)
                      (substatement-label          . +)
                      (label                       . +)
                      (statement-case-open         . +)
                      (statement-cont              . +)
                      (arglist-intro               . c-lineup-arglist-intro-after-paren)
                      (arglist-close               . c-lineup-arglist)
                      (access-label                . 0)
                      (inher-cont                  . c-lineup-java-inher)
                      (func-decl-cont              . c-lineup-java-throws))))


;; Whitespace
(whitespace-mode 1)
(global-whitespace-mode 1)
(setq-default global-whitespace-mode t)
(setq-default global-whitespace-newline-mode nil)
(setq-default whitespace-global-modes t)
(setq-default whitespace-line-column 100)
(setq-default whitespace-style
              '(tabs spaces trailing space-before-tab indentation empty space-mark tab-mark))



;; Dired
(setq-default directory-free-space-program "di")
(setq-default directory-free-space-args "-Ph")
(setq-default dired-listing-switches "-phl")

(eval-after-load "dired"
  ;; don't remove `other-window', the caller expects it to be there
  '(defun dired-up-directory (&optional other-window)
     "Run Dired on parent directory of current directory."
     (interactive "P")
     (let* ((dir (dired-current-directory))
            (orig (current-buffer))
            (up (file-name-directory (directory-file-name dir))))
       (or (dired-goto-file (directory-file-name dir))
           ;; Only try dired-goto-subdir if buffer has more than one dir.
           (and (cdr dired-subdir-alist)
                (dired-goto-subdir up))
           (progn
             (kill-buffer orig)
             (dired up)
             (dired-goto-file dir))))))



;; dtrt-indent
(dtrt-indent-mode 1)
(setq-default dtrt-indent-max-merge-deviation 20)
(setq-default dtrt-indent-max-offset 16)
(setq-default dtrt-indent-max-relevant-lines 2000)
(setq-default dtrt-indent-min-hard-tab-superiority 200)
(setq-default dtrt-indent-min-indent-superiority 80)
(setq-default dtrt-indent-min-offset 2)
(setq-default dtrt-indent-min-quality 20)
(setq-default dtrt-indent-min-soft-tab-superiority 200)
(setq-default dtrt-indent-require-confirmation-flag nil)
(setq-default dtrt-indent-verbosity 1)



;; ;; Haml and Sass modes
;; (add-to-list 'auto-mode-alist '("\\.haml$"      . haml-mode))
;; (add-to-list 'auto-mode-alist '("\\.haml.html$" . haml-mode))
;; (add-to-list 'auto-mode-alist '("\\.sass$"      . sass-mode))

(add-hook 'haml-mode-hook
          '(lambda ()
             (haml-backspace-backdents-nesting t)
             (haml-indent-offset 2)))



;; Completion
(setq-default completion-auto-show (quote tooltip))
(setq-default completion-auto-show-delay 1)
(setq-default completion-overwrite nil)
(setq-default completion-resolve-behaviour (quote leave))
(setq-default predictive-auto-complete t)



;; Smex
(smex-auto-update 60)
(setq-default smex-history-length 20)
(setq-default smex-prompt-string ": ")
(setq-default smex-save-file "~/.emacs.d/smex.save")



;; TRAMP
(setq-default tramp-shell-prompt-pattern "^- $")
(setq-default tramp-verbose 10)



;; Haskell
(eval-after-load "flymake"
  '(progn
     (defun flymake-hslint-init ()
       (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                            'flymake-create-temp-inplace))
              (local-file  (file-relative-name
                            temp-file
                            (file-name-directory buffer-file-name))))
         (list "hslint" (list local-file))))

     (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.l?hs\\'" flymake-hslint-init))))

(setq-default haskell-program-name "ghci")
(setq-default hs-lint-replace-with-suggestions t)



;; Smart Tab Mode
(global-smart-tab-mode t)
(add-hook 'global-smart-tab-mode-hook
          '(lambda ()
             (smart-tabs-advice ruby-indent-line ruby-indent-level)
             (smart-tab-using-hippie-expand t)
             ))
