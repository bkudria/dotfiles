(icomplete-mode t) ; Turn on icomplete-mode

;; (balanced-on) ; turn on balanced pns

(defun turn-on-paredit-no-errors ()
  "Turns on paredit-mode, ignoring errors"
  (ignore-errors (paredit-mode t))
  )

(autopair-global-mode t)
(setq-default autopair-autowrap t)

(ido-mode t) ; Turn on ido-mode
(setq-default ido-create-new-buffer 'always)
(setq-default ido-default-buffer-method 'raise-frame)
(setq-default ido-default-file-method 'raise-frame)
(setq-default ido-enable-flex-matching t)
(setq-default ido-everywhere t)
(setq-default ido-decorations (quote ("(" ")" " | " " | ..." "[" "]" " [None]" " [Matched]" " [Not readable]" " [Too big]")))
(setq-default ido-max-prospects 20)
(setq-default ido-max-window-height 0.4)
(setq-default ido-rotate-file-list-default t)
(setq-default ido-max-directory-size 1000000)

; Org mode
(setq-default org-completion-use-ido t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(cua-mode t) ; Load CUA mode
(auto-compression-mode 1) ; Allow opening compressed files

(define-globalized-minor-mode global-highlight-parentheses-mode highlight-parentheses-mode highlight-parentheses-mode :group 'highlight-parentheses)
(global-highlight-parentheses-mode t)

(setq-default dired-listing-switches "-phl")

(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")

(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

(add-hook 'ruby-mode-hook
		  '(lambda ()
             (inf-ruby-keys)
             ))

(add-hook 'ruby-mode-hook
		  '(lambda ()
             (ruby-electric-mode)
             ))

(add-hook 'ruby-electric-mode-hook
		  '(lambda ()
             (ruby-electric-expand-delimiters-list '(96 124))
             ))

(add-hook 'ruby-mode-hook
		  '(lambda ()
             (setq-default ruby-indent-tabs-mode t)
             (setq-default ruby-indent-level 4)
             ))

;; Rakefiles are Ruby too
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))

(whitespace-mode 1)
(global-whitespace-mode 1)
(setq-default global-whitespace-mode t)
(setq-default global-whitespace-newline-mode nil)
(setq-default whitespace-global-modes t)
(setq-default whitespace-line-column 100)
(setq-default whitespace-style
			  '(tabs spaces trailing space-before-tab indentation empty space-mark tab-mark))

;; make dired slightly smarter
(setq-default directory-free-space-program "di")
(setq-default directory-free-space-args "-Ph")

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

(recentf-mode t)
(setq-default recentf-max-saved-items 1000)

;; Don't try to tell me about new mail
(setq-default display-time-mail-file nil)
(setq-default display-time-mail-string "")
(setq-default display-time-mail-function nil)

(add-hook 'anything-before-initialize-hoo
		  '(lambda ()
             (setq fit-frame-inhibit-fitting-flag t)))

;; Guess indent mode for newly-opened files
(dtrt-indent-mode 1)

(setq-default dtrt-indent-max-merge-deviation 20.0)
(setq-default dtrt-indent-max-offset 16)
(setq-default dtrt-indent-max-relevant-lines 2000)
(setq-default dtrt-indent-min-hard-tab-superiority 200.0)
(setq-default dtrt-indent-min-indent-superiority 80.0)
(setq-default dtrt-indent-min-offset 2)
(setq-default dtrt-indent-min-quality 20.0)
(setq-default dtrt-indent-min-soft-tab-superiority 200.0)
(setq-default dtrt-indent-require-confirmation-flag nil)
(setq-default dtrt-indent-verbosity 1)

;; Haml and Sass modes
(add-to-list 'auto-mode-alist '("\\.haml$"      . haml-mode))
(add-to-list 'auto-mode-alist '("\\.haml.html$" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.sass$"      . sass-mode))

(add-hook 'haml-mode-hook
		  '(lambda ()
             (setq indent-tabs-mode t)
			 (haml-backspace-backdents-nesting t)
			 (haml-indent-offset 1)))


;; buffer menu
(setq-default Buffer-menu-use-header-line nil)

;; completion
(setq-default completion-auto-show (quote tooltip))
(setq-default completion-auto-show-delay 1)
(setq-default completion-overwrite nil)
(setq-default completion-resolve-behaviour (quote leave))

(setq-default predictive-auto-complete t)

;; erc
(setq-default erc-email-userid "bkudria")
(setq-default erc-modules
			  '(autoaway autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom services stamp spelling track truncate))
(setq-default erc-nick "bkudria")
(setq-default erc-nick-uniquifier "_")
(setq-default erc-nickserv-passwords '((freenode (("bkudria" . "11z12=op")))))
(setq-default erc-port 6667)
(setq-default erc-prompt-for-password nil)
(setq-default erc-public-away-p t)
(setq-default erc-server "irc.freenode.net")
(setq-default erc-user-full-name "Benjamin Kudria")

;; ido
(setq-default ido-use-filename-at-point '(guess))
(setq-default ido-use-url-at-point t)

;; define the android SDK dir
(setq-default android-mode-sdk-dir "/usr/local/android")




;; also sace search terms
(setq-default savehist-additional-variables '(search-ring regexp-search-ring))

;; Set savehist location
(setq-default savehist-file "~/.emacs.d/savehist")

;; Turn on savehist
(savehist-mode t)

;; Save place in files between sessions
(setq-default save-place t)

;; Display buffer size in modeline
(size-indication-mode t)

;; Make scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Smex
(smex-initialize)
(smex-auto-update 60)
(setq-default smex-history-length 20)
(setq-default smex-prompt-string ": ")
(setq-default smex-save-file "~/.emacs.d/smex.save")


;; iBuffer
(add-to-list 'ibuffer-never-show-predicates "^\\*")

(add-hook 'php-mode-hook
		  '(lambda ()
             (flymake-mode t)
             ))

(add-hook 'php-mode-hook
		  '(lambda ()
             (c-set-style "php")
             ))


;; TRAMP
(setq-default tramp-shell-prompt-pattern "^- $")
(setq-default tramp-verbose 10)

;; Flymake Haskell

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


(add-hook 'haskell-mode-hook
		  '(lambda ()
             (tabkey2-mode -1)
             ))

(setq-default haskell-program-name "ghci")
(setq-default hs-lint-replace-with-suggestions t)

(c-add-style "php" '((c-basic-offset . 4)
                     (c-comment-only-line-offset 0 . 0)
                     (c-offsets-alist
                     (inline-open . 0)
                     (topmost-intro-cont . +)
                     (statement-block-intro . +)
                     (knr-argdecl-intro . 5)
                     (substatement-open . +)
                     (substatement-label . +)
                     (label . +)
                     (statement-case-open . +)
                     (statement-cont . +)
                     (arglist-intro . c-lineup-arglist-intro-after-paren)
                     (arglist-close . c-lineup-arglist)
                     (access-label . 0)
                     (inher-cont . c-lineup-java-inher)
                     (func-decl-cont . c-lineup-java-throws))))


(defun flymake-erlang-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name temp-file
                                         (file-name-directory buffer-file-name))))
    (list "~/bin/flymake-erlang.erl" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))


(add-hook 'erlang-mode-hook
		  '(lambda ()
             (flymake-mode t)
             ))

(global-smart-tab-mode t)

(add-hook 'global-smart-tab-mode-hook
		  '(lambda ()
             (smart-tabs-advice ruby-indent-line ruby-indent-level)
			 (smart-tab-using-hippie-expand t)
             ))

(setq ruby-indent-tabs-mode t)

(setq rinari-tags-file-name "TAGS")

