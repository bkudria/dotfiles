(color-theme-lite-brite)

(icomplete-mode t) ; Turn on icomplete-mode

(ido-mode t) ; Turn on ido-mode
(setq-default ido-create-new-buffer 'always)
(setq-default ido-default-buffer-method 'raise-frame)
(setq-default ido-default-file-method 'raise-frame)
(setq-default ido-enable-flex-matching t)
(setq-default ido-everywhere t)

(predictive-mode t)
(setq-default completion-auto-show 'menu)
(setq-default predictive-auto-add-to-dict t)
(setq-default predictive-auto-learn t)
(setq-default predictive-add-to-dict-ask nil)

(cua-mode) ; Load CUA mode
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

(setq-default ruby-indent-tabs-mode t)
(setq-default ruby-indent-level 4)

(global-whitespace-mode 1)
(setq-default global-whitespace-mode t)
(setq-default global-whitespace-newline-mode nil)
(setq-default whitespace-global-modes t)
(setq-default whitespace-line-column 100)
(setq-default whitespace-style
			  '(tabs spaces trailing lines-tail space-before-tab indentation empty space-after-tab space-mark tab-mark))