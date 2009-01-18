(color-theme-scanner-brightly)

(icomplete-mode t) ; Turn on icomplete-mode

;; (balanced-on) ; turn on balanced pns

(defun turn-on-paredit-no-errors ()
  "Turns on paredit-mode, ignoring errors"
  (ignore-errors (paredit-mode t))
  )

(define-globalized-minor-mode global-paredit-mode paredit-mode turn-on-paredit-no-errors)

(global-paredit-mode t)

(ido-mode t) ; Turn on ido-mode
(setq-default ido-create-new-buffer 'always)
(setq-default ido-default-buffer-method 'raise-frame)
(setq-default ido-default-file-method 'raise-frame)
(setq-default ido-enable-flex-matching t)
(setq-default ido-everywhere t)

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
			  '(tabs spaces trailing space-before-tab indentation empty space-after-tab space-mark tab-mark))

;; do some spooky shit to make the time display
(display-time-mode t)
(setq-default display-time-default-load-average 2)
(setq-default  display-time-mail-file 'none)
(setq-default  display-time-string-forms (quote ((if (and (not display-time-format) display-time-day-and-date) (format-time-string " %a %b %e " now) "") (propertize (format-time-string (or display-time-format (if display-time-24hr-format " %H:%M" "%-I:%M%p")) now) (quote help-echo) (format-time-string " %a %b %e, %Y" now)) load (if mail (concat " " (propertize display-time-mail-string (quote display) (\` (when (and display-time-use-mail-icon (display-graphic-p)) (\,@ display-time-mail-icon) (\,@ (if (and display-time-mail-face (memq (plist-get (cdr display-time-mail-icon) :type) (quote (pbm xbm)))) (let ((bg (face-attribute display-time-mail-face :background))) (if (stringp bg) (list :background bg))))))) (quote face) display-time-mail-face (quote help-echo) "You have new mail; mouse-2: Read mail" (quote mouse-face) (quote mode-line-highlight) (quote local-map) (make-mode-line-mouse-map (quote mouse-2) read-mail-command))) ""))))

										; make dired slightly smarter
(setq-default directory-free-space-program "di")
(setq-default directory-free-space-args "-Ph")

(tabkey2-mode t)
(setq-default tabkey2-completion-functions
			  '(("Yasnippet" yas/expand)
				("Semantic Smart Completion" senator-complete-symbol senator-minor-mode)
				("Programmable completion" pcomplete)
				("nXML completion" nxml-complete)
				("Hippe Expansion" hippie-expand (commandp 'hippie-expand))
				("Complete Emacs symbol" lisp-complete-symbol)
				("Predictive word" complete-word-at-point predictive-mode)
				("Predictive abbreviations" pabbrev-expand-maybe)
				("Dynamic word expansion" dabbrev-expand nil (setq dabbrev--last-abbrev-location nil))
				("Anything" anything (commandp 'anything))
				("Ispell complete word" ispell-complete-word)
				("Spell check word" flyspell-correct-word-before-point (commandp 'flyspell-correct-word-before-point))))

(recentf-mode t)
(setq-default recentf-max-saved-items 1000)

;; Don't try to tell me about new mail
(setq-default display-time-mail-file nil)
(setq-default display-time-mail-string "")
(setq-default display-time-mail-function nil)

;; Don't open tons o' buffers
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

(add-hook 'anything-before-initialize-hoo
		  '(lambda ()
			 (setq fit-frame-inhibit-fitting-flag t)))

;; Turn on SLIME, set default lisp
(slime-setup)
(setq inferior-lisp-program "sbcl")

;; Clojure + Swank
(setq-default swank-clojure-binary "/home/bkudria/archive/installs/clojure-extra/sh-script/clojure")

;; Guess indent mode for newly-opened files
(dtrt-indent-mode 1)

;; Haml and Sass modes
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.haml.html$" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))


;; buffer menu
(setq-default Buffer-menu-use-header-line nil)

;; completion
(setq-default completion-auto-show (quote tooltip))
(setq-default completion-auto-show-delay 1)
(setq-default completion-overwrite nil)
(setq-default completion-resolve-behaviour (quote leave))

(setq-default predictive-auto-complete t)

;; tabkey2
(setq-default tabkey2-completion-lighter-on t)
(setq-default tabkey2-mode t)

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
