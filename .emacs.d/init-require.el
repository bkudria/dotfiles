(when
	(load
	 (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; Define this so autotest-update is defined when requireing autotest below
(setq-default autotest-use-ui t)

(require 'ido) ; Load the the Ido library
(require 'predictive)
(require 'ruby-flymake) ; load the flymake ruby lib
(require 'sml-flymake) ; load the flymake sml lib
(require 'haskell-flymake) ; load the flymake haskell lib
(require 'inf-haskell)
(require 'flymake-shell) ; load the fly-make shell lib
(require 'autofit-frame) ; Autofit frames to contents
(require 'fringe-helper)
(require 'color-theme)
(require 'highlight-parentheses)
(require 'inf-ruby)
(require 'unit-test)
(require 'autotest)
(require 'balanced)
(require 'ruby-electric)
(require 'find-recursive)
(require 'smooth-scrolling)
(require 'parenface)
(require 'color-theme-scanner-brightly)
(require 'color-theme-lite-brite)
(require 'tabkey2)
(require 'dtrt-indent)
(require 'sudo-save)
(require 'haml-mode nil 't)
(require 'sass-mode nil 't)
(require 'android-mode)
(require 'saveplace)
(require 'smex)
(require 'multi-term)
(require 'imenu)
(require 'ibuf-ext)
(require 'mouse-drag)
(require 'tramp)
(require 'sr-speedbar)
(require 'etags)
(require 'php-mode)