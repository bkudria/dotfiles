(when
	(load
	 (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(require 'dired-details)
(require 'buff-menu+) ; Load enhanced buffer menu lib
(require 'dired+)
(require 'ido) ; Load the the Ido library
(require 'predictive)
(require 'lacarte) ; Load the LaCarte menu access library
(require 'ruby-flymake) ; load the flymake ruby lib
(require 'sml-flymake) ; load the flymake sml lib
(require 'haskell-flymake) ; load the flymake haskell lib
(require 'flymake-shell) ; load the fly-make shell lib
(require 'autofit-frame) ; Autofit frames to contents
(require 'fringe-helper)
(require 'color-theme)
(require 'highlight-parentheses)
(require 'inf-ruby)
(require 'balanced)
(require 'ruby-electric)
(require 'find-recursive)
(require 'smooth-scrolling)
(require 'parenface)
(require 'color-theme-lite-brite)
(require 'tabkey2)