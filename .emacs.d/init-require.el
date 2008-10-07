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
(require 'flymake-shell) ; load the fly-make shell lib
(require 'autofit-frame) ; Autofit frames to contents
(require 'fringe-helper)
(require 'light-symbol-autoloads)
(require 'color-theme)
(require 'inf-ruby)
(require 'ruby-electric)
(require 'find-recursive)