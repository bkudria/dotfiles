(require 'cl)

(setq custom-file "~/.emacs.d/customize") ; Change the customize-file location
(load custom-file) ; Be sure to load the customize-file
;; Add . emacs.d/ dir to load-path:
(add-to-list 'load-path "~/.emacs.d/")

(defun autocompile nil
  "compile itself if ~/.emacs or ~/.emacs.d/init-*"
  (interactive)
  (require 'bytecomp)
  (flet
	  ((default-file (file)
		 (expand-file-name (concat default-directory file))))
	(if (or
		 (string= (buffer-file-name) (default-file ".emacs"))
		 (when (buffer-file-name)
		   (string-match
			"\\.emacs\\.d/init-.*\\.el"
			(buffer-file-name))))
		(byte-compile-file (buffer-file-name)))))

(add-hook 'after-save-hook 'autocompile)

(let
	(( init-files '("init-require"
					"init-common"
					"init-modes"
					"init-functions"
					"init-projects"
					"init-bindings"
					"init-x-windows"
					"init-console"
					)))
  (mapc 'load-library init-files))


(server-mode t) ; Enable server mode


(put 'set-goal-column 'disabled nil)


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
