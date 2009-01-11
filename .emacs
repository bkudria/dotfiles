(require 'cl)

(setq custom-file "~/.emacs.d/customize") ; Change the customize-file location
(load custom-file) ; Be sure to load the customize-file
										; Add .emacs.d/ dir to load-path:
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
					"init-bindings"
					"init-x-windows"
					"init-console") ))
  (mapc 'load-library init-files))

;; Set server host name to hostname so that non-local connections are accepted
(setq-default server-host (substring (shell-command-to-string "hostname") 0 -1))

;; Make sure we use TCP sockets
(setq-default server-use-tcp t)

;; Start server mode
(server-mode t)

