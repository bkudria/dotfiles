(setq custom-file "~/.emacs.d/customize") ; Change the customize-file location
(load custom-file) ; Be sure to load the customize-file

;; Add . emacs.d/ dir to load-path:
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized/")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")


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
    (( init-files
       '(
         "init-require"
         "init-elpa"
         "init-el-get"
         "init-x-windows"
         "init-console"
         "init-common"
         "init-modes"
         "init-functions"
         "init-bindings"
         )))
  (mapc 'load-library init-files))

;; Server mode
(server-mode t)
