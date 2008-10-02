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

(light-symbol-mode t)
(highlight-parentheses-mode t)
(highlight-symbol-mode t)

(setq-default dired-listing-switches "-phl")