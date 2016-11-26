(defconst bkudria-packages '(evil-extra-operator))

(defun bkudria/init-evil-extra-operator ()
  (use-package evil-extra-opeator
    :init
    (define-key evil-motion-state-map "gl" 'evil-operator-clone)))

(spacemacs|use-package-add-hook gruvbox-theme
  :post-config
  ((custom-theme-set-faces
    'gruvbox
    '(font-lock-comment-face ((t (:slant oblique))))
    '(font-lock-string-face ((t (:slant oblique))))
  )))
