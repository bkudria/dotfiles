(setq bkudria-packages '(powerline-evil
                         evil-extra-operator))

(defun bkudria/init-evil-extra-operator ()
  (use-package evil-extra-operator
    :init
    (define-key evil-motion-state-map "gl" 'evil-operator-clone)
    )
  )
