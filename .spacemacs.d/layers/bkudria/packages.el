(defconst
  bkudria-packages
  '(
    evil-extra-operator
    (evil-textobj-line :location (recipe :fetcher github :repo "syohex/evil-textobj-line"))
    ))

(defun bkudria/init-evil-extra-operator ()
  (use-package evil-extra-operator :defer t
    :init
    (define-key evil-motion-state-map "gl" 'evil-operator-clone)))

(defun bkudria/init-evil-textobj-line ()
  (use-package evil-textobj-line))
