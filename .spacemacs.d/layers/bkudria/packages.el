(defconst
  bkudria-packages
  '(
    evil-extra-operator
    (evil-textobj-line :location (recipe :fetcher github :repo "syohex/evil-textobj-line"))
    evil-replace-with-register
    highlight-indent-guides
    magithub
    processing-mode
    ruby-refactor
    evil-goggles
    (case :location (recipe :fetcher url
                           :url "https://raw.githubusercontent.com/chrisbarrett/spacemacs-layers/master/cb-core/local/case/case.el"))))

(defun bkudria/init-case ()
  (use-package case))

(defun bkudria/init-ruby-refactor ()
  (use-package ruby-refactor))

(defun bkudria/init-processing-mode ()
  (use-package processing-mode :defer t
    :config (spacemacs/set-leader-keys-for-major-mode 'processing-mode
              "r" 'processing-sketch-run
              "b" 'processing-sketch-build)))

(defun bkudria/init-evil-goggles ()
  (use-package evil-goggles)
  :init
  (evil-goggles-mode))

(defun bkudria/init-evil-extra-operator ()
  (use-package evil-extra-operator :defer t
    :init
    (define-key evil-motion-state-map "gl" 'evil-operator-clone)))

(defun bkudria/init-evil-replace-with-register ()
  (use-package evil-extra-operator :defer t
    :init
    (define-key evil-motion-state-map "gr" 'evil-replace-with-register)))

(defun bkudria/init-evil-textobj-line ()
  (use-package evil-textobj-line))

(defun bkudria/init-highlight-indent-guides ()
  (use-package highlight-indent-guides)
  :init
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\u2502)
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (set-face-foreground 'highlight-indent-guides-character-face "#504945")
  )

(defun bkudria/init-magithub ()
  (use-package magithub
    :after magit
    :config (magithub-feature-autoinject t)))

(spacemacs|use-package-add-hook move-text
  :post-config
  (global-set-key [s-down] 'sticky-move-down)
  (global-set-key [s-up] 'sticky-move-up))

(spacemacs|use-package-add-hook web-mode
  :post-config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(spacemacs|use-package-add-hook css-mode
  :post-config
  (setq css-indent-offset 2))
