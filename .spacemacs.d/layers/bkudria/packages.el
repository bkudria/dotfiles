(push "~/.spacemacs.d/layers/bkudria/" load-path)
(require 'iterm)

(defconst
  bkudria-packages
  '(
    evil-extra-operator
    evil-replace-with-register
    evil-ruby-text-objects
    processing-mode
    all-the-icons-ivy
    all-the-icons-dired
    evil-embrace
    ivy-posframe
    keyfreq
    ))

(defun bkudria/init-processing-mode ()
  (use-package processing-mode :defer t
    :config (spacemacs/set-leader-keys-for-major-mode 'processing-mode
              "r" 'processing-sketch-run
              "b" 'processing-sketch-build)))

(defun bkudria/init-evil-extra-operator ()
  (use-package evil-extra-operator :defer t
    :init
    (define-key evil-motion-state-map "gl" 'evil-operator-clone)))

(defun bkudria/init-evil-replace-with-register ()
  (use-package evil-replace-with-register :defer t
    :init
    (define-key evil-motion-state-map "gr" 'evil-replace-with-register)))

(defun bkudria/init-evil-ruby-text-objects ()
  (use-package evil-ruby-text-objects))

(defun bkudria/init-all-the-icons-ivy ()
  (use-package all-the-icons-ivy
    :init
    :ensure t
    :config
    (setq all-the-icons-ivy-file-commands
          '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir projectile-recentf))
    (all-the-icons-ivy-setup)))

(defun bkudria/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :init
    :ensure t))

(defun bkudria/init-evil-embrace ()
  (use-package evil-embrace
    :init
    :ensure t
    :config
    (setq evil-embrace-show-help-p nil)
    (evil-embrace-enable-evil-surround-integration)))

(defun bkudria/init-ivy-posframe ()
  (use-package ivy-posframe
    :init
    :ensure t
    :config
    (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center))))
  (ivy-posframe-mode 1)
 )

(defun bkudria/init-keyfreq ()
  (use-package keyfreq)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  )

(spacemacs|use-package-add-hook dired-mode
  :post-config
  (all-the-icons-dired-mode))

(spacemacs|use-package-add-hook ruby-mode
  :post-config
  (evil-ruby-text-objects-mode))

(spacemacs|use-package-add-hook move-text
  :post-config
  (global-set-key [s-down] 'sticky-move-down)
  (global-set-key [s-up] 'sticky-move-up))

(spacemacs|use-package-add-hook web-mode
  :post-config
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2))

(spacemacs|use-package-add-hook js2-mode
  :post-config
  (setq-default js-indent-level 2))

(spacemacs|use-package-add-hook css-mode
  :post-config
  (setq-default css-indent-offset 2))

(spacemacs|use-package-add-hook scss-mode
  :post-config
  (setq-default css-indent-offset 2))
