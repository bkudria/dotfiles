;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;;
(defun load-better-gruvbox ()
  "loads custom faces"
  (load! "better-gruvbox.el")
  )

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Benjamin Kudria"
      user-mail-address "ben@kudria.net")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 28 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 28))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox
      doom-gruvbox-dark-variant "medium")

(load-better-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq confirm-kill-emacs 'nil)
(setq initial-frame-alist '((fullscreen . maximized)))
(setq server-client-instructions 'nil)
(setq-default fill-column 140)

(setq scroll-preserve-screen-position t
      scroll-conservatively 0
      maximum-scroll-margin 0.5
      scroll-margin 99999)

(map!
 :leader
 :desc "directory" "-"     #'dired-jump
 :desc "other"     "<tab>" #'evil-switch-to-windows-last-buffer
 )


(global-subword-mode)


(use-package! rainbow-identifiers :hook prog-mode)

(use-package! dimmer
  :config (dimmer-mode))


(use-package evil-extra-operator
  :bind
  (:map evil-normal-state-map
   ("gl" . evil-operator-clone)
   :map evil-visual-state-map
   ("gl" . evil-operator-clone)
   ))

(use-package! evil-surround
  :config (global-evil-surround-mode 1))

(use-package! evil-matchit
  :config (global-evil-matchit-mode 1))

(use-package evil-replace-with-register
  :bind
  (:map evil-normal-state-map
   ("gr" . evil-replace-with-register)
   :map evil-visual-state-map
   ("gr" . evil-replace-with-register)))


(use-package emacs-everywhere
  :bind
  (:map emacs-everywhere-mode-map
        ("C-c C-c" . (lambda ()(interactive) (setq emacs-everywhere--contents nil) (emacs-everywhere-finish))))
  :config
  (remove-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-insert-selection)
  (remove-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-set-frame-position)
  (setq emacs-everywhere-markdown-windows '(".*"))
  (setq emacs-everywhere-markdown-apps '(".*"))
  (setq emacs-everywhere-frame-parameters
        `((name . "emacs-everywhere")
          (undecorated-round . t)
          (user-position . t)
          (width . 100)
          (height . 15)
          (left . 0.25)
          (top . 0.4)
          )
        ))


(use-package! doom-modeline
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  (setq doom-modeline-buffer-encoding t)
  )

(use-package! lsp-mode
  :config
  (setq lsp-disabled-clients '(semgrep-ls rubocop-ls)))

(after! magit
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

(use-package! aidermacs
  :config
  (setq aidermacs-backend 'vterm))

;;   (setq
;;    ;; Edit settings
;;    ;; org-auto-align-tags nil
;;    org-tags-column 0

;;    ;; Org styling, hide markup etc.
;;    org-hide-emphasis-markers t
;;    org-pretty-entities t
;;    org-ellipsis "…"

;;    ;; Agenda styling
;;    org-agenda-tags-column 0
;;    org-agenda-block-separator ?─)

;;   :hook
;;   (org-mode . global-org-modern-mode))


;; Apply vertico-posframe customizations after the package loads
(after! vertico-posframe
  (load! "vertico-posframe-custom.el")
  (my-vertico-posframe-setup))



(use-package! auto-dark
  :defer t
  :init
  (setq! auto-dark-dark-theme  'doom-gruvbox
         doom-gruvbox-dark-variant "soft"
         auto-dark-light-theme 'doom-gruvbox-light
         doom-gruvbox-light-variant "hard")
  ;; Inspired by doom-ui.el.
  ;; Note that server-after-make-frame-hook also avoids the issues with an early
  ;; start of the emacs daemon using systemd, which causes problems with the
  ;; DBus connection that auto-dark mode relies upon.
  (let ((hook (if (daemonp)
                  'server-after-make-frame-hook
                'after-init-hook)))
    ;; Depth -95 puts this before doom-init-theme-h, which sounds like a good
    ;; idea, if only for performance reasons.
    (add-hook hook #'auto-dark-mode -95)
    (add-hook hook #'load-better-gruvbox -94)))

(load! "better-gruvbox.el")

;; (use-package! org-modern
;;   :config
;;   (setq org-modern-todo-faces
;;         `(
;;           ("TODO" :background ,(doom-color 'bg) :foreground ,(doom-color 'red) :inverse-video t)
;;           ("DOING" :background ,(doom-color 'bg) :foreground ,(doom-color 'purple) :inverse-video t)
;;           )))
