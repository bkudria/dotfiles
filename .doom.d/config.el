;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


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
      doom-variable-pitch-font (font-spec :family "Gill Sans" :size 28))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox
      doom-gruvbox-dark-variant "soft")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

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
(setq initial-frame-alist '((fullscreen . fullscreen)))

(setq scroll-preserve-screen-position t
      scroll-conservatively 0
      maximum-scroll-margin 0.5
      scroll-margin 99999)

(global-subword-mode)

(use-package! rainbow-identifiers
  :hook (prog-mode . rainbow-identifiers-mode))

(use-package! dimmer
  :config (dimmer-mode))

(use-package! evil-extra-operator
  :bind (:map evil-normal-state-map
         ("gl" . evil-operator-clone)
         :map evil-visual-state-map
         ("gl" . evil-operator-clone)
         ))

(use-package! evil-surround
  :config (global-evil-surround-mode 1))

(use-package! evil-matchit
  :config (global-evil-matchit-mode 1))

(use-package! evil-replace-with-register
  :bind (:map evil-normal-state-map
         ("gr" . evil-replace-with-register)
         :map evil-visual-state-map
         ("gr" . evil-replace-with-register)))

(use-package! ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-height-alist
        '((t
           lambda (_caller)
           (round (* 0.7 (frame-height) )))))
  )

(use-package! doom-modeline
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  (setq doom-modeline-buffer-encoding t)

  )


(use-package! mini-frame
  :config (mini-frame-mode))

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
