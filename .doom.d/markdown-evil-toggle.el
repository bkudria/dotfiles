;;; markdown-evil-toggle.el --- Toggle markdown markup visibility with Evil states -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Benjamin Kudria
;;
;; Author: Benjamin Kudria <ben@kudria.net>
;; Keywords: markdown, evil, convenience
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (markdown-mode "2.3") (evil "1.0.0"))

;;; Commentary:
;;
;; This package syncs markdown markup visibility with Evil states:
;; - Normal/Visual mode: Markup is hidden (clean reading view)
;; - Insert/Replace mode: Markup is shown (editing view)
;;
;; Performance optimizations:
;; - Window-only font-lock flush (not entire buffer)
;; - Memoized `markdown-get-lang-mode` for code block fontification

;;; Code:

(after! markdown-mode
  ;; === Performance: Memoize markdown-get-lang-mode ===
  ;; This function is expensive (scans auto-mode-alist) and called for every
  ;; code block during fontification. Cache results to avoid repeated lookups.

  (defvar bk/markdown-lang-mode-cache (make-hash-table :test 'equal)
    "Cache for `markdown-get-lang-mode' results.")

  (defadvice! bk/markdown-cache-lang-mode-a (fn lang)
    "Cache results of `markdown-get-lang-mode' for performance."
    :around #'markdown-get-lang-mode
    (let ((cached (gethash lang bk/markdown-lang-mode-cache 'bk/not-found)))
      (if (eq cached 'bk/not-found)
          (let ((result (funcall fn lang)))
            (puthash lang result bk/markdown-lang-mode-cache)
            result)
        cached)))

  (defun bk/markdown-clear-lang-mode-cache ()
    "Clear the markdown language mode cache.
Use if you've installed new language modes during your session."
    (interactive)
    (let ((count (hash-table-count bk/markdown-lang-mode-cache)))
      (clrhash bk/markdown-lang-mode-cache)
      (message "Cleared %d cached markdown language mode entries" count)))

  ;; === Evil State Markup Toggle ===
  ;; Hide markup in normal mode, show in insert mode.
  ;; Uses window-only font-lock-flush for performance.

  (defun bk/markdown-ensure-markup-hidden ()
    "Ensure markdown markup is hidden in current buffer."
    (when (and (derived-mode-p 'markdown-mode)
               (not markdown-hide-markup))
      (setq markdown-hide-markup t)
      (add-to-invisibility-spec 'markdown-markup)
      (font-lock-flush (window-start) (window-end nil t))))

  (defun bk/markdown-ensure-markup-shown ()
    "Ensure markdown markup is shown in current buffer."
    (when (and (derived-mode-p 'markdown-mode)
               markdown-hide-markup)
      (setq markdown-hide-markup nil)
      (remove-from-invisibility-spec 'markdown-markup)
      (font-lock-flush (window-start) (window-end nil t))))

  (defun bk/markdown-sync-markup-to-evil-state ()
    "Sync markup visibility to current evil state."
    (when (derived-mode-p 'markdown-mode)
      (if (memq evil-state '(insert replace))
          (bk/markdown-ensure-markup-shown)
        (bk/markdown-ensure-markup-hidden))))

  (add-hook 'markdown-mode-hook
            (defun bk/markdown-setup-evil-markup-hiding ()
              "Setup evil state-dependent markup hiding for markdown."
              (add-hook 'evil-normal-state-entry-hook #'bk/markdown-ensure-markup-hidden nil t)
              (add-hook 'evil-visual-state-entry-hook #'bk/markdown-ensure-markup-hidden nil t)
              (add-hook 'evil-insert-state-entry-hook #'bk/markdown-ensure-markup-shown nil t)
              (add-hook 'evil-replace-state-entry-hook #'bk/markdown-ensure-markup-shown nil t)
              (add-hook 'evil-local-mode-hook #'bk/markdown-sync-markup-to-evil-state nil t)
              (when (bound-and-true-p evil-local-mode)
                (bk/markdown-sync-markup-to-evil-state)))))

(provide 'markdown-evil-toggle)
;;; markdown-evil-toggle.el ends here
