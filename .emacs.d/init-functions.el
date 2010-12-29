;; flymake-ler(file line type text &optional full-file)
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in the minibuffer"
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
          (let ((err (car (second elem))))
            (message "%s" (flymake-ler-text err)))))))

(defadvice flymake-goto-next-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-mode (before post-command-stuff activate compile)
  "Add functionality to the post command hook so that if the cursor is sitting on a flymake error the error information is displayed in the minibuffer (rather than having to mouse over it)"
  (set (make-local-variable 'post-command-hook)
       (cons 'show-fly-err-at-point post-command-hook)))

(defvar flymake-fringe-overlays nil)
(make-variable-buffer-local 'flymake-fringe-overlays)

(defadvice flymake-make-overlay
  (after
   add-to-fringe
   first
   (beg end tooltip-text face mouse-face)
   activate
   compile)
  (push
   (fringe-helper-insert-region
    beg
    end
    (fringe-lib-load
     (if (eq face 'flymake-errline)
         fringe-lib-exclamation-mark
       fringe-lib-question-mark))
    'left-fringe
    'font-lock-warning-face)
   flymake-fringe-overlays))

(defadvice flymake-delete-own-overlays
  (after
   remove-from-fringe
   activate
   compile)
  (mapc 'fringe-helper-remove flymake-fringe-overlays)
  (setq flymake-fringe-overlays nil))

(defun clean-buffer-or-region ()
  "clean and re-indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (dtrt-indent-adapt)
  (if (use-region-p)
      (narrow-to-region (region-beginning) (region-end)))

  (indent-region (point-min) (point-max) nil)
  (align (point-min) (point-max))
  (widen))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; Smooth scrolling
(defun smooth-scroll (increment)
  (scroll-up increment))

(global-set-key [(mouse-5)] '(lambda () (interactive) (smooth-scroll 1)))
(global-set-key [(mouse-4)] '(lambda () (interactive) (smooth-scroll -1)))

(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to . 
Symbols matching the text at point are put first in the completion list      . "
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(defun ido-save-or-write-file ()
  "Save the current buffer to a file, or, prompt to write it to a file with ido-write-file"
  (interactive)
  (if (eq buffer-file-name nil)
      (ido-write-file)
    (save-buffer))
  )

(defun ido-execute ()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     ": "
     (let (cmd-list)
       (mapatoms
        (lambda (S)
          (when (commandp S)
            (setq cmd-list (cons (format "%S" S) cmd-list)))))
       cmd-list)))))

(defun exit-and-ido-recentf ()
  (ido-recentf)
  (abort-recursive-edit))


;; Redefine so we don't insert a space
(defun paredit-space-for-delimiter-p (endp delimiter)
  nil)

(defun dwim-home ()
  "Moves to beginning-of-line, skipping indent, unless already at start of indent, in which case move to column 0"
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))
(put 'dwim-home 'CUA 'move)

(defalias 'qrr 'query-replace-regexp)
(defalias 'ar 'align-regexp)

(defun clone-current-buffer-in-new-frame ()
  "Creates a new frame viewing the current buffer, and switches the parent frame to *scratch*"
  (interactive)
  (make-frame)
  (switch-to-buffer (get-buffer "*scratch*")))

(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (dolist
      (buffer (buffer-list))
    (kill-buffer buffer))
  (delete-other-windows))

;; Use ido to find files in tags file
(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))

(defun create-tags-file ()
  (interactive)
  (let ((olddir default-directory))
    (cd-absolute project-base-dir)
    (call-process "ctags-exuberant" nil nil t "-e" "-R" "-f .etags")
    (cd-absolute olddir)
    (visit-tags-table-buffer (concat project-base-dir "/.etags"))
    (message "Created tags file for: %s" project-base-dir)))

;; Find tag using ido
(defun ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
            (unless (integerp x)
              (push (prin1-to-string x t) tag-names)))
          tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))

;; Better DLVs, from: http://atomized.org/2009/05/emacs-23-easier-directory-local-variables/
(defmacro absolute-dirname (path)
  "Return the directory name portion of a path . 

If PATH is local, return it unaltered                             . 
If PATH is remote, return the remote diretory portion of the path . "
  `(cond ((tramp-tramp-file-p ,path)
          (elt (tramp-dissect-file-name ,path) 3))
         (t ,path)))

(defmacro dir-locals (dir vars)
  "Set local variables for a directory . 

DIR is the base diretory to set variables on . 

VARS is an alist of variables to set on files opened under DIR,
in the same format as `dir-locals-set-class-variables' expects . "
  `(let ((name (intern (concat "dir-locals-"
                               ,(md5 (expand-file-name dir)))))
         (base-dir ,dir)
         (base-abs-dir ,(absolute-dirname dir)))
     (dir-locals-set-class-variables name ,vars)
     (dir-locals-set-directory-class ,dir name nil)))



(defun search-hayoo ()
  (interactive)
  (let* ((default (word-at-point))
         (term (read-string (format "Search Hayoo (%s): " default))))
    (let ((term (if (zerop(length term)) default term)))
      (browse-url (format "http://holumbus.fh-wedel.de/hayoo/hayoo.html?query=%s&start" term)))))

