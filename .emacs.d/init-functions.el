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

(defun ido-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
	(find-file
	 (ido-completing-read "Recentf open: "
						  (mapcar (lambda (path)
									(replace-regexp-in-string home "~" path))
								  recentf-list)
						  nil t)))
  )

;; Redefine so we don't insert a space
(defun paredit-space-for-delimiter-p (endp delimiter)
  nil)

;; From http://news.slashdot.org/comments.pl?sid=1021471&cid=25675361 , modified
(defun sane-beginning-of-line ()
  "Moves to beginning-of-line, skipping indent"

  (interactive)
  (move-beginning-of-line 1)
  (skip-chars-forward " \t"))

(defalias 'qrr 'query-replace-regexp)


(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a symbol to navigate to"
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
	(let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
		   (position (cdr (assoc selected-symbol name-and-pos))))
	  (goto-char position))))
