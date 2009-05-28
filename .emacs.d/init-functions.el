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

(defun dwim-home ()
  "Moves to beginning-of-line, skipping indent, unless already at start of indent, in which case move to column 0"
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
	  (beginning-of-line)
	(back-to-indentation)))
(put 'dwim-home 'CUA 'move)

(defalias 'qrr 'query-replace-regexp)

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

(defun find-tags-file (tag-file-name)
  "Recursively searches each parent directory for a file named `tag-file-name' and returns the path to that file or nil if a tags file is not found. Returns nil if the buffer is not visiting a file"
  (labels
      ((find-tags-file-r (path)
						 (let* ((parent (file-name-directory path))
								(possible-tags-file (concat parent tag-file-name)))
						   (cond
							((file-exists-p possible-tags-file) (throw 'found-it possible-tags-file))
							((string= (concat "/" tag-file-name) possible-tags-file) (error "no tags file found"))
							(t (find-tags-file-r (directory-file-name parent)))))))

    (if (buffer-file-name)
        (catch 'found-it
          (find-tags-file-r (buffer-file-name)))
	  (error "buffer is not visiting a file"))))

(defun set-tags-file-path ()
  "calls `find-tags-file' to recursively search up the directory tree to find a file named `.tags'. If found, calls `visit-tags-table' with that path as an argument, otherwise raises an error."
  (interactive)
  (visit-tags-table (find-tags-file ".tags")))

(defun create-tags-file ()
  (interactive)
  (let*
	  ((project-dir (read-directory-name "Project directory? "))
	   (project-tags-file (concat project-dir ".tags"))
	   (project-files (concat project-dir "*"))
	   (ctags-args '()))
	(call-process "ctags-exuberant" nil nil t "-e" "-f" project-tags-file " " project-files)))