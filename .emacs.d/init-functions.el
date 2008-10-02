;;flymake-ler(file line type text &optional full-file)
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the
message in the minibuffer"
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
  "Add functionality to the post command hook so that if the
cursor is sitting on a flymake error the error information is
displayed in the minibuffer (rather than having to mouse over
it)"
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





(defun clean-buffer ()
  "clean and re-indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (tabify (point-min) (point-max)))



(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

