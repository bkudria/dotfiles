;;; iterm.el - Send text to a running iTerm instance

(require 'pcase)
(require 'thingatpt)

(defvar iterm-default-thing 'line
  "The \"thing\" to send if no region is active.
Can be any symbol understood by `bounds-of-thing-at-point'.")

(defvar iterm-empty-line-regexp "^[[:space:]]*$"
  "Regexp to match empty lines, which will not be sent to iTerm.
Set to nil to disable removing empty lines.")

(defun iterm-escape-string (str)
  (let* ((str (replace-regexp-in-string "\\\\" "\\\\" str nil t))
         (str (replace-regexp-in-string "\"" "\\\"" str nil t))
         (str (replace-regexp-in-string "'" "\\'" str nil t)))
    str))

(defun iterm-last-char-p (str char)
  (let ((length (length str)))
    (and (> length 0)
         (char-equal (elt str (- length 1)) char))))

(defun iterm-chop-newline (str)
  (let ((length (length str)))
    (if (iterm-last-char-p str ?\n)
        (substring str 0 (- length 1))
      str)))

(defun iterm-maybe-add-newline (str)
  (if (iterm-last-char-p str ? )
      (concat str "\n")
    str))

(defun iterm-handle-newline (str)
  (iterm-maybe-add-newline (iterm-chop-newline str)))

(defun iterm-maybe-remove-empty-lines (str)
  (if iterm-empty-line-regexp
      (let ((regexp iterm-empty-line-regexp)
            (lines (split-string str "\n")))
        (mapconcat #'identity
                   (delq nil (mapcar (lambda (line)
                                       (unless (string-match-p regexp line)
                                         line))
                                     lines))
                   "\n"))
    str))

(defun iterm-send-string (str)
  "Send STR to a running iTerm instance."
  (let* ((str (iterm-maybe-remove-empty-lines str))
         (str (iterm-handle-newline str))
         (str (iterm-escape-string str)))
    (let ((cmd (concat "osascript "
                       "-e 'tell app \"iTerm2\"' "
                       "-e 'tell current window' "
                       "-e 'tell current session' "
                       "-e $'write text \"" str "\"' "
                       "-e 'end tell' "
                       "-e 'end tell' "
                       "-e 'end tell' ")))
      (shell-command cmd)
      )))

(defun iterm-text-bounds ()
  (pcase-let ((`(,beg . ,end) (if (use-region-p)
                                  (cons (region-beginning) (region-end))
                                (bounds-of-thing-at-point
                                 iterm-default-thing))))
    (list beg end)))

(defun iterm-send-text (beg end)
  "Send buffer text in region from BEG to END to iTerm.
If called interactively without an active region, send text near
point (determined by `iterm-default-thing') instead."
  (interactive (iterm-text-bounds))
  (let ((str (buffer-substring-no-properties beg end)))
    (iterm-send-string str))
  (forward-line 1))

(defun iterm-send-text-ruby (beg end)
  "Send buffer text in region from BEG to END to iTerm.
If called interactively without an active region, send text near
point (determined by `iterm-default-thing') instead."
  (interactive (iterm-text-bounds))
  (let ((str (buffer-substring-no-properties beg end)))
    (iterm-send-string (concat "begin\n" str "\nend")))
  (forward-line 1))


(defun ruby-send-region (start end &optional print)
  "Send the current region to the inferior Ruby process."
  (interactive "r\nP")
  (let (term (file (or buffer-file-name (buffer-name))) line)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char start)
        (setq line (+ start (forward-line (- start)) 1))
        (goto-char start)
        (while (progn
                 (setq term (apply 'format ruby-send-terminator (random) (current-time)))
                 (re-search-forward (concat "^" (regexp-quote term) "$") end t)))))
    (iterm-send-string (format "eval <<'%s', %s, %S, %d\n"
                                                term inf-ruby-eval-binding
                                                file line))
    (iterm-send-text-ruby start end)
    (when print (ruby-print-result))))

(provide 'iterm)
