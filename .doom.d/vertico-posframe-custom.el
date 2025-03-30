;;; vertico-posframe-custom.el --- Portrait mode optimization for vertico-posframe -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Benjamin Kudria
;;
;; Author: Benjamin Kudria <ben@kudria.net>
;; Keywords: completion, convenience
;; Package-Requires: ((emacs "26.1") (vertico "0.28") (posframe "1.0.0"))

;;; Commentary:
;;
;; Customizes vertico-posframe for vertical/portrait monitor use with
;; the following behavior:
;;
;; - Dimensions:
;;   • Width: exactly 90% of parent frame (fixed ratio)
;;   • Height: dynamically sized based on candidates
;;   • Height maximum: 90% of parent frame height
;;
;; - Candidate display:
;;   • Shows maximum possible candidates up to 90% frame height
;;   • Shrinks the frame when there are few candidates
;;   • Dynamically adjusts when input changes
;;
;; - Positioning:
;;   • Fixed position at 10% from top of frame
;;   • Horizontally centered
;;   • Maintains position regardless of candidate count
;;
;; Usage:
;;   (require 'vertico-posframe-custom)
;;   (after! vertico-posframe
;;     (vertico-posframe-portrait-setup))

;;; Code:

(require 'vertico-posframe)
(require 'cl-lib)

;; Customization options
(defgroup vertico-posframe-portrait nil
  "Portrait mode settings for vertico-posframe."
  :group 'vertico)

(defcustom vertico-posframe-portrait-width-ratio 0.9
  "Ratio of frame width for posframe (0.0-1.0)."
  :type 'float
  :group 'vertico-posframe-portrait)

(defcustom vertico-posframe-portrait-height-ratio 0.9
  "Maximum ratio of frame height for posframe (0.0-1.0)."
  :type 'float
  :group 'vertico-posframe-portrait)

;; Custom position handler using posframe's position function API
(defun vertico-posframe-position-portrait-handler (info)
  "Position handler for portrait mode that anchors the top edge consistently.
INFO is the position info from posframe."
  (let* ((parent-frame-width (plist-get info :parent-frame-width))
         (parent-frame-height (plist-get info :parent-frame-height))
         (posframe-width (plist-get info :posframe-width))
         ;; Calculate top position that would vertically center a max-height frame
         (top-margin-ratio (/ (- 1.0 vertico-posframe-portrait-height-ratio) 2.0))
         (y (floor (* parent-frame-height top-margin-ratio)))
         ;; Center horizontally
         (x (/ (- parent-frame-width posframe-width) 2)))
    (cons x y)))

;; Improved size function that works with vertico-posframe's API
(defun vertico-posframe-get-size-portrait (buffer)
  "Calculate optimal dimensions for BUFFER in portrait mode."
  (let* ((frame-width (frame-width))
         (frame-height (frame-height))
         (width (floor (* frame-width vertico-posframe-portrait-width-ratio)))
         (max-height (floor (* frame-height vertico-posframe-portrait-height-ratio)))
         (candidates (with-current-buffer buffer
                       (if (boundp 'vertico--candidates)
                           (length vertico--candidates)
                         0)))
         ;; Height based on actual number of candidates (or min 3)
         (needed-height (min max-height (+ 1 (max 3 (min vertico-count candidates))))))
    (list :height needed-height
          :width width
          :min-height 3
          :min-width width)))

;;;###autoload
(defun vertico-posframe-portrait-setup ()
  "Configure vertico-posframe for portrait orientation."
  ;; 1. Set the poshandler
  (setq-default vertico-posframe-poshandler #'vertico-posframe-position-portrait-handler)
  
  ;; 2. Set the size function
  (setq-default vertico-posframe-size-function #'vertico-posframe-get-size-portrait)
  
  ;; 3. Configure vertico-count to adapt to frame size
  (setq-default vertico-count (max 3 (floor (* (frame-height) vertico-posframe-portrait-height-ratio))))
  
  ;; 4. Add a hook to update vertico-count when frame size changes
  (add-hook 'window-size-change-functions
            (lambda (_)
              (setq-default vertico-count (max 3 (floor (* (frame-height) vertico-posframe-portrait-height-ratio)))))))

;; For backward compatibility
(defalias 'my-vertico-posframe-setup 'vertico-posframe-portrait-setup)

(provide 'vertico-posframe-custom)
;;; vertico-posframe-custom.el ends here