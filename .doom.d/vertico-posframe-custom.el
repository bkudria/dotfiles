;;; vertico-posframe-custom.el --- Portrait mode optimization for vertico-posframe -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Benjamin Kudria
;;
;; Author: Benjamin Kudria <ben@kudria.net>
;; Keywords: completion, convenience
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (vertico "0.28") (posframe "1.0.0"))
;; URL: https://github.com/bkudria/dotfiles

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
;;   • Fixed top position at the point where a maximum-height frame would be vertically centered
;;   • Horizontally centered
;;   • Maintains position regardless of candidate count
;;
;; Usage:
;;   (require 'vertico-posframe-custom)
;;   (after! vertico-posframe
;;     (vertico-posframe-portrait-setup))

;;; Code:

(require 'vertico-posframe)

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

(defcustom vertico-posframe-portrait-min-height 3
  "Minimum height for the vertico-posframe in portrait mode."
  :type 'integer
  :group 'vertico-posframe-portrait)

;; Position handler using posframe's native API
(defun vertico-posframe-position-portrait-handler (info)
  "Position handler for portrait mode that anchors the top edge consistently.
INFO is the position info from posframe.
Positions the frame so its top edge is where it would be if a max-height
frame were vertically centered in the parent frame."
  (let* ((parent-frame-width (plist-get info :parent-frame-width))
         (parent-frame-height (plist-get info :parent-frame-height))
         (posframe-width (plist-get info :posframe-width))
         ;; Calculate top position that would vertically center a max-height frame
         (top-margin-ratio (/ (- 1.0 vertico-posframe-portrait-height-ratio) 2.0))
         (y (floor (* parent-frame-height top-margin-ratio)))
         ;; Center horizontally
         (x (/ (- parent-frame-width posframe-width) 2)))
    (cons x y)))

;; Size function - simplified to leverage native vertico-posframe capabilities
(defun vertico-posframe-get-size-portrait (buffer)
  "Calculate optimal dimensions for BUFFER in portrait mode."
  (let* ((frame-width (frame-width))
         (frame-height (frame-height))
         (width (floor (* frame-width vertico-posframe-portrait-width-ratio)))
         (max-height (floor (* frame-height vertico-posframe-portrait-height-ratio)))
         (candidates (buffer-local-value 'vertico--total buffer))
         ;; Count the number of group titles if grouping is enabled
         (group-count (if (and (buffer-local-value 'vertico-group-format buffer)
                               (buffer-local-value 'vertico--groups buffer))
                          (length (buffer-local-value 'vertico--groups buffer))
                        0))
         ;; Add group titles to the needed height calculation
         (needed-height (min max-height 
                            (+ 1 (max vertico-posframe-portrait-min-height 
                                     (min vertico-count candidates))
                               group-count))))
    (list :height needed-height
          :width width
          :min-height vertico-posframe-portrait-min-height
          :min-width width)))

(defun vertico-posframe-portrait--update-count (_)
  "Update vertico-count based on frame height.
Called by `window-size-change-functions' with an ignored parameter."
  (setq-default vertico-count 
                (max vertico-posframe-portrait-min-height 
                     (floor (* (frame-height) vertico-posframe-portrait-height-ratio)))))

;;;###autoload
(defun vertico-posframe-portrait-setup ()
  "Configure vertico-posframe for portrait orientation."
  ;; Validate settings
  (when (or (< vertico-posframe-portrait-width-ratio 0)
            (> vertico-posframe-portrait-width-ratio 1))
    (user-error "Width ratio must be between 0 and 1"))
  
  (when (or (< vertico-posframe-portrait-height-ratio 0)
            (> vertico-posframe-portrait-height-ratio 1))
    (user-error "Height ratio must be between 0 and 1"))
  
  ;; Set vertico-posframe parameters directly
  (setq-default vertico-posframe-poshandler #'vertico-posframe-position-portrait-handler
                vertico-posframe-size-function #'vertico-posframe-get-size-portrait
                vertico-posframe-min-width nil  ; Let our size function handle this
                vertico-posframe-min-height nil ; Let our size function handle this
                vertico-count (max vertico-posframe-portrait-min-height 
                                  (floor (* (frame-height) 
                                          vertico-posframe-portrait-height-ratio))))
  
  ;; Update vertico-count when frame size changes
  (add-hook 'window-size-change-functions #'vertico-posframe-portrait--update-count))

;; For backward compatibility
(defalias 'my-vertico-posframe-setup 'vertico-posframe-portrait-setup)

(provide 'vertico-posframe-custom)
;;; vertico-posframe-custom.el ends here