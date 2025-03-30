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
;;   • Width: exactly 80% of parent frame (fixed ratio)
;;   • Height: dynamically sized based on candidates
;;   • Height maximum: 80% of parent frame height
;;
;; - Candidate display:
;;   • Shows maximum possible candidates up to 80% frame height
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
;;     (my-vertico-posframe-setup))

;;; Code:

(eval-when-compile (require 'cl-lib))

;; Declare external functions to satisfy byte-compiler
(declare-function vertico--update "ext:vertico")
(declare-function vertico--exhibit "ext:vertico")

;; Customization options
(defgroup vertico-posframe-custom nil
  "Custom posframe appearance for vertico in portrait mode."
  :group 'vertico
  :prefix "my-vertico-")

(defcustom my-vertico-width-ratio 0.9
  "Ratio of frame width to use for the posframe width (0.0-1.0)."
  :type 'float
  :group 'vertico-posframe-custom)

(defcustom my-vertico-height-ratio 0.9
  "Maximum ratio of frame height to use for the posframe (0.0-1.0)."
  :type 'float
  :group 'vertico-posframe-custom)

;; State variables (internal use)
(defvar-local my-vertico-max-count nil 
  "Maximum number of candidates that can fit in frame height.")

(defvar-local my-vertico-original-count nil 
  "Original vertico-count value to restore when minibuffer exits.")

(defvar-local my-vertico-last-candidate-count nil 
  "Cache of last candidate count to prevent redundant updates.")

(defvar my-vertico-cached-dimensions nil
  "Cached frame dimensions to avoid redundant calculations.")

;; Custom posframe position handler - position vertically based on height ratio
(defun my-vertico-posframe-position-handler (info)
  "Position handler placing frame centered from the top of the parent frame.
Position is calculated as (1-height-ratio)/2 to maintain visual balance.
INFO is the plist of frame parameters provided by posframe."
  (let* ((parent-frame-height (plist-get info :parent-frame-height))
         (parent-frame-width (plist-get info :parent-frame-width))
         (posframe-width (plist-get info :posframe-width))
         ;; Center horizontally
         (x (/ (- parent-frame-width posframe-width) 2))
         ;; Position to balance the frame - derived from height ratio
         (top-margin-ratio (/ (- 1.0 my-vertico-height-ratio) 2.0))
         (y (floor (* parent-frame-height top-margin-ratio))))
    (cons x y)))

;; Calculate and cache frame dimensions with memoization
(defun my-vertico-get-frame-dimensions ()
  "Calculate and cache frame dimensions based on customizable ratios.
Uses memoization to avoid redundant calculations."
  (or my-vertico-cached-dimensions
      (let* ((frame-height (frame-height))
             (frame-width (frame-width))
             (height (- (floor (* frame-height my-vertico-height-ratio)) 2)) ; Account for header and mode line
             (width (floor (* frame-width my-vertico-width-ratio))))
        (setq my-vertico-cached-dimensions
              (list :height height
                    :width width
                    :max-height height)))))

;; Calculate maximum vertico-count based on frame dimensions
(defun my-vertico-calculate-max-count ()
  "Calculate maximum number of candidates that fit in configured frame height ratio."
  ;; Get dimensions directly from the memoized function
  (let ((dimensions (my-vertico-get-frame-dimensions)))
    (max 3 (plist-get dimensions :height))))

;; Adaptive count adjustment - only runs when candidate list changes
(defun my-vertico-update-count-advice (orig-function &rest arguments)
  "Adjust vertico-count based on available candidates.
Advice for `vertico--update'. ORIG-FUNCTION and ARGUMENTS are passed through."
  (let ((result (apply orig-function arguments)))
    ;; Process candidates only when available and count has changed
    (when (and (boundp 'vertico--candidates)
               (let ((current-count (length vertico--candidates)))
                 (when (not (equal current-count my-vertico-last-candidate-count))
                   ;; Update the cached count
                   (setq-local my-vertico-last-candidate-count current-count)
                   
                   ;; Use cl-flet to avoid redundant calculations
                   (cl-flet ((get-max-count ()
                               (or my-vertico-max-count
                                 (setq-local my-vertico-max-count
                                             (my-vertico-calculate-max-count)))))
                     (let ((adjusted-count (min (get-max-count) (max 3 current-count))))
                       
                       ;; Preserve original count for restoration
                       (unless my-vertico-original-count
                         (setq-local my-vertico-original-count vertico-count))
                       
                       ;; Update vertico-count if needed
                       (unless (= vertico-count adjusted-count)
                         (setq-local vertico-count adjusted-count))))
                   t))))
    result))

;; Cleanup function to restore original settings
(defun my-vertico-restore-count ()
  "Restore original vertico-count when minibuffer exits."
  (when my-vertico-original-count
    (setq vertico-count my-vertico-original-count)
    (setq my-vertico-original-count nil)
    (setq my-vertico-last-candidate-count nil)
    (setq my-vertico-max-count nil)
    (setq my-vertico-cached-dimensions nil)))

;; Dynamic size calculation based on frame size and candidate count
(defun my-vertico-posframe-size-function (buffer)
  "Calculate optimal posframe dimensions for BUFFER.
Returns plist with :height, :width, and min-values."
  ;; Ensure dimensions are calculated
  (unless my-vertico-cached-dimensions
    (my-vertico-get-frame-dimensions))
  
  (let* (;; Get dimensions from cache
         (width (plist-get my-vertico-cached-dimensions :width))
         (max-height (plist-get my-vertico-cached-dimensions :height))
         ;; Get actual candidate count from the buffer
         (actual-candidate-count 
          (with-current-buffer buffer
            (if (boundp 'vertico--candidates)
                (length vertico--candidates)
              0)))
         ;; Adjust height based on candidate count (add 1 for header)
         (needed-height (min max-height 
                             (+ 1 (if (> actual-candidate-count 0)
                                      (min vertico-count actual-candidate-count)
                                    3)))))

    (list :height needed-height
          :width width
          :min-height 3
          :min-width width)))

;;;###autoload
(defun my-vertico-posframe-setup ()
  "Configure vertico-posframe with custom sizing and positioning.
This optimizes the posframe display for portrait monitor use."
  ;; Calculate initial frame dimensions and initialize vertico-count in one step
  (let ((dimensions (my-vertico-get-frame-dimensions)))
    (setq-default vertico-count 
                  (max 3 (plist-get dimensions :height))))
  
  ;; Apply custom handlers to vertico-posframe
  (setq vertico-posframe-poshandler #'my-vertico-posframe-position-handler)
  (setq vertico-posframe-size-function #'my-vertico-posframe-size-function)
  
  ;; Add advice to optimize size based on candidate count
  ;; Note: We use vertico--update not vertico--exhibit to avoid processing on cursor movement
  (advice-add #'vertico--update :around #'my-vertico-update-count-advice)
  
  ;; Cleanup when minibuffer exits
  (add-hook 'minibuffer-exit-hook #'my-vertico-restore-count)
  
  ;; Recalculate dimensions when frame is resized
  (add-hook 'window-size-change-functions
            (lambda (_) (setq my-vertico-cached-dimensions nil))))

(provide 'vertico-posframe-custom)
;;; vertico-posframe-custom.el ends here