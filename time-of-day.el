;;; time-of-day.el --- graphical time of day indication in mode line  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Magnus Henoch

;; Author: Magnus Henoch <magnus.henoch@gmail.com>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `time-of-day-mode' is a global minor mode that displays an image
;; representing the time of day in the mode line.  During the "day",
;; it displays the sun going from sunrise to sunset, and during the
;; "night", it displays the moon moving across the sky.
;;
;; "Day" and "night" refer to the times set in `time-of-day-dawn' and
;; `time-of-day-dusk'.  The defaults are meant to show a working day
;; as "day".
;;
;; This mode attempts to show the image only once in each frame, in
;; the right-most bottom-most window.
;;
;; tod-schedule-24hrs.png was taken from Battle for Wesnoth, and is
;; licenced under GPLv2 or later.

;;; Code:

(defconst time-of-day-image-file
;;; tod-schedule-24hrs.png PNG 375x312 375x312+0+0 8-bit sRGB 113KB 0.000u 0:00.000
  (expand-file-name "tod-schedule-24hrs.png"
		    (file-name-directory
		     (or load-file-name
			 buffer-file-name))))

(defgroup time-of-day nil
  "Graphically illustrate the passing of time in the mode line.")

(defcustom time-of-day-dawn 9
  "Dawn occurs at this hour.
To specify fractions of an hour, use a floating-point value."
  :type 'number
  :group 'time-of-day)

(defcustom time-of-day-dusk 18
  "Dusk occurs at this hour (24-hour clock).
To specify fractions of an hour, use a floating-point value."
  :type 'number
  :group 'time-of-day)

(defvar time-of-day-timer nil)

(defvar time-of-day-string "")
(put 'time-of-day-string 'risky-local-variable t)

(defun time-of-day-index-now ()
  (let* ((time (decode-time))
	 (hour (+ (nth 2 time)
		  (/ (nth 1 time) 60.0)
		  (/ (nth 0 time) 3600.0))))
    (cond
     ((<= time-of-day-dawn hour time-of-day-dusk)
      (floor (* 12 (/ (- hour time-of-day-dawn)
		      (float (- time-of-day-dusk time-of-day-dawn))))))
     ((< time-of-day-dusk hour)
      (floor (+ 12
		(* 12
		   (/ (- hour time-of-day-dusk)
		      (float (- (+ 24 time-of-day-dawn) time-of-day-dusk)))))))
     ((< hour time-of-day-dawn)
      (floor (+ 12
		(* 12
		   (/ (- (+ 24 hour) time-of-day-dusk)
		      (float (- (+ 24 time-of-day-dawn) time-of-day-dusk))))))))))

;;;###autoload
(define-minor-mode time-of-day-mode
  "Display pretty time of day pictures in the mode line."
  nil nil nil
  :global t
  (if time-of-day-mode
      (progn
	(time-of-day-update)
	(add-to-list 'global-mode-string '(time-of-day-mode (:eval (time-of-day-maybe))))
	(setq time-of-day-timer
	      (run-with-timer 300 300 'time-of-day-update)))
    (when (timerp time-of-day-timer)
      (cancel-timer time-of-day-timer)
      (setq time-of-day-timer nil))
    (setq global-mode-string (delete '(time-of-day-mode (:eval (time-of-day-maybe)))
				     global-mode-string))))

(defun time-of-day-update ()
  (setq time-of-day-string
	(propertize
	 (string 42)
	 'display
	 (list
	  (let ((index (time-of-day-index-now)))
	    (list
	     'slice
	     (- 1 (/ (1+ (% index 3)) 3.0))
	     (- 1 (/ (1+ (/ index 3)) 8.0))
	     (/ 1 3.0)
	     (/ 1 8.0)))
	  (create-image
	   time-of-day-image-file
	   'png nil :ascent 'center)))))

;;; Autoload this function, in case the specification get saved in
;;; `global-mode-string'.
;;;###autoload
(defun time-of-day-maybe ()
  ;; Only display the image in the bottom right window.
  (if (time-of-day--bottom-right-window-p)
      time-of-day-string
    ;;(propertize "**" 'help-echo (format "no time of day because %s" (window-buffer other-window)))
    ""))

(defun time-of-day--bottom-right-window-p (&optional window)
  (unless window
    (setq window (selected-window)))
  (and (not (time-of-day--window-ineligible-p window))
       (pcase-let ((`(,left ,top ,right ,bottom) (window-edges window)))
	 (let ((other-window (or (window-at (1+ right) top) ;XXX: check both for minibuffers?
				 (window-at left (1+ bottom)))))
	   (time-of-day--window-ineligible-p other-window)))))

(defun time-of-day--window-ineligible-p (window)
  (or (null window)
      (eq window (minibuffer-window))
      ;; Putting the time of day graphic in the mode line of a
      ;; completion buffer often results in the completions being
      ;; hidden.  Let's avoid that.  Such windows usually have a
      ;; preserved size.
      (let ((preserved-size (window-parameter window 'window-preserved-size)))
	(and preserved-size
	     (eq (car preserved-size) (window-buffer window))))))

(provide 'time-of-day)
;;; time-of-day.el ends here
