;;; amixer.el --- Functions to call out to amixer.

;; Copyright (C) 2013 R.W van 't Veer

;; Author: R.W. van 't Veer
;; Created: 18 Jun 2013
;; Keywords: convenience
;; Version: 0.1
;; URL: https://github.com/remvee/amixer-el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Very basic interface to amixer commandline tool to control audio
;; volume.
;;
;; (global-set-key (kbd "<XF86AudioRaiseVolume>") #'amixer-up-volume)
;; (global-set-key (kbd "<XF86AudioLowerVolume>") #'amixer-down-volume)
;; (global-set-key (kbd "<XF86AudioMute>") #'amixer-toggle-mute)

;;; Code:

(defcustom amixer-default-volume-increment 5
  "Default percentage to increment (or decrement) the volume of master."
  :group 'amixer
  :type 'integer)

(defun amixer-get-volume ()
  "Return volume of master in percentage."
  (let* ((command "amixer sget Master playback")
         (output (shell-command-to-string command)))
    (if (string-match "\\[\\([0-9]+\\)%\\]" output)
        (string-to-number (match-string 1 output))
      (error "Unexpected output from amixer: %s" output))))

;;;###autoload
(defun amixer-set-volume (perc)
  "Set volume to PERC of master via amixer."
  (interactive "nVolume (percentage): ")
  (let ((perc (if (< perc 0) 0 (if (> perc 100) 100 perc))))
    (shell-command-to-string (format "amixer sset Master playback %d%%" perc))
    (let (message-log-max) (message "Volume set to %s%%" perc))))

;;;###autoload
(defun amixer-up-volume (&optional perc)
  "Set volume of master via amixer, step size can be passed by PERC."
  (interactive "P")
  (amixer-set-volume (+ (amixer-get-volume)
                        (or perc
                            amixer-default-volume-increment))))

;;;###autoload
(defun amixer-down-volume (&optional perc)
  "Set volume of master via amixer, step size can be passed by PERC."
  (interactive "P")
  (amixer-up-volume (* -1 (or perc
                              amixer-default-volume-increment))))

;;;###autoload
(defun amixer-toggle-mute ()
  "Mute/unmute master via amixer."
  (interactive)
  (shell-command-to-string "amixer -D pulse set Master toggle"))

(provide 'amixer)

;;; amixer.el ends here
