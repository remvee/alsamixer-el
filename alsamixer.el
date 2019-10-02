;;; alsamixer.el --- Functions to call out to amixer.

;; Copyright (C) 2013 R.W van 't Veer

;; Author: R.W. van 't Veer
;; Created: 18 Jun 2013
;; Keywords: convenience
;; Version: 0.1
;; URL: https://github.com/remvee/alsamixer-el

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
;; (global-set-key (kbd "<XF86AudioRaiseVolume>") #'alsamixer-up-volume)
;; (global-set-key (kbd "<XF86AudioLowerVolume>") #'alsamixer-down-volume)
;; (global-set-key (kbd "<XF86AudioMute>") #'alsamixer-toggle-mute)

;;; Code:

(defgroup alsamixer nil "Functions to call out to amixer."
  :prefix "alsamixer-"
  :group 'multimedia)

(defcustom alsamixer-default-volume-increment 5
  "Default percentage to increment (or decrement) the volume of master."
  :group 'alsamixer
  :type 'integer)

(defcustom alsamixer-amixer-command "amixer"
  "Name of amixer command."
  :group 'alsamixer
  :type 'string)

(defcustom alsamixer-card nil
  "Card number to control."
  :group 'alsamixer
  :type 'string)

(defcustom alsamixer-device nil
  "Device name to control."
  :group 'alsamixer
  :type 'string)

(defcustom alsamixer-control "Master"
  "Name of control."
  :group 'alsamixer
  :type 'string)

(defun alsamixer-command (args &rest objs)
  "Build an amixer command with given ARGS and OBJS, as in `format'."
  (let* ((command alsamixer-amixer-command)
         (command (if alsamixer-card
                      (format "%s -c %d" command alsamixer-card)
                    command))
         (command (if alsamixer-device
                      (format "%s -D %s" command alsamixer-device)
                    command))
         (args (replace-regexp-in-string "%C" alsamixer-control args t t)))
    (apply #'format (concat command " "args) objs)))

(defun alsamixer-get-volume ()
  "Return volume in percentage."
  (let* ((command (alsamixer-command "sget %C playback"))
         (output (shell-command-to-string command)))
    (if (string-match "\\[\\([0-9]+\\)%\\]" output)
        (string-to-number (match-string 1 output))
      (error "Unexpected output from %s: %s" alsamixer-amixer-command output))))

;;;###autoload
(defun alsamixer-set-volume (perc)
  "Set volume to PERC."
  (interactive "nVolume (percentage): ")
  (let ((perc (if (< perc 0) 0 (if (> perc 100) 100 perc))))
    (shell-command-to-string (alsamixer-command "sset %C playback %d%%" perc))
    (let (message-log-max) (message "Volume set to %s%%" perc))))

;;;###autoload
(defun alsamixer-up-volume (&optional perc)
  "Set volume, step size can be passed by PERC."
  (interactive "P")
  (alsamixer-set-volume (+ (alsamixer-get-volume)
                           (or perc
                               alsamixer-default-volume-increment))))

;;;###autoload
(defun alsamixer-down-volume (&optional perc)
  "Set volume, step size can be passed by PERC."
  (interactive "P")
  (alsamixer-up-volume (* -1 (or perc
                                 alsamixer-default-volume-increment))))

;;;###autoload
(defun alsamixer-toggle-mute ()
  "Mute / unmute."
  (interactive)
  (shell-command-to-string (alsamixer-command "set %C toggle")))

(provide 'alsamixer)

;;; alsamixer.el ends here
