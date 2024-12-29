;;; mpereira-whisper.el --- Minor mode for managing Whisper transcription and system audio -*- lexical-binding: t; -*-

;; Author: Murilo Pereira
;; Version: 1.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: audio, ffmpeg, transcription

;;; Commentary:

;; This minor mode provides utilities for interacting with ffmpeg
;; devices, managing system audio, and transcription workflows related
;; to the Whisper project.
;;
;; Mostly borrowed from Robert Krahn:
;; https://gist.github.com/rksm/04be012be07671cd5e1dc6ec5b077e34

;;; Code:

(require 'cl-lib)

(defun mpereira-whisper-get-ffmpeg-device ()
  "Gets the list of devices available to ffmpeg.

The output of the ffmpeg command is parsed to extract audio and
video devices.

The return value contains two lists, one for video devices and
one for audiodevices.

Each list contains a list of cons cells, where the car is the
device number and the cdr is the device name."
  (unless (string-equal system-type "darwin")
    (error "This function is currently only supported on macOS"))

  (let ((lines (split-string (shell-command-to-string "ffmpeg -list_devices true -f avfoundation -i dummy || true") "\n")))
    (cl-loop with at-video-devices = nil
             with at-audio-devices = nil
             with video-devices = nil
             with audio-devices = nil
             for line in lines
             when (string-match "AVFoundation video devices:" line)
             do (setq at-video-devices t
                      at-audio-devices nil)
             when (string-match "AVFoundation audio devices:" line)
             do (setq at-audio-devices t
                      at-video-devices nil)
             when (and at-video-devices
                       (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
             do (push (cons (string-to-number (match-string 1 line)) (match-string 2 line)) video-devices)
             when (and at-audio-devices
                       (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
             do (push (cons (string-to-number (match-string 1 line)) (match-string 2 line)) audio-devices)
             finally return (list (nreverse video-devices) (nreverse audio-devices)))))

(defun mpereira-whisper-find-device-matching (string type)
  "Get the devices from `mpereira-whisper-get-ffmpeg-device' and
find a device matching STRING.

TYPE can be :video or :audio."
  (let* ((devices (mpereira-whisper-get-ffmpeg-device))
         (device-list (if (eq type :video)
                          (car devices)
                        (cadr devices))))
    (cl-loop for device in device-list
             when (string-match-p string (cdr device))
             return (car device))))

(defcustom mpereira-whisper-default-audio-device nil
  "The default audio device to use for whisper.el and other audio
processes."
  :type 'string)

(defun mpereira-whisper-select-default-audio-device (&optional device-name)
  "Set a default audio device for whisper.el and other audio
processes.
If DEVICE-NAME is provided, it will be used instead of prompting the user."
  (interactive)
  (let* ((audio-devices (cadr (mpereira-whisper-get-ffmpeg-device)))
         (indexes (mapcar #'car audio-devices))
         (names (mapcar #'cdr audio-devices))
         (name (or device-name (completing-read "Select audio device: " names nil t))))
    (setq mpereira-whisper-default-audio-device (mpereira-whisper-find-device-matching name :audio))
    (when (boundp 'whisper--ffmpeg-input-device)
      (setq whisper--ffmpeg-input-device (format ":%s" mpereira-whisper-default-audio-device)))))

(defun mpereira-whisper-kill-transcription ()
  "Kill the current Whisper transcription by copying it to the
 kill-ring."
  (interactive)
  (let ((content (buffer-string)))
    (kill-new content)))

(defun mpereira-system-audio-mute ()
  "Mute system audio using AppleScript."
  (interactive)
  (shell-command "osascript -e 'set volume output muted true'"))

(defun mpereira-system-audio-unmute ()
  "Unmute system audio using AppleScript."
  (interactive)
  (shell-command "osascript -e 'set volume output muted false'"))

;;;###autoload
(define-minor-mode mpereira-whisper-mode
  "Minor mode to manage ffmpeg devices, transcription, and system audio controls."
  :group 'mpereira)

(provide 'mpereira-whisper)

;;; mpereira-whisper.el ends here
