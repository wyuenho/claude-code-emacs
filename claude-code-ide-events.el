;;; claude-code-ide-events.el --- IDE protocol event notifications -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Keywords: tools, convenience
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module implements IDE protocol event notifications:
;; - selection_changed: Automatically sent when user selection changes
;; - at_mentioned: Sent when user explicitly includes code as context

;;; Code:

(require 'claude-code-base)
(require 'claude-code-ide-server)
(require 'projectile)

;;; Variables

(defvar claude-code-ide-events-selection-timer nil
  "Timer for debouncing selection change notifications.")

(defvar claude-code-ide-events-enabled t
  "Whether IDE event notifications are enabled.")

(defcustom claude-code-ide-events-selection-delay 0.3
  "Delay in seconds before sending selection change notifications.
This debounces rapid cursor movements."
  :type 'number
  :group 'claude-code)

;;; Selection Changed Notification

(defun claude-code-ide-events-selection-changed ()
  "Handle selection change events.
Debounces rapid changes and sends selection_changed notification."
  (when (and claude-code-ide-events-enabled
             (buffer-file-name)
             (projectile-project-root))
    ;; Cancel existing timer
    (when claude-code-ide-events-selection-timer
      (cancel-timer claude-code-ide-events-selection-timer))

    ;; Set new timer to debounce rapid changes
    (setq claude-code-ide-events-selection-timer
          (run-with-timer claude-code-ide-events-selection-delay nil
                          #'claude-code-ide-events-send-selection-changed))))

(defun claude-code-ide-events-send-selection-changed ()
  "Send selection_changed notification to Claude Code."
  (condition-case err
      (when-let ((project-root (projectile-project-root))
                 (file-path (buffer-file-name)))
        (let* ((has-selection (use-region-p))
               (start-pos (if has-selection (region-beginning) (point)))
               (end-pos (if has-selection (region-end) (point)))
               (text (if has-selection
                        (buffer-substring-no-properties start-pos end-pos)
                      ""))
               (start-line (line-number-at-pos start-pos))
               (end-line (line-number-at-pos end-pos))
               (start-char (save-excursion
                            (goto-char start-pos)
                            (current-column)))
               (end-char (save-excursion
                          (goto-char end-pos)
                          (current-column)))
               (file-url (concat "file://" file-path))
               (params `((text . ,text)
                        (filePath . ,file-path)
                        (fileUrl . ,file-url)
                        (selection . ((start . ((line . ,start-line)
                                                (character . ,start-char)))
                                     (end . ((line . ,end-line)
                                           (character . ,end-char)))))
                        (isEmpty . ,(if has-selection json-false t)))))
          (claude-code-ide-send-notification
           (claude-code-normalize-project-root project-root)
           "selection_changed"
           params)))
    (error
     (message "Error sending selection_changed notification: %S" err))))

;;; At Mentioned Notification

(defun claude-code-ide-send-at-mention ()
  "Send at_mentioned notification for current selection.
This explicitly includes the selected code as context for Claude Code.
User must have an active selection."
  (interactive)
  (if (not (use-region-p))
      (message "No active selection. Please select code to mention.")
    (condition-case err
        (when-let ((project-root (projectile-project-root))
                   (file-path (buffer-file-name)))
          (let* ((start-line (line-number-at-pos (region-beginning)))
                 (end-line (line-number-at-pos (region-end)))
                 (params `((filePath . ,file-path)
                          (lineStart . ,start-line)
                          (lineEnd . ,end-line))))
            (claude-code-ide-send-notification
             (claude-code-normalize-project-root project-root)
             "at_mentioned"
             params)
            (message "Sent code mention to Claude Code (lines %d-%d)" start-line end-line)))
      (error
       (message "Error sending at_mentioned notification: %S" err)))))

;;; Event Management

(defun claude-code-ide-events-enable ()
  "Enable IDE event notifications.
Hooks into selection changes to notify Claude Code."
  (interactive)
  (setq claude-code-ide-events-enabled t)
  ;; Add hooks for selection changes
  ;; Note: We use post-command-hook as there's no dedicated selection-change hook
  (add-hook 'post-command-hook #'claude-code-ide-events-selection-changed)
  (message "IDE event notifications enabled"))

(defun claude-code-ide-events-disable ()
  "Disable IDE event notifications."
  (interactive)
  (setq claude-code-ide-events-enabled nil)
  ;; Cancel any pending timers
  (when claude-code-ide-events-selection-timer
    (cancel-timer claude-code-ide-events-selection-timer)
    (setq claude-code-ide-events-selection-timer nil))
  ;; Remove hooks
  (remove-hook 'post-command-hook #'claude-code-ide-events-selection-changed)
  (message "IDE event notifications disabled"))

(provide 'claude-code-ide-events)
;;; claude-code-ide-events.el ends here
