;;; claude-code-core.el --- Core functionality for Claude Code Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: DESKTOP2 <yuya373@DESKTOP2>
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

;; Core functionality for Claude Code Emacs including:
;; - Buffer management functions
;; - String processing utilities
;; - Session lifecycle management

;;; Code:

(require 'projectile)
(require 'json)
(require 'vterm)

;; Forward declarations for MCP integration
(declare-function claude-code-mcp-disconnect "claude-code-mcp-connection" (project-root))
(declare-function claude-code-vterm-mode "claude-code-ui" ())

;; Forward declarations for IDE integration
(declare-function claude-code-ide-server-start "claude-code-ide-server" (project-root))
(declare-function claude-code-ide-server-stop "claude-code-ide-server" (project-root))
(declare-function claude-code-ide-events-enable "claude-code-ide-events" ())
(declare-function claude-code-ide-events-disable "claude-code-ide-events" ())

;;; Customization

(defgroup claude-code nil
  "Run Claude Code within Emacs."
  :group 'tools
  :prefix "claude-code-")


(defcustom claude-code-executable "claude"
  "The executable name or path for Claude Code CLI."
  :type 'string
  :group 'claude-code)

(defconst claude-code-available-options
  '(("--verbose" . "Enable detailed logging")
    ("--model sonnet" . "Use Claude Sonnet model")
    ("--model opus" . "Use Claude Opus model")
    ("--resume" . "Resume specific session by ID")
    ("--continue" . "Load latest conversation in current directory")
    ("--dangerously-skip-permissions" . "Skip permission prompts"))
  "Available options for Claude Code CLI.")

;;; Buffer Management

(defun claude-code-normalize-project-root (project-root)
  "Normalize PROJECT-ROOT by removing trailing slash.
Return nil if PROJECT-ROOT is nil."
  (when project-root
    (directory-file-name project-root)))

(defun claude-code-buffer-name ()
  "Return the buffer name for Claude Code session in current project.
Return nil if not in a project."
  (when-let ((project-root (claude-code-normalize-project-root (projectile-project-root))))
    (format "*claude:%s*" project-root)))

(defun claude-code-get-buffer ()
  "Get the Claude Code buffer for the current project, or nil if it doesn't exist."
  (get-buffer (claude-code-buffer-name)))

(defun claude-code-ensure-buffer ()
  "Ensure Claude Code buffer exists, error if not."
  (or (claude-code-get-buffer)
      (error "No Claude Code session for this project.  Use 'claude-code-run' to start one")))

(defun claude-code-with-vterm-buffer (body-fn)
  "Execute BODY-FN in the Claude Code vterm buffer."
  (let ((buf (claude-code-ensure-buffer)))
    (with-current-buffer buf
      (funcall body-fn))))

;;; IDE Lock File Management

(defun claude-code-ide-create-lock-file (port auth-token project-root)
  "Create ~/.claude/ide/<port>.lock with authentication token.
PORT is the WebSocket server port.
AUTH-TOKEN is the UUID for authentication.
PROJECT-ROOT is the project workspace folder."
  (let* ((ide-dir (expand-file-name "~/.claude/ide"))
         (lock-file (expand-file-name (format "%d.lock" port) ide-dir))
         (lock-data `((pid . ,(emacs-pid))
                      (workspaceFolders . (,project-root))
                      (ideName . "Emacs")
                      (transport . "ws")
                      (authToken . ,auth-token))))
    ;; Ensure ~/.claude/ide directory exists with secure permissions (700)
    (unless (file-exists-p ide-dir)
      (make-directory ide-dir t)
      (set-file-modes ide-dir #o700))
    ;; Write lock file directly to avoid with-temp-file issues in tests
    (write-region (json-encode lock-data) nil lock-file nil 'silent)
    (set-file-modes lock-file #o600)
    lock-file))

(defun claude-code-ide-remove-lock-file (port)
  "Remove IDE lock file for PORT if it exists."
  (let ((lock-file (expand-file-name (format "~/.claude/ide/%d.lock" port))))
    (when (file-exists-p lock-file)
      (delete-file lock-file))))

;;; Session Management

;;;###autoload
(defun claude-code-run ()
  "Start Claude Code session for the current project.
With prefix argument, select from available options."
  (interactive)
  (let* ((buffer-name (claude-code-buffer-name))
         (project-root (claude-code-normalize-project-root (projectile-project-root)))
         (default-directory project-root)

         ;; Start IDE WebSocket server FIRST (if available)
         (server-info (condition-case err
                          (claude-code-ide-server-start project-root)
                        (error
                         (message "Failed to start IDE server: %S" err)
                         nil)))
         (ide-port (when server-info (car server-info)))
         (auth-token (when server-info (cdr server-info)))

         ;; Create lock file if IDE server started successfully
         (_ (when (and ide-port auth-token)
              (claude-code-ide-create-lock-file ide-port auth-token project-root)
              ;; Enable IDE event notifications
              (claude-code-ide-events-enable)))

         ;; Set environment variables for IDE integration
         (vterm-environment (if ide-port
                                (append (list (format "CLAUDE_CODE_SSE_PORT=%d" ide-port)
                                              "ENABLE_IDE_INTEGRATION=true")
                                        vterm-environment)
                              (cons "ENABLE_IDE_INTEGRATION=true" vterm-environment)))

         (buf (get-buffer-create buffer-name))
         (selected-option (when current-prefix-arg
                            (let* ((choices (mapcar (lambda (opt)
                                                      (format "%s - %s"
                                                              (car opt)
                                                              (cdr opt)))
                                                    claude-code-available-options))
                                   (selected (completing-read "Select Claude option: " choices nil t)))
                              (when selected
                                (car (split-string selected " - "))))))
         (extra-input (when (and selected-option
                                 (string-match-p "--resume" selected-option))
                        (read-string "Session ID: ")))
         (vterm-shell (concat claude-code-executable
                              (when selected-option
                                (concat " " selected-option))
                              (when extra-input
                                (concat " " extra-input)))))

    ;; Log IDE server status
    (when ide-port
      (message "IDE server started on port %d" ide-port))

    (with-current-buffer buf
      (unless (eq major-mode 'claude-code-vterm-mode)
        (claude-code-vterm-mode)))
    (switch-to-buffer-other-window buffer-name)))

;;;###autoload
(defun claude-code-switch-to-buffer ()
  "Switch to the Claude Code buffer for the current project."
  (interactive)
  (let ((buffer-name (claude-code-buffer-name)))
    (if (get-buffer buffer-name)
        (switch-to-buffer-other-window buffer-name)
      (message "No Claude Code session for this project. Use 'claude-code-run' to start one."))))

;;;###autoload
(defun claude-code-close ()
  "Close the window displaying the Claude Code buffer for the current project."
  (interactive)
  (let* ((buffer-name (claude-code-buffer-name))
         (buffer (get-buffer buffer-name)))
    (if buffer
        (let ((window (get-buffer-window buffer)))
          (if window
              (delete-window window)
            (message "Claude Code buffer is not displayed in any window")))
      (message "No Claude Code buffer found for this project"))))

;;;###autoload
(defun claude-code-quit ()
  "Quit the Claude Code session for the current project and kill the buffer."
  (interactive)
  (let* ((buffer-name (claude-code-buffer-name))
         (buffer (get-buffer buffer-name))
         (project-root (claude-code-normalize-project-root (projectile-project-root))))
    (if buffer
        (progn
          ;; Stop IDE server if it's running
          ;; Disable IDE event notifications
          (claude-code-ide-events-disable)
          (claude-code-ide-server-stop project-root)

          ;; First close any windows showing the buffer
          (dolist (window (get-buffer-window-list buffer nil t))
            (delete-window window))
          ;; Kill the vterm process if it exists
          (with-current-buffer buffer
            (vterm-send-string "/quit")
            (vterm-send-return)
            (run-at-time 3 nil
                         (lambda ()
                           (when (buffer-live-p buffer)
                             ;; Kill vterm process if still running
                             (when (and (boundp 'vterm--process)
                                        vterm--process
                                        (process-live-p vterm--process))
                               (kill-process vterm--process))
                             ;; Kill the buffer
                             (let ((kill-buffer-query-functions nil))
                               (kill-buffer buffer)))
                           (message "Claude Code session ended for this project")))))
      (message "No Claude Code buffer found for this project"))))

;;; String Sending Functions

(defun claude-code-send-string (string &optional paste-p)
  "Send STRING to the Claude Code session."
  (interactive "sEnter text: ")
  (claude-code-with-vterm-buffer
   (lambda ()
     (vterm-send-string string paste-p)
     ;; NOTE: wait for `accept-process-output' in `vterm-send-string'
     (sit-for (* vterm-timer-delay 3))
     (vterm-send-return))))

;;;###autoload
(defun claude-code-send-region ()
  "Send selected region to Claude Code."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (claude-code-send-string text))
    (user-error "No region selected")))

(provide 'claude-code-core)
;;; claude-code-core.el ends here
