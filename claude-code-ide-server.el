;;; claude-code-ide-server.el --- IDE WebSocket server for Claude Code -*- lexical-binding: t; -*-

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

;; This module implements the IDE WebSocket server for Claude Code integration.
;; It provides:
;; - WebSocket server with authentication
;; - JSON-RPC 2.0 message handling
;; - Tool call dispatch
;; - Notification sending to Claude Code

;;; Code:

(require 'websocket nil t)
(require 'json)
(require 'cl-lib)

;; Declare websocket functions
(declare-function websocket-server "websocket" (port &rest plist))
(declare-function websocket-send-text "websocket" (websocket text))
(declare-function websocket-close "websocket" (websocket))
(declare-function websocket-server-conn-headers "websocket" (websocket))
(declare-function websocket-frame-text "websocket" (frame))

;; Forward declarations for tool handlers
(declare-function claude-code-ide-tools-dispatch "claude-code-ide-tools" (method params))

;;; Variables

(defvar claude-code-ide-servers (make-hash-table :test 'equal)
  "Hash table mapping project roots to IDE server info.
Each value is an alist with keys:
  - server: The WebSocket server instance
  - port: Server port number
  - auth-token: Authentication token (UUID)
  - websocket: The connected client WebSocket
  - project-root: Project root directory")

;;; Helper Functions

(defun claude-code-ide-generate-uuid ()
  "Generate a random UUID for authentication."
  (format "%04x%04x-%04x-%04x-%04x-%04x%04x%04x"
          (random 65536)
          (random 65536)
          (random 65536)
          (logior (logand (random 65536) #x0fff) #x4000)
          (logior (logand (random 65536) #x3fff) #x8000)
          (random 65536)
          (random 65536)
          (random 65536)))

(defun claude-code-ide-validate-auth-header (headers auth-token)
  "Validate authentication header in HEADERS matches AUTH-TOKEN.
Returns non-nil if valid, nil otherwise."
  (when-let ((auth-header (cdr (assoc "x-claude-code-ide-authorization" headers))))
    (string= auth-header auth-token)))

;;; JSON-RPC Message Handling

(defun claude-code-ide-handle-message (websocket frame project-root)
  "Handle JSON-RPC message from WEBSOCKET.
FRAME contains the message data.
PROJECT-ROOT is the project directory."
  (condition-case err
      (let* ((text (websocket-frame-text frame))
             (message (json-read-from-string text)))
        (claude-code-ide-dispatch-message websocket message project-root))
    (error
     (message "Error handling IDE message: %S" err))))

(defun claude-code-ide-dispatch-message (websocket message project-root)
  "Dispatch JSON-RPC MESSAGE from WEBSOCKET for PROJECT-ROOT."
  (let ((method (cdr (assoc 'method message)))
        (params (cdr (assoc 'params message)))
        (id (cdr (assoc 'id message))))
    (cond
     ;; Request (has id) - this is a tool call from Claude
     (id
      (let* ((result (claude-code-ide-tools-dispatch method params))
             (response (if (eq (car result) 'error)
                          `((jsonrpc . "2.0")
                            (id . ,id)
                            (error . ((code . -32603)
                                     (message . ,(cdr result)))))
                        `((jsonrpc . "2.0")
                          (id . ,id)
                          (result . ,result)))))
        (websocket-send-text websocket (json-encode response))))

     ;; Notification (no id) - we don't expect these from Claude but handle gracefully
     (t
      (message "Received notification from Claude: %s" method)))))

(defun claude-code-ide-send-notification (project-root method params)
  "Send notification to Claude Code for PROJECT-ROOT.
METHOD is the notification method name.
PARAMS is the notification parameters."
  (when-let* ((server-info (gethash project-root claude-code-ide-servers))
              (websocket (cdr (assoc 'websocket server-info))))
    (when websocket
      (let ((notification `((jsonrpc . "2.0")
                           (method . ,method)
                           (params . ,params))))
        (websocket-send-text websocket (json-encode notification))))))

;;; WebSocket Server Management

(defun claude-code-ide-server-start (project-root)
  "Start IDE WebSocket server for PROJECT-ROOT.
Returns a cons cell (PORT . AUTH-TOKEN)."
  (unless (featurep 'websocket)
    (error "websocket.el is required but not available"))

  (let* ((auth-token (claude-code-ide-generate-uuid))
         (server-info (list (cons 'auth-token auth-token)
                           (cons 'project-root project-root)
                           (cons 'websocket nil)
                           (cons 'server nil)
                           (cons 'port nil)))
         (server nil))

    ;; Create WebSocket server on port 0 (OS assigns)
    (setq server
          (websocket-server
           0
           :host 'local
           :on-open
           (lambda (websocket)
             (let ((headers (websocket-server-conn-headers websocket)))
               ;; Validate authentication header
               (if (claude-code-ide-validate-auth-header headers auth-token)
                   (progn
                     (message "Claude Code connected to IDE server (authenticated)")
                     ;; Store the websocket connection
                     (setcdr (assoc 'websocket server-info) websocket))
                 (progn
                   (message "Claude Code connection rejected: invalid auth token")
                   (websocket-close websocket)))))

           :on-message
           (lambda (websocket frame)
             (claude-code-ide-handle-message websocket frame project-root))

           :on-close
           (lambda (_websocket)
             (message "Claude Code disconnected from IDE server")
             ;; Clear the websocket reference
             (setcdr (assoc 'websocket server-info) nil))

           :on-error
           (lambda (_websocket type error)
             (message "IDE WebSocket error: %s - %S" type error))))

    ;; Get the assigned port from the server process
    ;; websocket-server returns a process, not a websocket
    (let ((port (process-contact server :service)))
      (setcdr (assoc 'server server-info) server)
      (setcdr (assoc 'port server-info) port)

      ;; Store server info
      (puthash project-root server-info claude-code-ide-servers)

      (message "IDE WebSocket server started on port %d" port)

      ;; Return (port . auth-token) cons
      (cons port auth-token))))

(defun claude-code-ide-server-stop (project-root)
  "Stop IDE WebSocket server for PROJECT-ROOT."
  (when-let ((server-info (gethash project-root claude-code-ide-servers)))
    (when-let ((server (cdr (assoc 'server server-info))))
      ;; websocket-server returns a process, so close it as a process
      (when (process-live-p server)
        (delete-process server)))
    (when-let ((port (cdr (assoc 'port server-info))))
      (claude-code-ide-remove-lock-file port))
    (remhash project-root claude-code-ide-servers)
    (message "IDE WebSocket server stopped for %s" project-root)))

(provide 'claude-code-ide-server)
;;; claude-code-ide-server.el ends here
