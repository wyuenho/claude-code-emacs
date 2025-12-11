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

(require 'json)
(require 'cl-lib)
(require 'websocket)
(require 'claude-code-base)
(require 'claude-code-ide-tools)

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

(defun claude-code-ide-server-filter (process output)
  "Custom filter for IDE WebSocket server that validates auth token.
Largely copied from `websocket-server-filter', but adds auth validation
before processing headers."
  (let* ((ws (process-get process :websocket))
         (text (concat (websocket-inflight-input ws) output)))
    (setf (websocket-inflight-input ws) nil)
    (cond ((eq (websocket-ready-state ws) 'connecting)
           ;; check for connection string
           (let ((end-of-header-pos
                  (let ((pos (string-match "\r\n\r\n" text)))
                    (when pos (+ 4 pos)))))
             (if end-of-header-pos
                 (progn
                   ;; CUSTOM: Extract and validate auth token BEFORE header verification
                   (let ((case-fold-search t)
                         ;; Get auth token from server process (not client process)
                         (server (websocket-server-conn ws))
                         (expected-token (process-get (websocket-server-conn ws) :claude-code-auth-token))
                         (auth-valid nil))
                     ;; Check if auth header is present and matches
                     (if (string-match "^x-claude-code-ide-authorization: \\(.+\\)\r\n" text)
                         (let ((client-token (match-string 1 text)))
                           (if (string= client-token expected-token)
                               (setq auth-valid t)
                             (message "IDE connection rejected: auth token mismatch")))
                       (message "IDE connection rejected: missing auth header"))

                     (if auth-valid
                         ;; Auth valid - proceed with normal WebSocket handshake
                         (let ((header-info (websocket-verify-client-headers text)))
                           (if header-info
                               (progn (setf (websocket-accept-string ws)
                                            (websocket-calculate-accept
                                             (plist-get header-info :key)))
                                      (process-send-string
                                       process
                                       (websocket-get-server-response
                                        ws (plist-get header-info :protocols)
                                        (plist-get header-info :extensions)))
                                      (setf (websocket-ready-state ws) 'open)
                                      (setf (websocket-origin ws) (plist-get header-info :origin))
                                      (websocket-try-callback 'websocket-on-open
                                                              'on-open ws))
                             (message "Invalid client headers found in: %s" output)
                             (process-send-string process "HTTP/1.1 400 Bad Request\r\n\r\n")
                             (websocket-close ws)))
                       ;; Auth invalid - send 401 and close
                       (process-send-string process "HTTP/1.1 401 Unauthorized\r\n\r\n")
                       (websocket-close ws)))
                   (when (> (length text) (+ 1 end-of-header-pos))
                     (claude-code-ide-server-filter process (substring
                                                              text
                                                              end-of-header-pos))))
               (setf (websocket-inflight-input ws) text))))
          ((eq (websocket-ready-state ws) 'open)
           (websocket-process-input-on-open-ws ws text))
          ((eq (websocket-ready-state ws) 'closed)
           (message "WARNING: Should not have received further input on closed websocket")))))

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
  (let* ((auth-token (claude-code-ide-generate-uuid))
         (server-info (list (cons 'auth-token auth-token)
                           (cons 'project-root project-root)
                           (cons 'websocket nil)
                           (cons 'server nil)
                           (cons 'port nil)))
         (server nil))

    ;; Create WebSocket server using make-network-process directly
    ;; This allows us to use a custom filter for auth validation
    (setq server
          (make-network-process
           :name (format "IDE websocket server on port %d" 0)
           :server t
           :family 'ipv4
           :noquery t
           :filter 'claude-code-ide-server-filter
           :log 'websocket-server-accept
           :filter-multibyte nil
           :plist (list :on-open
                        (lambda (websocket)
                          (message "Claude Code connected to IDE server (authenticated)")
                          ;; Store the websocket connection
                          (setcdr (assoc 'websocket server-info) websocket))
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
                          (message "IDE WebSocket error: %s - %S" type error))
                        :claude-code-auth-token auth-token)
           :host 'local
           :service 0))

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
