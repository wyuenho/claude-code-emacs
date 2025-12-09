;;; claude-code-ide-test.el --- Tests for IDE protocol integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Keywords: test

;;; Commentary:

;; Test suite for claude-code IDE protocol integration

;;; Code:

(require 'ert)
(require 'claude-code-core)
(require 'claude-code-ide-server)
(require 'claude-code-ide-tools)
(require 'json)

;;; Test utilities

(defmacro with-temp-ide-dir (&rest body)
  "Execute BODY with a temporary IDE directory."
  `(let* ((original-home (getenv "HOME"))
          (temp-home (make-temp-file "claude-ide-test-" t)))
     (unwind-protect
         (progn
           (setenv "HOME" temp-home)
           ,@body)
       (setenv "HOME" original-home)
       (delete-directory temp-home t))))

;;; Lock File Tests

(ert-deftest test-ide-lock-file-creation ()
  "Test IDE lock file creation with proper structure."
  (with-temp-ide-dir
    (let* ((port 12345)
           (auth-token "test-token-uuid")
           (project-root "/test/project")
           (lock-file (claude-code-ide-create-lock-file port auth-token project-root)))

      ;; Verify lock file exists
      (should (file-exists-p lock-file))

      ;; Verify lock file path
      (should (string-match-p "12345\\.lock$" lock-file))

      ;; Verify lock file contents
      (let ((lock-data (json-read-file lock-file)))
        (should (= (cdr (assoc 'pid lock-data)) (emacs-pid)))
        ;; workspaceFolders is encoded as an array by json-encode
        (should (equal (cdr (assoc 'workspaceFolders lock-data)) (vector project-root)))
        (should (string= (cdr (assoc 'ideName lock-data)) "Emacs"))
        (should (string= (cdr (assoc 'transport lock-data)) "ws"))
        (should (string= (cdr (assoc 'authToken lock-data)) auth-token))))))

(ert-deftest test-ide-lock-file-permissions ()
  "Test IDE lock file has secure permissions."
  (with-temp-ide-dir
    (let* ((port 23456)
           (auth-token "test-token")
           (project-root "/test/project")
           (lock-file (claude-code-ide-create-lock-file port auth-token project-root)))

      ;; Verify file permissions are 600 (readable/writable by owner only)
      (let* ((attrs (file-attributes lock-file))
             (mode (file-modes lock-file))
             (perms (logand mode #o777)))
        (should (= perms #o600))))))

(ert-deftest test-ide-directory-creation ()
  "Test IDE directory is created with secure permissions."
  (with-temp-ide-dir
    (let* ((ide-dir (expand-file-name "~/.claude/ide"))
           (port 34567)
           (auth-token "test-token")
           (project-root "/test/project"))

      ;; Directory shouldn't exist initially
      (should-not (file-exists-p ide-dir))

      ;; Create lock file (which creates directory)
      (claude-code-ide-create-lock-file port auth-token project-root)

      ;; Verify directory exists
      (should (file-exists-p ide-dir))
      (should (file-directory-p ide-dir))

      ;; Verify directory permissions are 700
      (let ((perms (logand (file-modes ide-dir) #o777)))
        (should (= perms #o700))))))

(ert-deftest test-ide-lock-file-removal ()
  "Test IDE lock file removal."
  (with-temp-ide-dir
    (let* ((port 45678)
           (auth-token "test-token")
           (project-root "/test/project")
           (lock-file (claude-code-ide-create-lock-file port auth-token project-root)))

      ;; Verify lock file exists
      (should (file-exists-p lock-file))

      ;; Remove lock file
      (claude-code-ide-remove-lock-file port)

      ;; Verify lock file is removed
      (should-not (file-exists-p lock-file)))))

(ert-deftest test-ide-lock-file-removal-nonexistent ()
  "Test removing non-existent lock file doesn't error."
  (with-temp-ide-dir
    ;; Should not throw error
    (should-not (claude-code-ide-remove-lock-file 99999))))

;;; UUID Generation Tests

(ert-deftest test-ide-uuid-generation ()
  "Test UUID generation produces valid format."
  (let ((uuid (claude-code-ide-generate-uuid)))
    ;; UUID format: 8-4-4-4-12 hex digits
    (should (string-match-p
             "^[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}$"
             uuid))))

(ert-deftest test-ide-uuid-uniqueness ()
  "Test generated UUIDs are unique."
  (let ((uuid1 (claude-code-ide-generate-uuid))
        (uuid2 (claude-code-ide-generate-uuid))
        (uuid3 (claude-code-ide-generate-uuid)))
    (should-not (string= uuid1 uuid2))
    (should-not (string= uuid1 uuid3))
    (should-not (string= uuid2 uuid3))))

;;; Authentication Tests

(ert-deftest test-ide-auth-header-validation-success ()
  "Test successful auth header validation."
  (let* ((auth-token "550e8400-e29b-41d4-a716-446655440000")
         (headers '(("x-claude-code-ide-authorization" . "550e8400-e29b-41d4-a716-446655440000")
                    ("host" . "localhost"))))
    (should (claude-code-ide-validate-auth-header headers auth-token))))

(ert-deftest test-ide-auth-header-validation-failure ()
  "Test auth header validation fails with wrong token."
  (let* ((auth-token "550e8400-e29b-41d4-a716-446655440000")
         (headers '(("x-claude-code-ide-authorization" . "wrong-token-here")
                    ("host" . "localhost"))))
    (should-not (claude-code-ide-validate-auth-header headers auth-token))))

(ert-deftest test-ide-auth-header-validation-missing ()
  "Test auth header validation fails when header is missing."
  (let* ((auth-token "550e8400-e29b-41d4-a716-446655440000")
         (headers '(("host" . "localhost"))))
    (should-not (claude-code-ide-validate-auth-header headers auth-token))))

(ert-deftest test-ide-auth-header-case-sensitive ()
  "Test auth header validation is case-sensitive for token."
  (let* ((auth-token "550e8400-e29b-41d4-a716-446655440000")
         (headers '(("x-claude-code-ide-authorization" . "550E8400-E29B-41D4-A716-446655440000"))))
    (should-not (claude-code-ide-validate-auth-header headers auth-token))))

;;; Tool Dispatching Tests

(ert-deftest test-ide-tool-dispatch-openFile ()
  "Test tool dispatch for openFile."
  (let* ((params '((path . "test.el")))
         (result (claude-code-ide-tools-dispatch "openFile" params)))
    ;; Should return some result (success or error)
    (should result)))

(ert-deftest test-ide-tool-dispatch-getCurrentSelection ()
  "Test tool dispatch for getCurrentSelection."
  (let* ((params '())
         (result (claude-code-ide-tools-dispatch "getCurrentSelection" params)))
    ;; Should return selection data structure
    (should (assoc 'text result))
    (should (assoc 'startLine result))
    (should (assoc 'endLine result))))

(ert-deftest test-ide-tool-dispatch-getWorkspaceFolders ()
  "Test tool dispatch for getWorkspaceFolders."
  (let* ((params '())
         (result (claude-code-ide-tools-dispatch "getWorkspaceFolders" params)))
    ;; Should return folders array
    (should (assoc 'folders result))))

(ert-deftest test-ide-tool-dispatch-unknown-method ()
  "Test tool dispatch for unknown method returns error."
  (let* ((params '())
         (result (claude-code-ide-tools-dispatch "unknownMethod" params)))
    ;; Should return error cons
    (should (eq (car result) 'error))
    (should (stringp (cdr result)))
    (should (string-match-p "Unknown method" (cdr result)))))

(ert-deftest test-ide-tool-dispatch-all-methods ()
  "Test all IDE protocol methods are registered."
  (let ((methods '("openFile" "openDiff" "getCurrentSelection" "getLatestSelection"
                   "getOpenEditors" "getWorkspaceFolders" "getDiagnostics"
                   "checkDocumentDirty" "saveDocument" "close_tab"
                   "closeAllDiffTabs" "executeCode")))
    (dolist (method methods)
      (let ((result (claude-code-ide-tools-dispatch method '())))
        ;; Should not return unknown method error
        (should-not (and (consp result)
                        (eq (car result) 'error)
                        (string-match-p "Unknown method" (cdr result))))))))

;;; WebSocket Server Tests (with mocks)

(ert-deftest test-ide-server-info-storage ()
  "Test server info is stored correctly."
  (let ((claude-code-ide-servers (make-hash-table :test 'equal))
        (project-root "/test/project"))

    ;; Initially empty
    (should-not (gethash project-root claude-code-ide-servers))

    ;; Mock the server startup to just store info
    (puthash project-root
             (list (cons 'port 12345)
                   (cons 'auth-token "test-token")
                   (cons 'websocket nil)
                   (cons 'server nil))
             claude-code-ide-servers)

    ;; Verify storage
    (let ((info (gethash project-root claude-code-ide-servers)))
      (should info)
      (should (= (cdr (assoc 'port info)) 12345))
      (should (string= (cdr (assoc 'auth-token info)) "test-token")))))

(ert-deftest test-ide-server-cleanup ()
  "Test server cleanup removes from hash table."
  (let ((claude-code-ide-servers (make-hash-table :test 'equal))
        (project-root "/test/project"))

    ;; Store server info
    (puthash project-root
             (list (cons 'port 12345)
                   (cons 'auth-token "test-token"))
             claude-code-ide-servers)

    (should (gethash project-root claude-code-ide-servers))

    ;; Cleanup (remhash)
    (remhash project-root claude-code-ide-servers)

    (should-not (gethash project-root claude-code-ide-servers))))

;;; JSON-RPC Message Handling Tests

(ert-deftest test-ide-dispatch-message-request ()
  "Test JSON-RPC request message dispatching."
  (let* ((message '((jsonrpc . "2.0")
                    (id . 1)
                    (method . "getWorkspaceFolders")
                    (params)))
         (websocket-sent nil)
         (websocket-text nil))

    ;; Mock websocket-send-text
    (cl-letf (((symbol-function 'websocket-send-text)
               (lambda (ws text)
                 (setq websocket-sent t)
                 (setq websocket-text text))))

      ;; Dispatch the message
      (claude-code-ide-dispatch-message 'mock-websocket message "/test/project")

      ;; Verify response was sent
      (should websocket-sent)
      (should websocket-text)

      ;; Verify response structure
      (let ((response (json-read-from-string websocket-text)))
        (should (string= (cdr (assoc 'jsonrpc response)) "2.0"))
        (should (= (cdr (assoc 'id response)) 1))
        (should (assoc 'result response))))))

(ert-deftest test-ide-dispatch-message-notification ()
  "Test JSON-RPC notification message (no id)."
  (let* ((message '((jsonrpc . "2.0")
                    (method . "someNotification")
                    (params)))
         (websocket-sent nil))

    ;; Mock websocket-send-text
    (cl-letf (((symbol-function 'websocket-send-text)
               (lambda (ws text)
                 (setq websocket-sent t))))

      ;; Dispatch the notification
      (claude-code-ide-dispatch-message 'mock-websocket message "/test/project")

      ;; Notifications don't send responses
      (should-not websocket-sent))))

(ert-deftest test-ide-send-notification ()
  "Test sending notification to Claude Code."
  (let ((claude-code-ide-servers (make-hash-table :test 'equal))
        (project-root "/test/project")
        (websocket-sent nil)
        (websocket-text nil)
        (mock-websocket 'mock-ws))

    ;; Store server info with websocket
    (puthash project-root
             (list (cons 'websocket mock-websocket))
             claude-code-ide-servers)

    ;; Mock websocket-send-text
    (cl-letf (((symbol-function 'websocket-send-text)
               (lambda (ws text)
                 (setq websocket-sent t)
                 (setq websocket-text text))))

      ;; Send notification
      (claude-code-ide-send-notification project-root "test/event" '((data . "test")))

      ;; Verify notification was sent
      (should websocket-sent)
      (should websocket-text)

      ;; Verify notification structure
      (let ((notification (json-read-from-string websocket-text)))
        (should (string= (cdr (assoc 'jsonrpc notification)) "2.0"))
        (should (string= (cdr (assoc 'method notification)) "test/event"))
        (should (assoc 'params notification))
        (should-not (assoc 'id notification))))))

(ert-deftest test-ide-send-notification-no-websocket ()
  "Test sending notification when no websocket connected."
  (let ((claude-code-ide-servers (make-hash-table :test 'equal))
        (project-root "/test/project")
        (websocket-sent nil))

    ;; Store server info without websocket
    (puthash project-root
             (list (cons 'websocket nil))
             claude-code-ide-servers)

    ;; Mock websocket-send-text
    (cl-letf (((symbol-function 'websocket-send-text)
               (lambda (ws text)
                 (setq websocket-sent t))))

      ;; Try to send notification
      (claude-code-ide-send-notification project-root "test/event" '((data . "test")))

      ;; Should not send anything
      (should-not websocket-sent))))

(provide 'claude-code-ide-test)
;;; claude-code-ide-test.el ends here
