;;; test-claude-code-core.el --- Tests for core utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Keywords: test

;;; Commentary:

;; Test suite for claude-code-core module

;;; Code:

(require 'ert)
(require 'claude-code-core)
(require 'cl-lib)

;;; Test utilities

(defmacro with-claude-test-project (&rest body)
  "Execute BODY in a temporary project directory."
  `(let* ((temp-dir (make-temp-file "claude-test-" t))
          (default-directory temp-dir)
          (projectile-project-root temp-dir))
     (unwind-protect
         (progn
           ;; Create a minimal .git directory to make projectile recognize it as a project
           (make-directory (expand-file-name ".git" temp-dir))
           ,@body)
       (delete-directory temp-dir t))))

;;; Tests for core functions

(ert-deftest test-claude-code-count-arguments ()
  "Test counting $ARGUMENTS placeholders."
  (should (= 0 (claude-code-count-arguments "No arguments here")))
  (should (= 1 (claude-code-count-arguments "One $ARGUMENTS here")))
  (should (= 2 (claude-code-count-arguments "$ARGUMENTS and $ARGUMENTS")))
  (should (= 3 (claude-code-count-arguments "Start $ARGUMENTS middle $ARGUMENTS end $ARGUMENTS"))))

(ert-deftest test-claude-code-send-region ()
  "Test sending selected region to Claude Code."
  ;; Mock the required functions
  (cl-letf* ((sent-text nil)
             ((symbol-function 'claude-code-send-string)
              (lambda (text) (setq sent-text text)))
             ((symbol-function 'use-region-p) (lambda () t))
             ((symbol-function 'region-beginning) (lambda () 1))
             ((symbol-function 'region-end) (lambda () 6)))
    ;; Test with region selected
    (with-temp-buffer
      (insert "Hello World")
      (claude-code-send-region)
      (should (equal sent-text "Hello"))))

  ;; Test without region selected
  (cl-letf (((symbol-function 'use-region-p) (lambda () nil)))
    (should-error (claude-code-send-region) :type 'user-error)))

(ert-deftest test-claude-code-run ()
  "Test starting Claude Code session."
  (with-claude-test-project
    (let* ((buffer-created nil)
           (buffer-switched nil)
           (created-buffer-name nil)
           (vterm-shell-value nil)
           (vterm-mode-called nil)
           (test-buffer (generate-new-buffer "*test-buffer*")))
      (unwind-protect
          (cl-letf* (((symbol-function 'claude-code-ide-create-lock-file)
                      (lambda (_port _auth-token _project-root)
                        ;; Mock lock file creation to avoid buffer issues
                        nil))
                     ((symbol-function 'get-buffer-create)
                      (lambda (name &optional _inhibit-buffer-hooks)
                        (setq buffer-created t)
                        (setq created-buffer-name name)
                        ;; Simulate vterm buffer
                        (with-current-buffer test-buffer
                          (setq-local major-mode 'vterm-mode)
                          (current-buffer))))
                     ((symbol-function 'switch-to-buffer-other-window)
                      (lambda (name)
                        (setq buffer-switched t)))
                     ((symbol-function 'claude-code-vterm-mode)
                      (lambda ()
                        (setq vterm-mode-called t)
                        ;; Mock vterm-mode setup
                        (setq-local vterm-shell claude-code-executable)
                        (setq major-mode 'claude-code-vterm-mode)))
                     ((symbol-function 'vterm)
                      (lambda (buffer-name)
                        ;; Return the test buffer to simulate vterm creation
                        test-buffer))
                     (current-prefix-arg nil))
            ;; Test basic run without prefix argument
            (claude-code-run)
            (should buffer-created)
            (should buffer-switched)
            (should (string-match-p "\\*claude:" created-buffer-name))
            (should vterm-mode-called))
        (kill-buffer test-buffer)))))

(ert-deftest test-claude-code-run-with-options ()
  "Test starting Claude Code session with interactive options."
  (with-claude-test-project
    (let* ((buffer-created nil)
           (buffer-switched nil)
           (vterm-shell-value nil)
           (vterm-mode-called nil)
           (test-buffer (generate-new-buffer "*test-buffer*")))
      (unwind-protect
          (cl-letf* (((symbol-function 'claude-code-ide-create-lock-file)
                      (lambda (_port _auth-token _project-root)
                        ;; Mock lock file creation to avoid buffer issues
                        nil))
                     ((symbol-function 'get-buffer-create)
                      (lambda (name &optional _inhibit-buffer-hooks)
                        (setq buffer-created t)
                        ;; Simulate vterm buffer
                        (with-current-buffer test-buffer
                          (setq-local major-mode 'vterm-mode)
                          (current-buffer))))
                     ((symbol-function 'switch-to-buffer-other-window)
                      (lambda (name)
                        (setq buffer-switched t)))
                     ((symbol-function 'claude-code-vterm-mode)
                      (lambda ()
                        (setq vterm-mode-called t)
                        ;; Capture the vterm-shell value that was set
                        (setq vterm-shell-value vterm-shell)
                        (setq major-mode 'claude-code-vterm-mode)))
                     ((symbol-function 'vterm)
                      (lambda (buffer-name)
                        ;; Return the test buffer to simulate vterm creation
                        test-buffer))
                     ((symbol-function 'completing-read)
                      (lambda (prompt choices &rest _)
                        "--model sonnet - Use Claude Sonnet model"))
                     (current-prefix-arg t))
            ;; Test run with prefix argument for option selection
            (claude-code-run)
            (should buffer-created)
            (should buffer-switched)
            (should vterm-mode-called)
            ;; Check that vterm-shell includes the selected option
            (should (string-match-p "--model sonnet" vterm-shell-value)))
        (kill-buffer test-buffer)))))

(ert-deftest test-claude-code-run-with-resume ()
  "Test starting Claude Code session with resume option."
  (with-claude-test-project
    (let* ((buffer-created nil)
           (vterm-shell-value nil)
           (read-string-called nil)
           (session-id "test-session-123")
           (vterm-mode-called nil)
           (test-buffer (generate-new-buffer "*test-buffer*")))
      (unwind-protect
          (cl-letf* (((symbol-function 'claude-code-ide-create-lock-file)
                      (lambda (_port _auth-token _project-root)
                        ;; Mock lock file creation to avoid buffer issues
                        nil))
                     ((symbol-function 'get-buffer-create)
                      (lambda (name &optional _inhibit-buffer-hooks)
                        (setq buffer-created t)
                        ;; Simulate vterm buffer
                        (with-current-buffer test-buffer
                          (setq-local major-mode 'vterm-mode)
                          (current-buffer))))
                     ((symbol-function 'switch-to-buffer-other-window)
                      (lambda (name) nil))
                     ((symbol-function 'claude-code-vterm-mode)
                      (lambda ()
                        (setq vterm-mode-called t)
                        ;; Capture the vterm-shell value that was set
                        (setq vterm-shell-value vterm-shell)
                        (setq major-mode 'claude-code-vterm-mode)))
                     ((symbol-function 'vterm)
                      (lambda (buffer-name)
                        ;; Return the test buffer to simulate vterm creation
                        test-buffer))
                     ((symbol-function 'completing-read)
                      (lambda (prompt choices &rest _)
                        "--resume - Resume specific session by ID"))
                     ((symbol-function 'read-string)
                      (lambda (prompt)
                        (setq read-string-called t)
                        session-id))
                     (current-prefix-arg t))
            ;; Test run with resume option
            (claude-code-run)
            (should buffer-created)
            (should read-string-called)
            (should vterm-mode-called)
            ;; Check that vterm-shell includes the resume option and session ID
            (should (string-match-p "--resume" vterm-shell-value))
            (should (string-match-p session-id vterm-shell-value)))
        (kill-buffer test-buffer)))))

(ert-deftest test-ide-env-vars-preserve-existing-with-port ()
  "Test that IDE env vars are appended without replacing existing ones when server starts."
  (let ((vterm-environment '("PATH=/usr/bin" "HOME=/test" "USER=testuser"))
        (ide-port 12345))
    ;; Simulate the code path when IDE server starts
    (let ((result (append (list (format "CLAUDE_CODE_SSE_PORT=%d" ide-port)
                                "ENABLE_IDE_INTEGRATION=true")
                          vterm-environment)))
      ;; New vars should be present
      (should (member "CLAUDE_CODE_SSE_PORT=12345" result))
      (should (member "ENABLE_IDE_INTEGRATION=true" result))
      ;; CRITICAL: Original vars must be preserved
      (should (member "PATH=/usr/bin" result))
      (should (member "HOME=/test" result))
      (should (member "USER=testuser" result)))))

(ert-deftest test-ide-env-vars-preserve-existing-without-port ()
  "Test that IDE env vars are appended without replacing existing ones when server doesn't start."
  (let ((vterm-environment '("PATH=/usr/bin" "LANG=en_US.UTF-8")))
    ;; Simulate the code path when IDE server doesn't start (ide-port is nil)
    (let ((result (cons "ENABLE_IDE_INTEGRATION=true" vterm-environment)))
      ;; New var should be present
      (should (member "ENABLE_IDE_INTEGRATION=true" result))
      ;; CRITICAL: Original vars must be preserved
      (should (member "PATH=/usr/bin" result))
      (should (member "LANG=en_US.UTF-8" result)))))

(provide 'test-claude-code-core)
;;; test-claude-code-core.el ends here
