;;; claude-code-ide-events-test.el --- Tests for IDE events -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Tests for claude-code-ide-events.el

;;; Code:

(require 'ert)
(require 'claude-code-ide-events)
(require 'cl-lib)

;;; Test selection_changed notification

(ert-deftest test-ide-events-selection-changed-with-selection ()
  "Test selection_changed notification with active selection."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\n")
    (setq buffer-file-name "/test/file.el")
    (goto-char (point-min))
    (set-mark (point))
    (forward-line 2)
    (activate-mark)

    (let ((sent-notification nil))
      (cl-letf (((symbol-function 'projectile-project-root)
                 (lambda () "/test/"))
                ((symbol-function 'claude-code-ide-send-notification)
                 (lambda (project method params)
                   (setq sent-notification (list project method params)))))

        ;; Send notification
        (claude-code-ide-events-send-selection-changed)

        ;; Verify notification was sent
        (should sent-notification)
        (should (equal (nth 0 sent-notification) "/test"))
        (should (equal (nth 1 sent-notification) "selection_changed"))

        ;; Verify params
        (let ((params (nth 2 sent-notification)))
          (should (equal (cdr (assoc 'filePath params)) "/test/file.el"))
          (should (string-prefix-p "file://" (cdr (assoc 'fileUrl params))))
          (should (equal (cdr (assoc 'isEmpty params)) json-false))
          (should (stringp (cdr (assoc 'text params)))))))))

(ert-deftest test-ide-events-selection-changed-no-selection ()
  "Test selection_changed notification without selection (cursor only)."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\n")
    (setq buffer-file-name "/test/file.el")
    (goto-char (point-min))

    (let ((sent-notification nil))
      (cl-letf (((symbol-function 'projectile-project-root)
                 (lambda () "/test/"))
                ((symbol-function 'claude-code-ide-send-notification)
                 (lambda (project method params)
                   (setq sent-notification (list project method params)))))

        ;; Send notification
        (claude-code-ide-events-send-selection-changed)

        ;; Verify notification was sent
        (should sent-notification)
        (should (equal (nth 1 sent-notification) "selection_changed"))

        ;; Verify isEmpty is true and text is empty
        (let ((params (nth 2 sent-notification)))
          (should (equal (cdr (assoc 'isEmpty params)) t))
          (should (equal (cdr (assoc 'text params)) "")))))))

(ert-deftest test-ide-events-selection-changed-no-file ()
  "Test selection_changed notification skips buffers without files."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\n")
    ;; No buffer-file-name set

    (let ((sent-notification nil))
      (cl-letf (((symbol-function 'projectile-project-root)
                 (lambda () "/test/"))
                ((symbol-function 'claude-code-ide-send-notification)
                 (lambda (project method params)
                   (setq sent-notification (list project method params)))))

        ;; Send notification
        (claude-code-ide-events-send-selection-changed)

        ;; Verify notification was NOT sent
        (should-not sent-notification)))))

;;; Test at_mentioned notification

(ert-deftest test-ide-send-at-mention ()
  "Test at_mentioned notification."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\n")
    (setq buffer-file-name "/test/file.el")
    (goto-char (point-min))
    (set-mark (point))
    (forward-line 2)
    (activate-mark)

    (let ((sent-notification nil))
      (cl-letf (((symbol-function 'projectile-project-root)
                 (lambda () "/test/"))
                ((symbol-function 'claude-code-ide-send-notification)
                 (lambda (project method params)
                   (setq sent-notification (list project method params)))))

        ;; Send at-mention
        (claude-code-ide-send-at-mention)

        ;; Verify notification was sent
        (should sent-notification)
        (should (equal (nth 0 sent-notification) "/test"))
        (should (equal (nth 1 sent-notification) "at_mentioned"))

        ;; Verify params
        (let ((params (nth 2 sent-notification)))
          (should (equal (cdr (assoc 'filePath params)) "/test/file.el"))
          (should (equal (cdr (assoc 'lineStart params)) 1))
          (should (equal (cdr (assoc 'lineEnd params)) 3)))))))

(ert-deftest test-ide-send-at-mention-no-selection ()
  "Test at_mentioned fails without selection."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\n")
    (setq buffer-file-name "/test/file.el")
    (goto-char (point-min))
    ;; No selection

    (let ((sent-notification nil)
          (message-log nil))
      (cl-letf (((symbol-function 'projectile-project-root)
                 (lambda () "/test/"))
                ((symbol-function 'claude-code-ide-send-notification)
                 (lambda (project method params)
                   (setq sent-notification (list project method params))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-log (apply #'format fmt args)))))

        ;; Try to send at-mention without selection
        (claude-code-ide-send-at-mention)

        ;; Verify notification was NOT sent
        (should-not sent-notification)
        ;; Verify error message
        (should (string-match-p "No active selection" message-log))))))

;;; Test event management

(ert-deftest test-ide-events-enable-disable ()
  "Test enabling and disabling IDE events."
  ;; Enable events
  (claude-code-ide-events-enable)
  (should claude-code-ide-events-enabled)
  (should (memq 'claude-code-ide-events-selection-changed post-command-hook))

  ;; Disable events
  (claude-code-ide-events-disable)
  (should-not claude-code-ide-events-enabled)
  (should-not (memq 'claude-code-ide-events-selection-changed post-command-hook)))

(ert-deftest test-ide-events-debouncing ()
  "Test that selection changes are debounced."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\n")
    (setq buffer-file-name "/test/file.el")

    (let ((notification-count 0)
          (claude-code-ide-events-enabled t))
      (cl-letf (((symbol-function 'projectile-project-root)
                 (lambda () "/test/"))
                ((symbol-function 'claude-code-ide-send-notification)
                 (lambda (_project _method _params)
                   (setq notification-count (1+ notification-count)))))

        ;; Trigger multiple selection changes rapidly
        (claude-code-ide-events-selection-changed)
        (claude-code-ide-events-selection-changed)
        (claude-code-ide-events-selection-changed)

        ;; Timer should be set but not fired yet
        (should claude-code-ide-events-selection-timer)
        (should (= notification-count 0))

        ;; Cancel timer to avoid side effects
        (when claude-code-ide-events-selection-timer
          (cancel-timer claude-code-ide-events-selection-timer)
          (setq claude-code-ide-events-selection-timer nil))))))

(provide 'claude-code-ide-events-test)
;;; claude-code-ide-events-test.el ends here
