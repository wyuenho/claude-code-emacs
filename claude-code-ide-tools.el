;;; claude-code-ide-tools.el --- IDE protocol tool handlers -*- lexical-binding: t; -*-

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

;; This module dispatches IDE protocol tool calls to existing MCP tool handlers.
;; It provides a unified interface for the 12 IDE protocol tools required by
;; Claude Code.

;;; Code:

(require 'claude-code-mcp-tools)

;;; Tool Dispatcher

(defun claude-code-ide-tools-dispatch (method params)
  "Dispatch IDE protocol tool call METHOD with PARAMS.
Returns the result or (error . MESSAGE) on failure."
  (condition-case err
      (pcase method
        ;; File operations
        ("openFile" (claude-code-mcp-handle-openFile params))
        ("openDiff" (claude-code-mcp-handle-openDiffFile params))

        ;; Selection operations
        ("getCurrentSelection" (claude-code-mcp-handle-getCurrentSelection params))
        ("getLatestSelection" (claude-code-mcp-handle-getLatestSelection params))

        ;; Editor operations
        ("getOpenEditors" (claude-code-mcp-handle-getOpenEditors params))
        ("getWorkspaceFolders" (claude-code-mcp-handle-getWorkspaceFolders params))

        ;; Diagnostics
        ("getDiagnostics" (claude-code-mcp-handle-getDiagnostics params))

        ;; Document operations
        ("checkDocumentDirty" (claude-code-mcp-handle-checkDocumentDirty params))
        ("saveDocument" (claude-code-mcp-handle-saveDocument params))

        ;; Tab/buffer operations
        ("close_tab" (claude-code-mcp-handle-closeTab params))
        ("closeAllDiffTabs" (claude-code-mcp-handle-closeAllDiffTabs params))

        ;; Code execution (not yet implemented)
        ("executeCode" (claude-code-ide-handle-executeCode params))

        ;; Unknown method
        (_ (cons 'error (format "Unknown method: %s" method))))
    (error
     (cons 'error (error-message-string err)))))

;;; Tool Implementations

(defun claude-code-ide-handle-executeCode (params)
  "Handle executeCode request with PARAMS.
Executes code in a shell or REPL environment.
Not yet fully implemented - returns placeholder."
  (let ((code (cdr (assoc 'code params)))
        (language (cdr (assoc 'language params))))
    (condition-case err
        (progn
          (unless code
            (error "Code is required"))
          ;; TODO: Implement code execution
          ;; For now, return a not-implemented message
          `((success . ,json-false)
            (message . ,(format "Code execution not yet implemented (language: %s)"
                               (or language "unknown")))))
      (error
       `((success . ,json-false)
         (message . ,(error-message-string err)))))))

(provide 'claude-code-ide-tools)
;;; claude-code-ide-tools.el ends here
