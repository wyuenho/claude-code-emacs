;;; claude-code-base.el --- Base utilities for Claude Code Emacs -*- lexical-binding: t; -*-

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

;; Base module for Claude Code Emacs containing shared utilities
;; that are needed by multiple modules.  This module has no dependencies
;; on other claude-code modules to avoid circular dependencies.

;;; Code:

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

;;; Utility Functions

(defun claude-code-normalize-project-root (project-root)
  "Normalize PROJECT-ROOT by removing trailing slash.
Return nil if PROJECT-ROOT is nil."
  (when project-root
    (directory-file-name project-root)))

(provide 'claude-code-base)
;;; claude-code-base.el ends here
