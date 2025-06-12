;;; claude-code.el --- Emacs integration for Claude Code -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Jason Lysinger <jasonlysinger@midnslost.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (vterm "0.0.1"))
;; Keywords: ai, coding, claude, anthropic
;; URL: https://github.com/mindslost/claude-code-emacs

;;; Commentary:

;; This package provides Emacs integration for Claude Code, Anthropic's
;; agentic command line tool for delegating coding tasks to Claude.
;;
;; Features:
;; - Send code regions or entire buffers to Claude Code
;; - Interactive prompts for coding tasks
;; - Project-wide code analysis and refactoring
;; - Integration with compilation mode for error handling
;; - Customizable Claude Code command and arguments

;;; Code:

(require 'compile)
(require 'project)
(require 'vterm)

(defgroup claude-code nil
  "Emacs integration for Claude Code."
  :group 'tools
  :prefix "claude-code-")

(defcustom claude-code-executable "claude-code"
  "Path to the Claude Code executable."
  :type 'string
  :group 'claude-code)

(defcustom claude-code-default-args '()
  "Default arguments to pass to Claude Code."
  :type '(repeat string)
  :group 'claude-code)

(defcustom claude-code-vterm-name "*Claude Code*"
  "Name of the vterm buffer for Claude Code."
  :type 'string
  :group 'claude-code)


(defcustom claude-code-auto-scroll t
  "Whether to auto-scroll the Claude Code output buffer."
  :type 'boolean
  :group 'claude-code)

(defcustom claude-code-confirm-before-applying t
  "Whether to confirm before applying Claude Code changes."
  :type 'boolean
  :group 'claude-code)

(defvar claude-code-history nil
  "History of Claude Code commands.")

(defvar claude-code-process nil
  "Current Claude Code process.")

(defvar claude-code-input-ring nil
  "Input history ring for Claude Code.")


(defface claude-code-prompt-face
  '((t (:foreground "#00ff00" :weight bold)))
  "Face for Claude Code prompts."
  :group 'claude-code)

(defface claude-code-output-face
  '((t (:foreground "#ffffff")))
  "Face for Claude Code output."
  :group 'claude-code)

(defface claude-code-error-face
  '((t (:foreground "#ff0000" :weight bold)))
  "Face for Claude Code errors."
  :group 'claude-code)

;;;###autoload
(defun claude-code-send-region (start end prompt)
  "Send the region from START to END to Claude Code with PROMPT."
  (interactive
   (list (region-beginning)
         (region-end)
         (read-string "Claude Code prompt: " nil 'claude-code-history)))
  (let ((code (buffer-substring-no-properties start end))
        (filename (buffer-file-name)))
    (claude-code--execute-in-vterm-with-input code prompt filename)))

;;;###autoload
(defun claude-code-send-buffer (prompt)
  "Send the entire buffer to Claude Code with PROMPT."
  (interactive
   (list (read-string "Claude Code prompt: " nil 'claude-code-history)))
  (claude-code-send-region (point-min) (point-max) prompt))

;;;###autoload
(defun claude-code-send-file (file prompt)
  "Send FILE to Claude Code with PROMPT."
  (interactive
   (list (read-file-name "File to send to Claude Code: ")
         (read-string "Claude Code prompt: " nil 'claude-code-history)))
  (claude-code--execute-in-vterm-with-file file prompt))

;;;###autoload
(defun claude-code-project-task (prompt)
  "Execute a project-wide task with Claude Code using PROMPT."
  (interactive
   (list (read-string "Claude Code project task: " nil 'claude-code-history)))
  (let ((project-root (claude-code--get-project-root)))
    (if project-root
        (let ((default-directory project-root))
          (claude-code--execute-in-vterm prompt))
      (error "Not in a project directory"))))

;;;###autoload
(defun claude-code-interactive ()
  "Start an interactive Claude Code session in vterm."
  (interactive)
  (claude-code--start-vterm-session))


;;;###autoload
(defun claude-code-explain-code ()
  "Ask Claude Code to explain the current region or buffer."
  (interactive)
  (let ((prompt "Please explain what this code does and how it works:")
        (start (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (claude-code-send-region start end prompt)))

;;;###autoload
(defun claude-code-refactor-code ()
  "Ask Claude Code to refactor the current region or buffer."
  (interactive)
  (let ((prompt "Please refactor this code to improve readability, performance, and maintainability:")
        (start (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (claude-code-send-region start end prompt)))

;;;###autoload
(defun claude-code-add-tests ()
  "Ask Claude Code to add tests for the current code."
  (interactive)
  (let ((prompt "Please add comprehensive tests for this code:")
        (start (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (claude-code-send-region start end prompt)))

;;;###autoload
(defun claude-code-fix-errors ()
  "Ask Claude Code to fix errors in the current code."
  (interactive)
  (let ((prompt "Please identify and fix any errors or issues in this code:")
        (start (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (claude-code-send-region start end prompt)))

(defun claude-code--execute-in-vterm (prompt)
  "Execute Claude Code with PROMPT in vterm."
  (let ((vterm-buffer (claude-code--get-or-create-vterm)))
    (with-current-buffer vterm-buffer
      (let ((command (format "%s %s"
                           (shell-quote-argument claude-code-executable)
                           (shell-quote-argument prompt))))
        (claude-code--send-to-vterm command)))))

(defun claude-code--execute-in-vterm-with-input (input prompt &optional filename)
  "Execute Claude Code with INPUT text and PROMPT in vterm.
Optionally specify FILENAME for context."
  (let* ((temp-file (make-temp-file "claude-code-" nil
                                   (when filename
                                     (concat "." (file-name-extension filename)))))
         (vterm-buffer (claude-code--get-or-create-vterm)))
    
    ;; Write input to temporary file
    (with-temp-file temp-file
      (insert input))
    
    (with-current-buffer vterm-buffer
      (when filename
        (claude-code--send-to-vterm (format "echo 'Processing file: %s'" filename)))
      (let ((command (format "%s %s %s"
                           (shell-quote-argument claude-code-executable)
                           (shell-quote-argument prompt)
                           (shell-quote-argument temp-file))))
        (claude-code--send-to-vterm command)
        ;; Clean up temp file after a delay
        (run-with-timer 1 nil (lambda () (when (file-exists-p temp-file)
                                          (delete-file temp-file))))))))

(defun claude-code--execute-in-vterm-with-file (file prompt)
  "Execute Claude Code with FILE and PROMPT in vterm."
  (let ((vterm-buffer (claude-code--get-or-create-vterm)))
    (with-current-buffer vterm-buffer
      (claude-code--send-to-vterm (format "echo 'Processing file: %s'" file))
      (let ((command (format "%s %s %s"
                           (shell-quote-argument claude-code-executable)
                           (shell-quote-argument prompt)
                           (shell-quote-argument file))))
        (claude-code--send-to-vterm command)))))

(defun claude-code--get-project-root ()
  "Get the root directory of the current project."
  (when-let ((project (project-current)))
    (project-root project)))

(defun claude-code--get-or-create-vterm ()
  "Get or create a vterm buffer for Claude Code."
  (let ((vterm-buffer-name claude-code-vterm-name))
    (or (get-buffer vterm-buffer-name)
        (claude-code--create-vterm-buffer vterm-buffer-name))))

(defun claude-code--create-vterm-buffer (buffer-name)
  "Create a new vterm buffer with BUFFER-NAME."
  (let ((vterm-buffer (get-buffer-create buffer-name)))
    (with-current-buffer vterm-buffer
      (vterm-mode)
      (rename-buffer buffer-name))
    (pop-to-buffer vterm-buffer)
    vterm-buffer))


(defun claude-code--send-to-vterm (command)
  "Send COMMAND to the current vterm buffer."
  (vterm-send-string command)
  (vterm-send-return))

(defun claude-code--start-vterm-session ()
  "Start an interactive Claude Code session in vterm."
  (let ((vterm-buffer (claude-code--get-or-create-vterm)))
    (with-current-buffer vterm-buffer
      (claude-code--send-to-vterm "echo 'Starting Claude Code interactive session...'")
      (claude-code--send-to-vterm claude-code-executable))
    (pop-to-buffer vterm-buffer)))


;; Simplified mode for vterm integration
(define-derived-mode claude-code-mode fundamental-mode "Claude Code"
  "Simple mode for Claude Code integration."
  (setq-local comment-start "# ")
  (setq-local comment-end ""))

;; Font-lock keywords
(defvar claude-code-font-lock-keywords
  '(("ERROR\\|Error\\|error\\|FAILED\\|Failed\\|failed" . 'claude-code-error-face)
    ("SUCCESS\\|Success\\|success\\|completed\\|DONE\\|Done\\|done" . 'claude-code-prompt-face))
  "Font-lock keywords for Claude Code mode.")

;; Basic key bindings
(define-key claude-code-mode-map (kbd "C-c C-s") 'claude-code-interactive)

(defun claude-code-open-vterm ()
  "Open the Claude Code vterm buffer."
  (interactive)
  (let ((vterm-buffer (claude-code--get-or-create-vterm)))
    (pop-to-buffer vterm-buffer)))

;; Keymap for convenient access
(defvar claude-code-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'claude-code-send-region)
    (define-key map (kbd "b") 'claude-code-send-buffer)
    (define-key map (kbd "f") 'claude-code-send-file)
    (define-key map (kbd "p") 'claude-code-project-task)
    (define-key map (kbd "i") 'claude-code-interactive)
    (define-key map (kbd "s") 'claude-code-open-vterm)
    (define-key map (kbd "e") 'claude-code-explain-code)
    (define-key map (kbd "R") 'claude-code-refactor-code)
    (define-key map (kbd "t") 'claude-code-add-tests)
    (define-key map (kbd "x") 'claude-code-fix-errors)
    map)
  "Keymap for Claude Code commands.")

;; Optional: Set up a global key binding
;; (global-set-key (kbd "C-c C") claude-code-map)

(provide 'claude-code)

;;; claude-code.el ends here
