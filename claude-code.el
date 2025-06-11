;;; claude-code.el --- Emacs integration for Claude Code -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Jason Lysinger <jasonlysinger@midnslost.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
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

(defcustom claude-code-buffer-name "*Claude Code*"
  "Name of the buffer to display Claude Code output."
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

(defface claude-code-prompt-face
  '((t (:foreground "cyan" :weight bold)))
  "Face for Claude Code prompts."
  :group 'claude-code)

(defface claude-code-output-face
  '((t (:foreground "green")))
  "Face for Claude Code output."
  :group 'claude-code)

(defface claude-code-error-face
  '((t (:foreground "red" :weight bold)))
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
    (claude-code--execute-with-input code prompt filename)))

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
  (claude-code--execute-with-file file prompt))

;;;###autoload
(defun claude-code-project-task (prompt)
  "Execute a project-wide task with Claude Code using PROMPT."
  (interactive
   (list (read-string "Claude Code project task: " nil 'claude-code-history)))
  (let ((project-root (claude-code--get-project-root)))
    (if project-root
        (let ((default-directory project-root))
          (claude-code--execute prompt))
      (error "Not in a project directory"))))

;;;###autoload
(defun claude-code-interactive ()
  "Start an interactive Claude Code session."
  (interactive)
  (let ((buffer (get-buffer-create claude-code-buffer-name)))
    (with-current-buffer buffer
      (claude-code-mode)
      (goto-char (point-max))
      (insert "\n=== Interactive Claude Code Session ===\n")
      (insert "Type your prompts below. Use C-c C-c to send.\n\n"))
    (pop-to-buffer buffer)))

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

(defun claude-code--execute (prompt &optional input-file)
  "Execute Claude Code with PROMPT and optional INPUT-FILE."
  (let* ((buffer (get-buffer-create claude-code-buffer-name))
         (args (append claude-code-default-args (list prompt)))
         (command (mapconcat 'shell-quote-argument
                           (cons claude-code-executable args) " ")))
    (with-current-buffer buffer
      (claude-code-mode)
      (goto-char (point-max))
      (insert (format "\n=== Claude Code: %s ===\n" prompt))
      (insert (format "Command: %s\n" command))
      (insert "Output:\n"))
    
    (when claude-code-auto-scroll
      (display-buffer buffer))
    
    (setq claude-code-process
          (start-process "claude-code" buffer
                        claude-code-executable prompt))
    
    (set-process-sentinel claude-code-process 'claude-code--process-sentinel)
    (set-process-filter claude-code-process 'claude-code--process-filter)))

(defun claude-code--execute-with-input (input prompt &optional filename)
  "Execute Claude Code with INPUT text and PROMPT, optionally for FILENAME."
  (let* ((buffer (get-buffer-create claude-code-buffer-name))
         (temp-file (make-temp-file "claude-code-" nil
                                   (when filename
                                     (concat "." (file-name-extension filename)))))
         (args (append claude-code-default-args (list prompt temp-file))))
    
    ;; Write input to temporary file
    (with-temp-file temp-file
      (insert input))
    
    ;; Display the buffer
    (with-current-buffer buffer
      (claude-code-mode)
      (goto-char (point-max))
      (insert (format "\n=== Claude Code: %s ===\n" prompt))
      (when filename
        (insert (format "File: %s\n" filename)))
      (insert "Output:\n"))
    
    (when claude-code-auto-scroll
      (display-buffer buffer))
    
    ;; Start the process
    (setq claude-code-process
          (apply 'start-process "claude-code" buffer
                 claude-code-executable args))
    
    (set-process-sentinel claude-code-process 
                         (lambda (proc event)
                           (claude-code--process-sentinel proc event)
                           (delete-file temp-file)))
    (set-process-filter claude-code-process 'claude-code--process-filter)))

(defun claude-code--execute-with-file (file prompt)
  "Execute Claude Code with FILE and PROMPT."
  (let* ((buffer (get-buffer-create claude-code-buffer-name))
         (args (append claude-code-default-args (list prompt file))))
    
    (with-current-buffer buffer
      (claude-code-mode)
      (goto-char (point-max))
      (insert (format "\n=== Claude Code: %s ===\n" prompt))
      (insert (format "File: %s\n" file))
      (insert "Output:\n"))
    
    (when claude-code-auto-scroll
      (display-buffer buffer))
    
    (setq claude-code-process
          (apply 'start-process "claude-code" buffer
                 claude-code-executable args))
    
    (set-process-sentinel claude-code-process 'claude-code--process-sentinel)
    (set-process-filter claude-code-process 'claude-code--process-filter)))

(defun claude-code--process-filter (proc string)
  "Filter function for Claude Code process PROC with output STRING."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert (propertize string 'face 'claude-code-output-face))
          (set-marker (process-mark proc) (point)))
        (when moving
          (goto-char (process-mark proc)))
        (when claude-code-auto-scroll
          (let ((window (get-buffer-window (current-buffer))))
            (when window
              (with-selected-window window
                (goto-char (point-max))))))))))

(defun claude-code--process-sentinel (proc event)
  "Sentinel function for Claude Code process PROC with EVENT."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (cond
       ((string-match-p "finished" event)
        (insert (propertize "\n=== Claude Code completed ===\n"
                           'face 'claude-code-output-face)))
       ((string-match-p "exited abnormally\\|killed" event)
        (insert (propertize (format "\n=== Claude Code failed: %s ===\n" event)
                           'face 'claude-code-error-face)))))))

(defun claude-code--get-project-root ()
  "Get the root directory of the current project."
  (when-let ((project (project-current)))
    (project-root project)))

;; Major mode for Claude Code output buffer
(define-derived-mode claude-code-mode special-mode "Claude Code"
  "Major mode for Claude Code output buffer."
  (setq buffer-read-only nil)
  (make-local-variable 'claude-code-process))

(define-key claude-code-mode-map (kbd "q") 'quit-window)
(define-key claude-code-mode-map (kbd "g") 'claude-code-refresh)
(define-key claude-code-mode-map (kbd "k") 'claude-code-kill-process)

(defun claude-code-refresh ()
  "Refresh the Claude Code output buffer."
  (interactive)
  (when (buffer-live-p (current-buffer))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Claude Code output buffer refreshed.\n"))))

(defun claude-code-kill-process ()
  "Kill the current Claude Code process."
  (interactive)
  (when (and claude-code-process
             (process-live-p claude-code-process))
    (kill-process claude-code-process)
    (message "Claude Code process killed")))

;; Keymap for convenient access
(defvar claude-code-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'claude-code-send-region)
    (define-key map (kbd "b") 'claude-code-send-buffer)
    (define-key map (kbd "f") 'claude-code-send-file)
    (define-key map (kbd "p") 'claude-code-project-task)
    (define-key map (kbd "i") 'claude-code-interactive)
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