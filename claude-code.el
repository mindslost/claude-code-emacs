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
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "\n=== Claude Code: %s ===\n" prompt))
        (insert (format "Command: %s\n" command))
        (insert "Output:\n")))
    
    ;; Always display the buffer and make it active
    (pop-to-buffer buffer)
    (goto-char (point-max))
    
    (setq claude-code-process
          (start-process "claude-code" buffer
                        claude-code-executable prompt))
    
    (with-current-buffer buffer
      (set-process-marker claude-code-process (point-max)))
    
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
    
    ;; Display the buffer and make it active
    (with-current-buffer buffer
      (claude-code-mode)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "\n=== Claude Code: %s ===\n" prompt))
        (when filename
          (insert (format "File: %s\n" filename)))
        (insert "Output:\n")))
    
    (pop-to-buffer buffer)
    (goto-char (point-max))
    
    ;; Start the process
    (setq claude-code-process
          (apply 'start-process "claude-code" buffer
                 claude-code-executable args))
    
    (with-current-buffer buffer
      (set-process-marker claude-code-process (point-max)))
    
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
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "\n=== Claude Code: %s ===\n" prompt))
        (insert (format "File: %s\n" file))
        (insert "Output:\n")))
    
    (pop-to-buffer buffer)
    (goto-char (point-max))
    
    (setq claude-code-process
          (apply 'start-process "claude-code" buffer
                 claude-code-executable args))
    
    (with-current-buffer buffer
      (set-process-marker claude-code-process (point-max)))
    
    (set-process-sentinel claude-code-process 'claude-code--process-sentinel)
    (set-process-filter claude-code-process 'claude-code--process-filter)))

(defun claude-code--process-filter (proc string)
  "Filter function for Claude Code process PROC with output STRING."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc)))
            (inhibit-read-only t))
        (save-excursion
          (goto-char (process-mark proc))
          ;; Insert the output with appropriate face
          (let ((start (point)))
            (insert string)
            (put-text-property start (point) 'face 'claude-code-output-face))
          (set-marker (process-mark proc) (point)))
        (when moving
          (goto-char (process-mark proc)))
        (when claude-code-auto-scroll
          (let ((window (get-buffer-window (current-buffer))))
            (when window
              (with-selected-window window
                (goto-char (point-max))
                (recenter -1)))))))))

(defun claude-code--process-sentinel (proc event)
  "Sentinel function for Claude Code process PROC with EVENT."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (cond
         ((string-match-p "finished" event)
          (insert (propertize "\n=== Claude Code completed ===\n"
                             'face 'claude-code-output-face))
          (claude-code-insert-prompt))
         ((string-match-p "exited abnormally\\|killed" event)
          (insert (propertize (format "\n=== Claude Code failed: %s ===\n" event)
                             'face 'claude-code-error-face))
          (claude-code-insert-prompt)))))))

(defun claude-code--get-project-root ()
  "Get the root directory of the current project."
  (when-let ((project (project-current)))
    (project-root project)))

;; Major mode for Claude Code output buffer
(define-derived-mode claude-code-mode comint-mode "Claude Code"
  "Major mode for Claude Code output buffer with interactive capabilities."
  (setq buffer-read-only nil)
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t)
  (make-local-variable 'claude-code-process)
  (make-local-variable 'claude-code-input-ring)
  (setq claude-code-input-ring (make-ring 50))
  
  ;; Enable input history
  (set (make-local-variable 'comint-input-ring) claude-code-input-ring)
  (set (make-local-variable 'comint-input-ring-size) 50)
  
  ;; Set up font-lock for better highlighting
  (setq font-lock-defaults
        '(claude-code-font-lock-keywords t nil nil nil))
  
  ;; Make it feel more like a terminal
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t))

;; Font-lock keywords for better syntax highlighting
(defvar claude-code-font-lock-keywords
  '(("^===.*===$" . 'claude-code-prompt-face)
    ("^Command:.*$" . 'font-lock-comment-face)
    ("^Output:$" . 'font-lock-keyword-face)
    ("^File:.*$" . 'font-lock-string-face)
    ("ERROR\\|Error\\|error" . 'claude-code-error-face)
    ("SUCCESS\\|Success\\|success\\|completed" . 'claude-code-output-face))
  "Font-lock keywords for Claude Code mode.")

(define-key claude-code-mode-map (kbd "q") 'quit-window)
(define-key claude-code-mode-map (kbd "g") 'claude-code-refresh)
(define-key claude-code-mode-map (kbd "k") 'claude-code-kill-process)
(define-key claude-code-mode-map (kbd "C-c C-c") 'claude-code-interrupt)
(define-key claude-code-mode-map (kbd "C-c C-d") 'claude-code-send-eof)
(define-key claude-code-mode-map (kbd "C-c C-z") 'claude-code-stop-process)
(define-key claude-code-mode-map (kbd "RET") 'claude-code-send-input)

(defun claude-code-refresh ()
  "Refresh the Claude Code output buffer."
  (interactive)
  (when (buffer-live-p (current-buffer))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Claude Code output buffer refreshed.\n")
      (claude-code-insert-prompt))))

(defun claude-code-kill-process ()
  "Kill the current Claude Code process."
  (interactive)
  (when (and claude-code-process
             (process-live-p claude-code-process))
    (kill-process claude-code-process)
    (message "Claude Code process killed")))

(defun claude-code-interrupt ()
  "Send interrupt signal to Claude Code process."
  (interactive)
  (when (and claude-code-process
             (process-live-p claude-code-process))
    (interrupt-process claude-code-process)
    (message "Interrupt signal sent to Claude Code")))

(defun claude-code-send-eof ()
  "Send EOF to Claude Code process."
  (interactive)
  (when (and claude-code-process
             (process-live-p claude-code-process))
    (process-send-eof claude-code-process)
    (message "EOF sent to Claude Code")))

(defun claude-code-stop-process ()
  "Stop the Claude Code process gracefully."
  (interactive)
  (when (and claude-code-process
             (process-live-p claude-code-process))
    (process-send-string claude-code-process "exit\n")
    (message "Stop signal sent to Claude Code")))

(defun claude-code-send-input ()
  "Send input to Claude Code process."
  (interactive)
  (when (and claude-code-process
             (process-live-p claude-code-process))
    (let ((input (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position))))
      (unless (string-empty-p (string-trim input))
        (process-send-string claude-code-process (concat input "\n"))
        (goto-char (point-max))
        (insert "\n")))))

(defun claude-code-insert-prompt ()
  "Insert a prompt in the Claude Code buffer."
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (insert (propertize "claude-code> " 'face 'claude-code-prompt-face))
  (set-marker (process-mark claude-code-process) (point)))

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

;;;###autoload
(defun claude-code-shell-command (command)
  "Execute a Claude Code shell command from within Emacs.
This function can be called from shell buffers or via M-x."
  (interactive "sClaud Code command: ")
  (let ((parts (split-string command))
        (action (car parts))
        (args (cdr parts)))
    (cond
     ((string= action "explain")
      (if args
          (claude-code-send-file (car args) "Please explain what this code does and how it works:")
        (claude-code-explain-code)))
     
     ((string= action "refactor")
      (if args
          (claude-code-send-file (car args) "Please refactor this code to improve readability, performance, and maintainability:")
        (claude-code-refactor-code)))
     
     ((string= action "test")
      (if args
          (claude-code-send-file (car args) "Please add comprehensive tests for this code:")
        (claude-code-add-tests)))
     
     ((string= action "fix")
      (if args
          (claude-code-send-file (car args) "Please identify and fix any errors or issues in this code:")
        (claude-code-fix-errors)))
     
     ((string= action "project")
      (if args
          (claude-code-project-task (string-join args " "))
        (call-interactively 'claude-code-project-task)))
     
     ((string= action "custom")
      (if (>= (length args) 2)
          (claude-code-send-file (car args) (string-join (cdr args) " "))
        (message "Usage: claude-code custom <file> <prompt>")))
     
     (t
      (message "Available commands: explain, refactor, test, fix, project, custom")))))

;; Shell integration functions
(defun claude-code-from-shell ()
  "Handle claude-code commands from shell buffers."
  (interactive)
  (let ((command (read-string "Claude Code: ")))
    (claude-code-shell-command command)))

;; Add shell command completion
(defvar claude-code-shell-commands
  '("explain" "refactor" "test" "fix" "project" "custom")
  "List of available Claude Code shell commands.")

(defun claude-code-shell-completion (command)
  "Completion function for Claude Code shell commands."
  (let ((candidates claude-code-shell-commands))
    (all-completions command candidates)))

;; Integration with different shell modes
(defun claude-code-setup-shell-integration ()
  "Set up Claude Code integration with shell modes."
  (interactive)
  
  ;; For eshell
  (when (featurep 'eshell)
    (add-hook 'eshell-mode-hook
              (lambda ()
                (define-key eshell-mode-map (kbd "C-c c") 'claude-code-from-shell))))
  
  ;; For shell mode
  (add-hook 'shell-mode-hook
            (lambda ()
              (define-key shell-mode-map (kbd "C-c c") 'claude-code-from-shell)))
  
  ;; For term mode
  (add-hook 'term-mode-hook
            (lambda ()
              (define-key term-raw-map (kbd "C-c c") 'claude-code-from-shell))))

;; Eshell command definition
(defun eshell/claude-code (&rest args)
  "Eshell command for Claude Code integration."
  (let ((command (string-join args " ")))
    (claude-code-shell-command command)
    nil)) ; Return nil to avoid printing return value

;; Auto-setup shell integration when module loads
(add-hook 'after-init-hook 'claude-code-setup-shell-integration)

;; Helper function for getting current file in shell context
(defun claude-code-get-current-file ()
  "Get the current file, trying different methods based on context."
  (cond
   ;; If we're in a file buffer
   ((buffer-file-name) (buffer-file-name))
   
   ;; If we're in a shell buffer, try to get the file from the command line
   ((or (eq major-mode 'shell-mode)
        (eq major-mode 'eshell-mode)
        (eq major-mode 'term-mode))
    (let ((file (read-file-name "File: ")))
      (when (file-exists-p file) file)))
   
   ;; Default case
   (t (read-file-name "File: "))))

;; Enhanced shell commands that are context-aware
;;;###autoload
(defun claude-code-explain-current ()
  "Explain the current file or prompt for one in shell context."
  (interactive)
  (let ((file (claude-code-get-current-file)))
    (if file
        (claude-code-send-file file "Please explain what this code does and how it works:")
      (message "No file selected"))))

;;;###autoload
(defun claude-code-refactor-current ()
  "Refactor the current file or prompt for one in shell context."
  (interactive)
  (let ((file (claude-code-get-current-file)))
    (if file
        (claude-code-send-file file "Please refactor this code to improve readability, performance, and maintainability:")
      (message "No file selected"))))

(provide 'claude-code)

;;; claude-code.el ends here