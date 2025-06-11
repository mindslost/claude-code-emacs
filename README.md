This package provides Emacs integration for Claude Code, Anthropic's
agentic command line tool for delegating coding tasks to Claude.

Features:
- Send code regions or entire buffers to Claude Code
- Interactive prompts for coding tasks
- Project-wide code analysis and refactoring
- Integration with compilation mode for error handling
- Customizable Claude Code command and arguments

## Main Functions

- **`claude-code-send-region`** - Send selected text to Claude Code
- **`claude-code-send-buffer`** - Send entire buffer to Claude Code
- **`claude-code-send-file`** - Send any file to Claude Code
- **`claude-code-project-task`** - Execute project-wide tasks
- **`claude-code-interactive`** - Start an interactive session

## Convenience Functions

- **`claude-code-explain-code`** - Ask Claude to explain code
- **`claude-code-refactor-code`** - Request code refactoring
- **`claude-code-add-tests`** - Generate tests for code
- **`claude-code-fix-errors`** - Identify and fix code issues

## Installation & Setup

1. Save the code to a file named `claude-code.el`
2. Add it to your Emacs load path
3. Add to your `.emacs` or `init.el`:

```elisp
(require 'claude-code)
;; Optional: set up global keybinding
(global-set-key (kbd "C-c C") claude-code-map)
```

## Configuration

The module includes several customizable options:
- `claude-code-executable` - Path to Claude Code binary
- `claude-code-default-args` - Default command arguments
- `claude-code-auto-scroll` - Auto-scroll output buffer
- `claude-code-confirm-before-applying` - Confirmation before changes

## Key Bindings (when prefix is set)

- `C-c C r` - Send region
- `C-c C b` - Send buffer  
- `C-c C f` - Send file
- `C-c C p` - Project task
- `C-c C i` - Interactive mode
- `C-c C e` - Explain code
- `C-c C R` - Refactor code
- `C-c C t` - Add tests
- `C-c C x` - Fix errors

The module handles process management, output formatting, and provides a clean interface for integrating Claude Code into your Emacs workflow. You'll need Claude Code installed and accessible in your PATH for this to work.
