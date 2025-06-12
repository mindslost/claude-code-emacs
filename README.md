# Claude Code Emacs Integration

This package provides Emacs integration for Claude Code, Anthropic's
agentic command line tool for delegating coding tasks to Claude.

## Features

- Send code regions or entire buffers to Claude Code
- Interactive prompts for coding tasks
- Project-wide code analysis and refactoring
- Full terminal integration using vterm
- Customizable Claude Code command and arguments
- Pre-built convenience functions for common tasks

## Requirements

- Emacs 28.1+
- [vterm](https://github.com/akermu/emacs-libvterm) package
- Claude Code CLI tool installed and accessible in PATH

## Main Functions

- **`claude-code-send-region`** - Send selected text to Claude Code
- **`claude-code-send-buffer`** - Send entire buffer to Claude Code
- **`claude-code-send-file`** - Send any file to Claude Code
- **`claude-code-project-task`** - Execute project-wide tasks
- **`claude-code-interactive`** - Start an interactive Claude Code session
- **`claude-code-open-vterm`** - Open the Claude Code vterm buffer

## Convenience Functions

- **`claude-code-explain-code`** - Ask Claude to explain code
- **`claude-code-refactor-code`** - Request code refactoring
- **`claude-code-add-tests`** - Generate tests for code
- **`claude-code-fix-errors`** - Identify and fix code issues

## Installation & Setup

See [INSTALL.md](INSTALL.md) for detailed installation instructions.

Quick setup:
1. Install vterm package
2. Save `claude-code.el` to your load path
3. Add to your configuration:

```elisp
(require 'claude-code)
;; Optional: set up global keybinding
(global-set-key (kbd "C-c C") claude-code-map)
```

## Configuration

The package includes several customizable options:
- `claude-code-executable` - Path to Claude Code binary (default: "claude-code")
- `claude-code-default-args` - Default command arguments
- `claude-code-vterm-name` - Name of the vterm buffer (default: "*Claude Code*")
- `claude-code-confirm-before-applying` - Confirmation before changes

## Key Bindings

When using the built-in keymap (`claude-code-map`):

- `r` - Send region
- `b` - Send buffer  
- `f` - Send file
- `p` - Project task
- `i` - Interactive mode
- `s` - Open vterm buffer
- `e` - Explain code
- `R` - Refactor code
- `t` - Add tests
- `x` - Fix errors

## How It Works

### Vterm Integration

All Claude Code interactions happen within a dedicated vterm buffer, providing:
- **Full terminal experience** - Complete terminal emulation with proper colors and formatting
- **Real-time output** - See Claude Code responses as they stream
- **Interactive session** - Continue conversations after initial commands
- **Terminal controls** - Full keyboard support for terminal navigation

### Workflow Example

1. **Send code**: Run `claude-code-send-buffer` with prompt "explain this code"  
2. **Vterm opens**: Claude Code vterm buffer opens showing real-time response
3. **Interactive session**: After response, you can continue typing commands
4. **Full terminal**: Use all standard terminal features (scrolling, copying, etc.)
5. **Persistent session**: The vterm stays open for continued interaction

### File and Project Integration

- **File context**: When sending files, the full file path is shared with Claude
- **Project awareness**: Project tasks automatically set the correct working directory
- **Temporary files**: Code regions are safely passed via temporary files

## Vterm Benefits

Using vterm provides several advantages over traditional shell integration:
- **Better performance** - Native terminal emulation
- **Full compatibility** - Works with all Claude Code features
- **Rich formatting** - Proper colors, styling, and terminal graphics
- **Standard terminal** - All familiar terminal shortcuts and behaviors work

The package requires vterm to be installed and will create a dedicated Claude Code terminal for all interactions.