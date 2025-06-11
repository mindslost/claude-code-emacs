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

Now the Claude Code buffer is fully interactive! Here are the key improvements:

## New Interactive Features:

### **Real-time Interaction:**
- Buffer is now based on `comint-mode` (like shell buffers)
- You can see Claude Code output in real-time as it streams
- The buffer automatically pops up and becomes active when you run commands

### **Interactive Controls:**
- **`RET`** - Send input to Claude Code process
- **`C-c C-c`** - Interrupt the process (like Ctrl+C)
- **`C-c C-d`** - Send EOF to process
- **`C-c C-z`** - Stop process gracefully
- **`k`** - Kill process
- **`q`** - Quit window
- **`g`** - Refresh buffer

### **Enhanced Display:**
- Better syntax highlighting for output
- Proper font-lock keywords for errors, success messages, etc.
- Process markers and prompts for better interaction
- Auto-scrolling to show latest output

### **Command History:**
- Input history ring (like shell buffers)
- Navigate through previous commands

## How It Works Now:

1. **When you send a buffer/region**: The Claude Code buffer opens and becomes active
2. **Real-time output**: You see Claude Code's response streaming in real-time
3. **Interactive session**: After the initial response, you get a `claude-code>` prompt
4. **Continue conversation**: You can type additional prompts and press Enter
5. **Process control**: Use the keyboard shortcuts to control the Claude Code process

## Example Workflow:

1. Run `claude-code-send-buffer` with prompt "explain this code"
2. Claude Code buffer opens and shows real-time response
3. When done, you see `claude-code>` prompt
4. Type "now refactor it" and press Enter
5. See the refactored code in real-time
6. Continue the conversation as needed

The buffer is now truly interactive and behaves like a proper terminal session with Claude Code!
