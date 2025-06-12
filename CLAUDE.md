# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Emacs integration package for Claude Code (Anthropic's command-line AI coding assistant). The package provides a comprehensive interface for sending code regions, buffers, or files to Claude Code directly from Emacs.

## Architecture

**Single File Package**: The entire functionality is contained in `claude-code.el` - a self-contained Emacs Lisp package with no external dependencies beyond built-in Emacs packages (`compile`, `project`).

**Core Components**:
- **Interactive Commands**: Functions like `claude-code-send-region`, `claude-code-send-buffer`, `claude-code-project-task`
- **Process Management**: Handles Claude Code subprocess execution with real-time output streaming
- **Major Mode**: `claude-code-mode` based on `comint-mode` for interactive terminal-like experience
- **Shell Integration**: Commands and completion for various shell modes (eshell, shell, term)

**Key Design Patterns**:
- Uses temporary files for passing code content to Claude Code subprocess
- Implements process filters and sentinels for real-time output handling
- Provides both imperative commands and interactive mode for flexible usage
- Customizable through Emacs customization system

## Development Commands

Since this is an Emacs Lisp package, development primarily involves:

**Testing**: Load and test the package in Emacs:
```elisp
(load-file "claude-code.el")
(claude-code-send-buffer "test prompt")
```

**Byte Compilation**: Check for warnings/errors:
```bash
emacs -batch -f batch-byte-compile claude-code.el
```

**Package Validation**: Use built-in Emacs package tools:
```elisp
(package-lint-current-buffer)  ; if package-lint is available
```

## Key Functions and Entry Points

- `claude-code-send-region`: Send selected text with custom prompt
- `claude-code-send-buffer`: Send entire buffer content  
- `claude-code-project-task`: Execute project-wide tasks
- `claude-code-interactive`: Start persistent interactive session
- Convenience functions: `claude-code-explain-code`, `claude-code-refactor-code`, `claude-code-add-tests`, `claude-code-fix-errors`

## Installation and Setup

The package supports multiple installation methods (manual, use-package, straight.el) and requires:
- Emacs 26.1+
- Claude Code CLI tool installed and accessible in PATH
- Optional global keybinding setup: `(global-set-key (kbd "C-c C") claude-code-map)`

## File Structure Notes

- Main implementation: `claude-code.el`
- Documentation: `README.md` (comprehensive feature overview)
- Installation guide: `INSTALL.md` (multiple installation methods)
- No test files or build scripts - testing is done interactively in Emacs