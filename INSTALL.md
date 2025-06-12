# Installation Guide

Here are several ways to install the Claude Code Emacs integration package:

## Prerequisites

Before installing, ensure you have:
- **Emacs 28.1+** - Check with `M-x emacs-version`
- **vterm package** - Install from MELPA or compile from source
- **Claude Code CLI** - Install from Anthropic and ensure it's in your PATH

### Installing vterm

vterm is required for terminal integration. Install it via your preferred method:

**Using MELPA:**
```elisp
(use-package vterm)
```

**Or manually:**
```elisp
(package-install 'vterm)
```

For compilation from source, see the [vterm documentation](https://github.com/akermu/emacs-libvterm).

## Method 1: Manual Installation (Simplest)

1. **Save the file**:
   - Copy `claude-code.el` to `~/.emacs.d/lisp/claude-code.el`
   - Create the directory if needed: `mkdir -p ~/.emacs.d/lisp/`

2. **Add to your Emacs configuration**:
   ```elisp
   ;; Add to ~/.emacs or ~/.emacs.d/init.el
   (add-to-list 'load-path "~/.emacs.d/lisp/")
   (require 'claude-code)
   
   ;; Optional: Set up global keybinding
   (global-set-key (kbd "C-c C") claude-code-map)
   ```

3. **Restart Emacs** or evaluate the configuration with `M-x eval-buffer`

## Method 2: Using use-package (Recommended)

If you use `use-package`, add this to your config:

```elisp
(use-package claude-code
  :load-path "~/.emacs.d/lisp/"
  :bind (("C-c C r" . claude-code-send-region)
         ("C-c C b" . claude-code-send-buffer)
         ("C-c C f" . claude-code-send-file)
         ("C-c C p" . claude-code-project-task)
         ("C-c C i" . claude-code-interactive)
         ("C-c C s" . claude-code-open-vterm)
         ("C-c C e" . claude-code-explain-code)
         ("C-c C R" . claude-code-refactor-code)
         ("C-c C t" . claude-code-add-tests)
         ("C-c C x" . claude-code-fix-errors))
  :custom
  (claude-code-executable "claude")
  (claude-code-vterm-name "*Claude Code*"))
```

## Method 3: Package Development Setup

If you want to develop/modify the package:

1. **Clone or create a directory**:
   ```bash
   mkdir ~/.emacs.d/dev/claude-code-emacs
   cd ~/.emacs.d/dev/claude-code-emacs
   ```

2. **Save the file** as `claude-code.el` in that directory

3. **Add to your config**:
   ```elisp
   (add-to-list 'load-path "~/.emacs.d/dev/claude-code-emacs/")
   (require 'claude-code)
   ```

## Method 4: Straight.el (Advanced)

If you use `straight.el` package manager:

```elisp
(straight-use-package
 '(claude-code :type git :local-repo "~/.emacs.d/dev/claude-code-emacs"))

(use-package claude-code
  :straight nil  ; Already loaded above
  :bind (("C-c C r" . claude-code-send-region)
         ("C-c C b" . claude-code-send-buffer)
         ("C-c C i" . claude-code-interactive))
  :custom
  (claude-code-executable "claude"))
```

## Configuration Options

Customize the package behavior:

```elisp
(setq claude-code-executable "claude")                    ; CLI command name
(setq claude-code-default-args '())                       ; Default arguments  
(setq claude-code-vterm-name "*Claude Code*")             ; vterm buffer name
(setq claude-code-confirm-before-applying t)              ; Confirm changes
```

Or use the customization interface: `M-x customize-group RET claude-code`

## Verification Steps

After installation:

1. **Check if it loaded**: `M-x describe-function RET claude-code-send-region`
2. **Test vterm integration**: `M-x claude-code-interactive` should open a vterm buffer
3. **Test basic function**: Try `M-x claude-code-send-buffer` with a simple prompt
4. **Check keybindings**: If configured, try your key combinations

## Troubleshooting

### vterm Issues
- Ensure vterm is properly installed and compiled
- Check that your system has the required dependencies for vterm
- Test vterm independently: `M-x vterm`

### Claude Code CLI Issues
- Verify Claude Code is installed: `M-x shell-command RET which claude`
- Test Claude Code works: `M-x shell-command RET claude --version`
- Check PATH if command not found

### Package Loading Issues
- Check that the file path is correct in your load-path
- Verify Emacs version: `M-x emacs-version` (must be 28.1+)
- Look for errors in `*Messages*` buffer

### vterm Compilation
If vterm fails to compile:
- Install cmake and libtool on your system
- Follow the [vterm installation guide](https://github.com/akermu/emacs-libvterm#installation)
- On macOS: `brew install cmake libtool`
- On Ubuntu/Debian: `sudo apt install cmake libtool-bin`

The manual installation method is usually the easiest to start with. Once you verify everything works, you can switch to a more sophisticated package management approach if desired.