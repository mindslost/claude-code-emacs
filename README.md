Here are several ways to install the Claude Code module into Emacs:

## Method 1: Manual Installation (Simplest)

1. **Save the file**:
   - Copy the code from the artifact and save it as `claude-code.el`
   - Choose a location like `~/.emacs.d/lisp/claude-code.el`

2. **Add to your Emacs configuration**:
   ```elisp
   ;; Add to ~/.emacs or ~/.emacs.d/init.el
   (add-to-list 'load-path "~/.emacs.d/lisp/")
   (require 'claude-code)
   
   ;; Optional: Set up global keybinding
   (global-set-key (kbd "C-c C") claude-code-map)
   ```

3. **Restart Emacs** or evaluate the configuration with `M-x eval-buffer`

## Method 2: Using `use-package` (Recommended)

If you use `use-package`, add this to your config:

```elisp
(use-package claude-code
  :load-path "~/.emacs.d/lisp/"
  :bind ("C-c C" . claude-code-map)
  :custom
  (claude-code-executable "claude-code")
  (claude-code-auto-scroll t))
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
```

## Verification Steps

After installation:

1. **Check if it loaded**: `M-x describe-function RET claude-code-send-region`
2. **Test the keybinding**: Try `C-c C r` (if you set up the global binding)
3. **Check customization**: `M-x customize-group RET claude-code`

## Prerequisites

Make sure you have:
- Claude Code installed and in your PATH
- Emacs 26.1 or later
- The `compile` and `project` packages (usually built-in)

## Troubleshooting

If you get errors:
- Check that the file path is correct
- Ensure Emacs can find Claude Code with `M-x shell-command RET which claude-code`
- Verify your Emacs version with `M-x emacs-version`

The manual installation method is usually the easiest to start with. Once you verify it works, you can switch to a more sophisticated package management approach if desired.