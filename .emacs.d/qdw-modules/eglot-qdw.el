;; -*- lexical-binding: t; -*-

(use-package dape
    :after (eglot)
    
    :custom
    ;; Don't display info or buffers on startup
    (dape-buffer-window-arrangement 'right)
    
    :hook
    ((kill-emacs . dape-breakpoint-save)
     (after-init . dape-breakpoint-load))
  
    :config dape-buffer-window-arrangement 'right

            ;; Show inline variable values (like VS Code's "decorations")
            dape-inlay-hints t

            ;; Don't kill compile buffer/other windows on session start
            (setq dape-kill-buffers nil)

            ;; Turn on repl completion
            (add-hook 'dape-on-start-hooks
                      (lambda () (save-some-buffers t t)))

    ;; --- Python ---
    ;; Requires: pip install debugpy
    ;; Built-in `debugpy` template covers this; just make sure
    ;; debugpy is on PATH / in your active venv.
    
    ;; --- C ---
    ;; Requires lldb-dap (ships with Xcode CLT on macOS) or gdb+gdb-dap
    ;; on Linux. Built-in `lldb-dap` and `gdb` templates cover this.
    
    ;; --- JS/TS ---
    ;; Requires js-debug adapter: 
    ;;   git clone https://github.com/microsoft/vscode-js-debug
    ;;   npm install && npx gulp dapDebugServer
    ;; then point dape-configs at the built js-debug bundle, e.g.:
    (add-to-list 'dape-configs
                 `(js-debug
                   modes (js-mode js-ts-mode typescript-mode
                                  typescript-ts-mode)
                   command "node"
                   command-args
                   (,(expand-file-name
                      "~/vscode-js-debug/dist/src/dapDebugServer.js")
                     "${port}")
                   port :autoport
                   :type "pwa-node"
                   :request "launch"
                   :program dape-buffer-default
                   :cwd dape-cwd-default))
