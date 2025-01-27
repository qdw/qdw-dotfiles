(defvar qdw-themes '("amber" "amber-without-bold" "mariana" "tritium"))

(mapc
 (lambda (file-name)
         (add-to-list 'custom-theme-load-path (expand-file-name file-name)))
 '("~/.emacs.d/qdw-themes"
   "~/src/1/emacs-mariana-theme"
   "~/.emacs.d/third-party-themes"))

;;;; Color settings

(defun if-file-is-theme-reload-theme ()
  (let ((base-name (file-name-nondirectory (file-truename buffer-file-name))))
    (when (string-match "\\(.*\\)-theme\\.el$" base-name)
      (let ((theme-name (match-string 1 base-name)))
        (message "theme-name: %s; qdw-themes: %s" theme-name qdw-themes)
        (when (string-in theme-name qdw-themes)
          (message "theme name is in qdw-themes")
          (load-theme (intern theme-name) 't 't))))))

(add-hook 'after-save-hook #'if-file-is-theme-reload-theme)

(defun test-pattern ()
  (interactive)
  (let ((buffer (get-buffer-create "*tritium colors*")))
    (with-current-buffer buffer
      (erase-buffer)
      (dolist (color-name '("black"
                            "red"
                            "green"
                            "yellow"
                            "blue"
                            "magenta"
                            "cyan"
                            "white"))
        (let ((hex-string
               (symbol-value (intern color-name)))
              
              (bright-hex-string
              (symbol-value (intern (concat "bright-" color-name))))
              
              (color-symbol (symbol-name (intern color-name)))
              
              (bright-color-symbol (symbol-name
                                    (intern
                                     (concat "bright-" color-name)))))
          (insert
           (propertize
            (concat hex-string "  " color-name "\n")
            'face `(:foreground ,hex-string)))
          (insert 
           (propertize
            (concat bright-hex-string "  bright-" color-name "\n")
            'face `(:foreground ,bright-hex-string)))))
      (display-buffer buffer))))

(defun test-pattern-brights-last ()
  (interactive)
  (let ((buffer (get-buffer-create "*tritium colors*")))
    (with-current-buffer buffer
      (erase-buffer)
      (dolist (brightness '("" "bright-"))
        (dolist (color '("black"
                         "red"
                         "green"
                         "yellow"
                         "blue"
                         "magenta"
                         "cyan"
                         "white"))
          (let* ((color-name (concat brightness color))
                 (hex-string (symbol-value (intern color-name))))
            (insert (propertize (concat hex-string "  " color-name "\n")
                                'face `(:foreground ,hex-string)))))))
    (display-buffer buffer)))

(defun tritium-generate-tinted-theme-yaml ()
  (interactive)
  (let ((buffer (get-buffer-create "*tritium.yaml")))
    (with-current-buffer buffer
      (erase-buffer)
      (dolist (base16name-tritiumname '(("base00" . "black")
                                        ("base01" . "red")
                                        ("base02" . "green")
                                        ("base03" . "yellow")
                                        ("base04" . "blue")
                                        ("base05" . "magenta")
                                        ("base06" . "cyan")
                                        ("base07" . "white")
                                        ("base08" . "bright-black")
                                        ("base09" . "bright-red")
                                        ("base0A" . "bright-green")
                                        ("base0B" . "bright-yellow")
                                        ("base0C" . "bright-blue")
                                        ("base0D" . "bright-magenta")
                                        ("base0E" . "bright-cyan")
                                        ("base0F" . "bright-white")))                             
        
        (let* ((tinted-name (car base16name-tritiumname))
                 (tritium-name (cdr base16name-tritiumname))
                 (hex (symbol-value (intern tritium-name))))
          (insert (concat tinted-name ": \"" hex "\"\n"))))
      (display-buffer buffer))))

(defalias 'colors 'list-colors-display)

(defun reload-tritium ()
  (interactive)
  (load-theme 'tritium 't 't))
  
(defun reload-amber ()
  (interactive)
  (load-theme '4MB3R 't 't))

(defun re-theme ()
  "Load a file that defines the 16 4-bit ANSI colors;
  then load tritium, my custom 16-color theme"
  (interactive)
  (load-file
   (read-file-name "Color definition file: "
                   (expand-file-name "~/.emacs.d/qdw-themes/")))
  (reload-tritium))

;; Interpret any ANSI escape codes in this buffer,
;; coloring the buffer text accordingly.
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; tritium, a versatile, dim dark theme by qdw
;; - Looks good under a variety of indoor lighting conditions
;; - Looks good in the dark
;; - Stays readable if red-shifted Flux.app or Night Shift
;; - Auto-detects how many colors your terminal supports
;;   - And adapts colors accordingly
;;     - Supported color spaces:
;;       - 24-bit true color  (millions of colors)
;;       -  8-bit xterm color (256 colors)
;;       -  4-bit ANSI color   (16 colors)

;;; Third-party color themes settings
;;
;;; Loud themes, for prod boxes
;; T  birds-of-paradise-plus: very loud red theme,
;;    for use on production boxes
;; (load-theme 'birds-of-paradise-plus 't)
;;
;;; Light themes, for use outdoors
;; T  almost-mono: light theme, okay for using outdoors, I guess?
;; (load-theme 'almost-mono-cream 't)
;;
;;; Dark themes
;; T  base16-greenscreen: green-phosphor nostalgia theme
;; (load-theme 'base16-greenscreen 't)
;; T  tron-legacy: shades of blue and green text on a black background
;; (load-theme 'tron-legacy)
