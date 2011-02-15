;;;
;;; Basic logging
;;;

(require 'rel-support)

(defun rel-timestamp ()
  (format-time-string "%j-%H:%M:%S"))

(defun rel-log-buffer ()
  (or (get-buffer rel-log-bufname)
      (let ((log-buffer (get-buffer-create rel-log-bufname)))
	(with-current-buffer log-buffer
	  (setq buffer-read-only t)
	  (buffer-disable-undo)
	  (let (rel-timestamp-function)
	    (rel-log 'unnamed "** LOG CREATED %s ***\n" (rel-timestamp))))
	log-buffer)))

(defconst rel-timestamp-function 'rel-timestamp
  "If non-nil, timestamps are enabled in logs. Can be set to a
  custom function.")

(defconst rel-log-level-names
  '((error . "ERR!") (warning . "WARN") (info . "INFO") (debug . "DEBG"))
  "Alist of text to prefix log-level messages with.")

(defconst rel-echo-levels
  '(error warning)
  "List of log-levels which will also go to the echo area")

(defun rel-log-level-name (level)
  "Returns a print-name for the log level LEVEL or nil if none is
found. See `rel-log-level-names'."
  (cdr (assq level rel-log-level-names)))

(defun rel-log (level message &rest rest)
  "Log something. LEVEL is the log level. Should be something like `error',
`warning', `info' or `debug'. MESSAGE and REST are used as used as
arguments to `format'."
  (let* ((level-name (rel-log-level-name level))
	 (formatted-message (apply 'format message format-args))
	 (log-message (if level-name 
			  (concat level-name ": " formatted-message)
			formatted-message))
	 (max-echo-size (- (window-width) 8)))
    ;; some levels also trigger an echo-message
    (when (memq level rel-echo-levels)
      (message (truncate-string-to-width log-message max-echo-size)))
    ;; timestamps don't show up in messages
    (if rel-timestamp-function
	(setq log-message 
	      (concat (funcall rel-timestamp-function) ": " log-message)))
    (with-current-buffer (rel-log-buffer)
      (let (buffer-read-only)
	(buffer-puts-tail (rel-log-buffer) log-message)))
    (redisplay t) ;; force immediate update
    ))

(defun rel-log-info (message &rest format-args)
  (apply 'rel-log 'info message format-args))
(defun rel-log-debug (message &rest format-args)
  (apply 'rel-log 'debug message format-args))
(defun rel-log-warn (message &rest format-args)
  (apply 'rel-log 'warning message format-args))
(defun rel-log-error (message &rest format-args)
  (apply 'rel-log 'error message format-args))


(defun append-string-to-buffer (buffer string)
  "Append a STRING to BUFFER. Does not move point."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (insert string))))

(defun add-smart-newline (string)
  "Returns STRING with an \n appended if it does not currenlty have one."
  (if (string-match "\n" string)
      string
    (concat string "\n")))

(defvar buffer-puts-tail-mode 'always
  "What to do after `buffer-puts'. Values: `always' - always move
to end of window. `maybe' - move to the end of the window if the
window-point was at the end prior to the function. `never' -
nothing is done on nil.")


(provide 'rel-log)