;;;
;;; Process control
;;;

(defvar rel-process nil
  "Process handle to currently running rel Server, if any.")

(defun rel-attach-process (process)
  (setq rel-process process))

(defun rel-process-status ()
  "Returns process status as defined by `process-status'."
  ;; `pocessp' check is to keep `process-status' from using the current
  ;; buffer if rel-process is nil or otherwise broken
  (and (processp rel-process)
       (process-status rel-process)))

(defun rel-is-alive-p ()
  "Returns non-nil if REL process is alive."
  (eq (rel-process-status) 'run))
(defalias 'rel-running-p 'rel-is-alive-p)

(defun rel-kill-process ()
  (process-send-signal 'SIGTERM rel-process)
  (with-timeout (5 (process-send-signal 'SIGKILL rel-process))
    (while (rel-is-alive-p)
      (sleep-for 0.1)))
  (setq rel-process nil))

(defun rel-process-name ()
  (process-name rel-process))

(provide 'rel-process)