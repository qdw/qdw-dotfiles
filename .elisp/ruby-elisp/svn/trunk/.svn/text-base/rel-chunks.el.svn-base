;;;
;;; Chunking suppot functions
;;;
;;; Definition:
;;; - "chunk" is a complete request to the server or the
;;; complete response from the server. It does not include any
;;; communication-level data such as a seperator.
;;;
;;; Note: currently multi-chunking (more then one chunk in the queue)
;;; is not supported and it will display a message. Results are not
;;; predictable.
;;;

(require 'rel-support)
(require 'cl) ;; push/pop

;; 05 MAR 2007 - Chunk storage is per-process and the rel-process
;; variable is used to identify which process.

(defun rel-chunk-ready-p ()
  "Non-nil if a chunk is ready."
  (/= (length (process-get rel-process 'rel-chunks)) 0))

;; PST -- currently multiple chunks will raise a warning
(defun rel-add-chunk (chunk)
  "Add a chunk to the end of the chunk FIFO queue."
  (rel-log-info ">> Chunk in: [[%s]]"
                (truncate-string (prin1-to-string chunk) 120))
  (when (rel-chunk-ready-p)
    (rel-log-warn "Multiple chunks in buffer."))
  (push chunk (process-get rel-process 'rel-chunks)))

(defun rel-reset-chunks ()
  "Remove all chunks"
  (process-put rel-process 'rel-chunks nil))

(defun rel-next-chunk ()
  "Returns the next chunk or nil is there are no chunks ready.
The chunk queue is altered."
  (let ((chunks (process-get rel-process 'rel-chunks)))
    (if chunks
        (let ((chunks-left (1- (length chunks))))
          (rel-log-info "<< Chunk read: chunks-left=%d" chunks-left))
      (rel-log-info "<< Chunk read failed: no chunks left"))
    (pop (process-get rel-process 'rel-chunks))))

(provide 'rel-chunks)