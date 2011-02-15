;; rel - Ruby<->ELisp 
;;
;; REL is a client/server architecture to "run" Ruby from elisp and
;; elisp from Ruby. It strives to be a clean, simple architecture
;; utilizing the features of both languages which can be used to build
;; more advanced tools.
;;
;; Copyright (C) 2007 Paul Stickney <pstickne@gmail.com>

;;; --Disclaimer--
;; While I came up with the idea of REL before I learned about EL4R,
;; the REL elisp code was, at one time, simply a modification of the
;; original EL4R (Elisp for Ruby) code. While it has since metamorphed into a
;; completely different creature, much was the groundwork has roots in
;; the original EL4R code and some similarities may still be observed.
;;
;; EL4R (Elisp for Ruby) can be found at:
;;   http://raa.ruby-lang.org/project/el4r/
;;   http://www.rubyist.net/~rubikitch/computer/el4r/
;; EL4R is Copyright (C) 2005-2007 rubikitch <rubikitch@ruby-lang.org>


;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;


(put 'rel-ruby-error
     'error-conditions                        
     '(error rel-ruby-error))
(put 'rel-ruby-error 'error-message "Error raised in Ruby")

(require 'rel-to-ruby)

(defvar *rel-ruby-program* "ruby"
  "Ruby executable used to start `*rel-server-program*'. Need not be
  absolute if it can be found in the path.")

(defvar *rel-server-program* "/home/pstickne/code/rin/rel/rel.rb"
  "Full path of REL server Ruby program.")

(defvar *rel-server-arguments* nil
  "Misc. arguments to send to REL server.")

(defvar *rel-process-name* "rel"
  "Name of REL process. Tidies up ps and the like.")

(defvar rel-log-bufname "log"
  "Name of buffer to spit log messages to.")

;; PST -- should be buffer-local'able
(defvar rel-process nil
  "Process handle to currently running REL process, if any. It is safe to
make this buffer-local later.")


;; PST -- are any of these implemented?
(defvar rel-debug-on-error nil)
(defvar rel-coding-system nil)
(defconst rel-debug-on-eval nil)
(defconst rel-debug-on-read nil)


(require 'rel-log)


(defun rel-running-p ()
  "Returns non-nil if REL server is running."
  (and (processp rel-process)
       (eq (process-status rel-process) 'run)))

(defun rel-kill-process ()
  (signal-process rel-process 'SIGTERM)
  (with-timeout (5 (signal-process rel-process 'SIGKILL))
    (while (rel-running-p)
      (sleep-for 0.1)))
  (setq rel-process nil))

(defun rel-start ()
  "Intialize the connection with the Server."
  (interactive)
  (if (rel-running-p)
      (rel-stop))
  (rel-log-info "-- SERVER STARTING --")
  (setq rel-process
        (rel-connect))
  (if (not (rel-running-p))
      (rel-log-error "Can't start server")
    ;; initialize
    (set-process-filter rel-process 'rel-process-filter)
    (set-process-sentinel rel-process 'rel-process-sentinel)
    (rel-log-info
     (format "Server process created [pid=%s status=%s]"
             (process-id rel-process)
             (process-status rel-process)))))

(defun rel-connect ()
  (let* ((default-directory                  ; fix working directory
           (file-name-directory *rel-server-program*))
         (process-connection-type nil)       ; use a pipe
         (process-adaptive-read-buffering t) ; use buffering
         (process 
          (apply 'start-process *rel-process-name* nil
                 *rel-ruby-program*
                 *rel-server-program*
                 *rel-server-arguments*)))
    ;;PST - what is the coding system useful for?
    ;;(set-process-coding-system process rel-decode rel-encode)
    process))

(defun rel-stop ()
  "Shutdown rel."
  (interactive)
  (if (rel-running-p)
      (progn
        (rel-log-info "-- SERVER STOPPING --")
        (rel-kill-process)
        (rel-log-info "Shutdown complete"))
    (rel-log-info "Server is not running: can't shutdown")))

(defun rel-restart ()
  "Restart the REL server."
  (interactive)
  (rel-stop)
  (rel-start))

(defun rel-assert-alive ()
  "Signals an error if the process is not alive or returns non-nil if it is
  alive."
  (if (rel-running-p)
      t
    (rel-log-error "-- ASSERT-LIVE FAILED: SERVER IS DEAD --")
    (error "Server is not running")))

;;;
;;; Basic protocol functions
;;;

(defconst rel-to-ruby-sep "\n\n\n")
(defconst rel-from-ruby-sep "\n\n\n")

(require 'rel-chunks)

(defun rel-process-sentinel (process what)
  (rel-log-info
   (case (process-status process)
     ((exit signal)
      ;; perform additional cleanup/checks here
      (format "REL process terminated [pid=%s reason=%s exit-status=%d]"
              (process-id process)
              (process-status process)
              (process-exit-status process)))
     (t
      (format "REL process state changed [pid=%s status=%s]"
              (process-id process)
              (process-status process))))))

(defun rel-process-filter (process new-input)
  "Process filter for responses from REL. Will add chunks which can be read
later with `rel-next-chunk'."
  (rel-log-debug "Recieved input: [[%s]]" new-input)
  (let ((input (concat (process-get process 'old-input) new-input)))
    (save-match-data
      (while (string-match rel-from-ruby-sep input)
        (let* ((next-chunk-start (match-end 0))
               (chunk-end (match-beginning 0))
               (packet (rel-read-entire (substring input 0 chunk-end))))
          (setq input (substring input next-chunk-start))
          (if (eq '* (car packet))
              "do immediate stuff" ;; PST -- needs implementing
            (rel-add-chunk packet)))))
    (process-put process 'old-input input)))

(defun rel-recv (&optional timeout)
  "Read data from the server and return a chunk. May block for a certain
time--default of 5 seconds--but can be specified with TIMEOUT."
  (rel-assert-alive)
  (with-timeout ((or timeout 5) "(reli-timeout-error)")
    (while (not (rel-chunk-ready-p))
      (accept-process-output rel-process))
    (rel-next-chunk)))

(defun rel-send (payload)
  "Send the data to the REL server. May signal an error."
  (rel-log-debug "Sending data: [[%s]]" payload)
  (let ((packet (concat payload rel-from-ruby-sep)))
    (condition-case the-error
	(process-send-string rel-process packet)
      (error
       (rel-assert-alive)
       (resignal the-error)))))

(defun rel-read-eval (sexpr)
  (rel-eval (rel-read-entire sexpr)))

(defun rel-eval (sexpr)
  "Eval a sexpr.  May signal an error."
  (condition-case err
      (eval sexpr)
    (error
     (rel-log-error (format "Error evaling: %s" sexpr))
     (if rel-debug-on-read
         (resignal err)))))

(defun rel-read-entire (string)
  "Reads a sexpr from SEXPR-STRING like `read-from-string'.  However this
will all signal an error if the sexpr does not cover the entire string."
  (let ((read-data
         (condition-case err
             (read-from-string string)
           (end-of-file
            (rel-log-error "Error reading: end-of-string: `%s'" string)
            (resignal err)))))
    (if (eq (cdr read-data) (length string))
        (car read-data)
      (rel-log-error "Error reading: trailing junk: %s" string)
      (error "Extra data found"))))

;;;
;;; Packet/upper processing
;;;

(defun rel-make-header (&optional header-alist)
  "Creates a packet header. HEADER-ALIST should be an alist of (OPTION
. VALUE) pairs or nil. OPTION must be a symbol and VALUE must be a string."
  (mapconcat
   (lambda (cons)
     (concat
      (symbol-name (car cons)) "=" (cdr cons)))
   header-alist ","))

(defun rel-do (header data)
  "Send a command to the server and returns a chunk. May signal an error."
  (rel-send (concat header ":" data))
  (rel-recv))

(defun rel-run (header data)
  (rel-process (rel-do header data)))

(defun rel-process (initial-response)
  "Process a response from the server and returns the final value. Since
this may go through many transactions it will block until it's
complete. May signal any number of errors."
  ;; To get around stack limit problems this will dispatch new calls
  ;; itself; in the past `rel-process-normal' would handle everything
  ;; with recursion.
  (let ((response initial-response)
        have-result
        final-result)
    (while (not have-result)
      (let ((status (car response))
            (options (nth 1 response))
            (data (nth 2 response)))
        (case status
          (=
           (let ((next-packet
                  (catch 'continuing-request
                    (setq final-result
                          (rel-process-normal status options data))
                    (setq have-result t)
                    nil)))
             ;; setup next request
             (when next-packet
               (setq response (apply 'rel-do next-packet)))))
          ((! !!)
           (rel-process-error status options data))
          (t
           (error "Unknown/unsupported status: %S" status)))))
    final-result))

(defun rel-process-error (status options err)
  (let* ((err-type    (nth 0 err))
         (err-message (nth 1 err))
         (err-trace   (nth 2 err)))
    (rel-log-error "ERROR: %s" err-type)
    (rel-log-error "    %s" err-message)
    (dolist (trace-item err-trace)
      (rel-log-error "    %s" trace-item))
    (signal 'rel-ruby-error err-message)))

(defun rel-process-normal (status options data)
  (let ((transaction (cdr (assq 't options))))
    (if transaction
        ;; In a transaction; this will start a new request and start
        ;; recursing...
        (catch 'wrapped
          (let* ((outgoing-header 
                  (rel-make-header (list (cons 't transaction))))
                 (outgoing-data
                  (condition-case err
                      (rel-to-ruby
                       (if (assq 'e options)
                           ;; Flet used to start recursion early and
                           ;; maintain dynamic scope.
                           (flet ((+wrap+ ()
                                    (throw 'wrapped
                                      (rel-run outgoing-header nil))))
                             (rel-eval data))
                         data))
                    (error
                     ;; Errors passed back to Ruby first
                     (rel-log-error "Error with elisp: %S" err)
                     (format "raise %%q{%S}" err)))))
            ;; only here if not wrapped (throw'n out otherwise)
            ;; to save the stack we drop out here as well
            (throw 'continuing-request (list outgoing-header outgoing-data))))
      ;; Not in a transaction, no response possible
      (if (assq 'e options)
          (rel-eval data)
        data))))


(defun ruby-eval (ruby-expr &optional supress-error-message)
  "Send a RUBY-EXPR to the REL server to evaluate.  The result is returned
or an error is raised.  Because this may evaluate elisp, there could be
side-effects."
  (interactive "sRuby:")
  (condition-case err
      (message
       (prin1-to-string
        (rel-run (rel-make-header) ruby-expr)))
    (error
     (rel-log-error "%S" err)
     (when (not supress-error-message)
       (message "An error occured."))
     (resignal err))))

(defalias 'rel-ruby-eval 'ruby-eval)

;;;
;;; reli-* functions are for interaction from the REL server
;;;

;; XXX simulate via a faked response?
(defun reli-timeout-error (&optional seconds)
  (rel-log-error "Taking too long: stopping"))


(provide 'rel)