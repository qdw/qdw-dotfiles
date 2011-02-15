;; rel-ib - REL Interactive [Ruby] Buffer
;;
;; REL-IB is a test-case for REL.  It is designed to easy prototyping
;; and testing various things in Ruby. However, unlike IRB which is
;; line-based, REL-IB attempts to take on an approach similar to that
;; found in program such as Maple where code and results are in-line
;; with eachother.
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


(require 'ruby-mode) ;; PST -- needed?

(defun rel-ib-init ()
  (interactive)
  (rel-start)
  (set (make-local-variable 'rel-ib-transaction) nil)
  (set (make-local-variable 'rel-ib-counter) 0)
  (rel-ib-bind-keys))

(defun rel-ib-reset ()
  (interactive)
  (setq rel-ib-transaction nil)
  (setq rel-ib-counter 0))

(defun rel-ib-bind-keys ()
  (use-local-map (copy-keymap ruby-mode-map))
  (local-set-key (kbd "C-x C-e") 'rel-ib-eval-block)
  (local-set-key (kbd "C-c C-c") 'rel-ib-clear-output)
  (local-set-key (kbd "C-c C-r") 'rel-ib-clear-all-output))

;; PST -- currently hacked-down overkill
(defun rel-ib-eval-ruby (ruby-expr)
  "Returns a cons of (ERROR . RESULT) were ERROR is t or nil indicating if
an error occured and and RESULT is a string-representation of the
result of the evaluation (or error message)."
  (condition-case err
      (let* ((flags (cons '(p . "+") ;; request persistent
                          (when nil ;;rel-ib-transaction
                            (list (cons 't rel-ib-transaction)))))
             (chunk (rel-do (rel-make-header flags) ruby-expr))
             (options (nth 1 chunk)))
        (setq rel-ib-transaction (assq 't options))
        (cons nil (prin1-to-string
                   (rel-process chunk))))
    (rel-ruby-error
     (cons t (cdr err)))))

(defun ruby-eval-region (start end &optional print)
  "Use REL to evaluate a region."
  (interactive "r")
  (let ((code (buffer-substring start end)))
    (rel-ib-eval-ruby code)))

(defun ruby-toplevel-block-region ()
  "Returns a cons (START . STOP) of the range of a top-level block started
 with a keyword (such as class/module/def, etc).  The keywords and
 corresponding `end' must start on column-0. See ruby-beginning-of-defun
 and ruby-end-of-defun."
  (let ((start (point)))
    (save-excursion
      (let ((block-start
             ;; ruby-beginning-of-defun returns nil at end-of-buffer
             (when (ruby-beginning-of-defun)
               (point)))
            (block-end
             ;; ruby-end-of-defun returns ... deptruh? anyway, we get
             ;; non-0 at end-of-buffer
             (when (= 0 (ruby-end-of-defun))
               (skip-chars-backward " \t\n")
               (point))))
        (when (and block-start block-end)
          (when (and (>= start block-start) (<= start block-end))
            (cons block-start block-end)))))))

(defvar rel-ib-output-marker-re
  "#\\([0-9]+\\):\\(>>\\|!!\\) ")

(defun rel-ib-output-at-point ()
  (save-match-data
    (looking-at rel-ib-output-marker-re)))

(defun rel-ib-beginning-of-output ()
  "Returns the beginning of the output block that is either associated
with the current ruby block POINT is in, or in output block POINT is in."
  (save-excursion
    (save-match-data
      (if (save-excursion
            (beginning-of-line)
            (looking-at rel-ib-output-marker-re))
          ;; in output block
          (let (at)
            (beginning-of-line)
            (while (looking-at rel-ib-output-marker-re)
              (setq at (point))
              (forward-line -1))
            at)
        ;; not in output block
        (let ((region
               (or (ruby-toplevel-block-region)
                   (cons (point-at-bol) (point-at-eol)))))
          (goto-char (cdr region))
          (forward-line 1)
          (when (looking-at rel-ib-output-marker-re)
            (point)))))))

(defun rel-ib-clear-output ()
  (interactive)
  (let ((output-start (rel-ib-beginning-of-output)))
    (when output-start
      (save-excursion
        (save-match-data
          (goto-char output-start)
          (while (looking-at rel-ib-output-marker-re)
            (kill-whole-line 1)))))))

(defun rel-ib-clear-all-output ()
  (interactive)
  (save-excursion
    (delete-matching-lines
     (concat "^" rel-ib-output-marker-re) (point-min) (point-max))))

(defun rel-ib-eval-block ()
  (interactive)
  (let* ((region
          (or (ruby-toplevel-block-region)
              (cons (point-at-bol) (point-at-eol))))
         (start-point (point))
         (result
          (ruby-eval-region (car region) (cdr region)))
         (has-error (car result))
         (result-text (cdr result)))
    (save-excursion
      (setq rel-ib-counter (1+ rel-ib-counter))
      ;; math to attempt to offset buffer modifications during eval
      (goto-char (+ (cdr region)
                    (- (point) start-point)))
      (end-of-line)
      (let ((maybe-newline (if (looking-at " *\n") "" "\n"))
            (prelude (concat "\n#" (int-to-string rel-ib-counter) ":"
                             (if has-error "!! " ">> "))))
        (save-excursion
          (forward-line 1)
          (while (looking-at rel-ib-output-marker-re)
            (kill-whole-line 1)))
        (insert (concat prelude
                        (mapconcat 'identity
                                   (split-string result-text "\n")
                                   prelude)
                        maybe-newline))))))

(defun ruby-eval-line ()
  (interactive)
  (ruby-eval-region
   (point-at-bol) (point-at-eol)))


(provide 'rel-ib)