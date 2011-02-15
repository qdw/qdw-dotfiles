;;;
;;; Turn elisp data into Ruby structures.
;;;


;;;
;;; The originals deal with passing around objects.  The replacements
;;; are much simpler in that they pass everything literally. Don't
;;; like it, add your own. The server/client infractucture is too limiting for
;;; me to see why it would useful to implement something more complex.
;;;
;;; Unless otherwise noted, a these functions take in an ELISP value
;;; and return a corresponding RUBY expression.
;;;


;;; XXX - is not very smart. Will break.
(defun rel-to-string (string)
  "Convert STRING to Ruby string expression."
  (concat "%q{" string "}"))

(defun rel-to-symbol (symbol)
  "Turn SYMBOL into a Ruby symbol expression."
  (concat ":" (prin1-to-string
               (symbol-name symbol))))

;;; XXX - can cause circular loop
(defun rel-list-to-native-array (list)
  "Turn LIST to a Ruby Array expression."
  (concat
   "["
   (mapconcat (lambda (i)
                (rel-to-ruby i))
              list
              ", ")
   "]"))

(defun rel-cons-to-native-array (cons)
  "Turn CONS into a Ruby Array expression."
  (rel-list-to-native-array
   (list (car cons) (cdr cons))))

;;; XXX - can cause circular loop
(defun rel-alist-to-native-hash (alist)
  "Turn ALIST into a Ruby Hash expression. This function signal an error if
ALIST is not an alist."
  (concat
   "{"
   (mapconcat (lambda (hk) ;hk should be (H . K)
                (concat (rel-to-ruby (car hk))
                        " => "
                        (rel-to-ruby (cdr hk))))
              alist
              ", ")
   "}"))

(defun rel-simple-cons-p (cons)
  "Unlike `consp' which will return t on '(1 2 3) this will only return t
only if the cdr is a not also list."
  (and (consp cons)
       (not (listp (cdr cons)))))

(defun rel-simple-alist-p (list)
  "Try to guess is a LIST is an alist that is, if the first element is a
simple cons cell, see `rel-simple-cons-p'."
  (and (listp list)
       (rel-simple-cons-p (car list))))

(defun rel-to-ruby (object &optional use-native)
  "Convert OBJECT into a Ruby expression.  A signal will be raised is the
conversion can not be performed."
  (cond ((not object)
	 "nil")
        ((eq object t)
	 "true")
        ((numberp object)
	 (number-to-string object))
        ((stringp object)
	 (rel-to-string object))
        ((symbolp object)
         (rel-to-symbol object))
	;; XXX looks like passing around object IDs again
;;        ((rel-rubyexpr-p obj)
;;	 (rel-rubyexpr-string obj))
;;        ((rel-rubyobj-p obj)
;;         (format "rel_rubyobj_stock.id2obj(%s)"
;;                 (rel-rubyobj-id obj)))
;;         ((rel-proper-list-p obj)
;;          (format "rel_elobject_new(%d, ELListCell)"
;;                  (rel-lisp-object-to-id obj)))
;;         ((and (consp obj) (atom (cdr obj)))
;;          (format "rel_elobject_new(%d, ELConsCell)"
;;                  (rel-lisp-object-to-id obj)))
;;         ((vectorp obj)
;;          (format "rel_elobject_new(%d, ELVector)"
	;;                 (rel-lisp-object-to-id obj)))
        ;; XXX - should likely use special objects to represent
        ((rel-simple-alist-p object)
         (rel-alist-to-native-hash object))
        ((rel-simple-cons-p object)
         (rel-cons-to-native-array object))
        ((list object)
         (rel-list-to-native-array object))
        (t
	 (error (format "Can't handle this data-type: [[%S]]" object)))
	))



(defsubst rel-string-to-rubystr (str)
  (let ((file-read "File.read(conf.temp_file)"))
    (if (or (not rel-treat-ctrl-codes)
            (string= str file-read))
        (concat "%q" (prin1-to-string (rel-no-properties obj)))
      (cond ((eq rel-treat-ctrl-codes 'use-file) ;experimental
             ;; !FIXME! coding-system @ XEmacs
             (with-temp-buffer
               (insert str)
               ;; suppress "wrote file-name" message
               ;; (find-efunctiondescr 'write-region "VISIT is neither")
               (write-region 1 (point-max) rel-temp-file nil 0))
             file-read)
            (t
	      (concat "%Q"
                      (with-temp-buffer
                        (insert (prin1-to-string (rel-no-properties str)))
                        (mapcar (lambda (x)
                                  (goto-char 1)
                                  (while (search-forward (car x) nil t)
                                    (replace-match (cdr x))))
                                '(("#" . "\\\\#")
                                  ("\003" . "\\\\cc")
                                  ("\004" . "\\\\cd")
                                  ("\021" . "\\\\cq")
                                  ("\023" . "\\\\cs")
                                  ("\026" . "\\\\cv")
                                  ("\027" . "\\\\cw")
                                  ("\031" . "\\\\cy")
                                  ("\032" . "\\\\cz")
                                  ))
                        (buffer-string)))
      
      )))))


(defun rel-proper-list-p (expression)
  ;; Tell if a list is proper, id est, that it is `nil' or ends with `nil'.
  (cond ((not expression))
	((consp expression) (not (cdr (last expression))))))

(defun rel-lisp2ruby (obj)
  (cond ((eq obj nil) "nil")
        ((eq obj t) "true")
        ((numberp obj) (number-to-string obj))
        ((stringp obj) (rel-string-to-rubystr obj))
        ((rel-rubyexpr-p obj) (rel-rubyexpr-string obj))
        ((rel-rubyobj-p obj)
         (format "rel_rubyobj_stock.id2obj(%s)"
                 (rel-rubyobj-id obj)))
        ((rel-proper-list-p obj)
         (format "rel_elobject_new(%d, ELListCell)"
                 (rel-lisp-object-to-id obj)))
        ((and (consp obj) (atom (cdr obj)))
         (format "rel_elobject_new(%d, ELConsCell)"
                 (rel-lisp-object-to-id obj)))
        ((vectorp obj)
         (format "rel_elobject_new(%d, ELVector)"
                 (rel-lisp-object-to-id obj)))
        (t
         (format "rel_elobject_new(%d)"
                 (rel-lisp-object-to-id obj)))
        ))

(defun rel-rubyexpr-p (rubyexpr)
  (and (listp rubyexpr) (eq (car rubyexpr) 'rel-rubyexpr)))

(defun rel-rubyexpr-string (rubyexpr)
  (cdr rubyexpr))

(defun rel-rubyexpr-quote (string)
  (cons 'rel-rubyexpr string))


(provide 'rel-to-ruby)