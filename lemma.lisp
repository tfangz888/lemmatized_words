;; (list-all-packages)
(in-package :cl-user)

(ql:quickload "lol-re")
(defpackage :words
  (:use :cl :lol-re #:iterate))
(in-package :words)

;; get first word of one line. it is the headword
;; return a string
(defun headword (string)
    (let ((headword (m~ "^[a-zA-Z]+[-]?[a-zA-Z]*"  string)))
        headword))

;; get refered words
(defun find-refer-patterns(string)
    (iter (for match in-matches-of string using (m~ "([a-zA-Z]+[-]?[a-zA-Z]*) -> \\[([a-zA-Z]+[-]?[a-zA-Z]*)\\]"))
      (collect `(,$1 ,$2))))
;;(find-refer-patterns "abandon   abandoned, abandoning, abandons")
;;(find-refer-patterns "abide   abided, abides, abiding, abidingly, abode -> [abode]")
;;(find-refer-patterns "abode -> [abide]   abodes")
;;(find-refer-patterns "abstract   abstracted -> [abstracted], abstracting, abstractly -> [test], abstractness, abstracts")

;; get related wrods
(defun find-related-word(string)
    (iter (for match in-matches-of string using (m~ "[ ][a-zA-Z]+[-]?[a-zA-Z]*"))
      (collect (string-trim " " match))))
;;(find-related-word "abandon   abandoned, abandoning, aban-dons")
;;(find-related-word "abide   abided, abides, abiding, abidingly, abode -> [abode]")
;;(find-related-word "abode -> [abide]   abodes")
;;(find-related-word "abstract   abstracted -> [abstracted], abstracting, abstractly, abstractness, abstracts")

(defun string-to-symbol (word)
    (intern (string-upcase word)))

(defun related-pairs (related-words headword)
    (mapcar #'(lambda (x) (reverse (cons  headword `(,x) )))  related-words))
;; (related-pairs (find-related-word "abide   abided, abides, abiding, abidingly, abode -> [abode]") "abide")

(defparameter *all-symbols* ())

(defun update-value-of-symbol (symbol value)
    (let* ((original-value (if (boundp symbol) (symbol-value symbol) ()))
           (new-value (union value original-value)))
        (setf (symbol-value symbol) new-value)
        (setf *all-symbols* (cons symbol *all-pairs2*))))

(defun make-word-as-symbol (pair)
    (let ((symbol (string-to-symbol (car pair)))
          (value (string-to-symbol (cadr pair))))
        (update-value-of-symbol symbol (list value))))

;; get one line of all pairs
(defun get-pairs-of-oneline (line)
    (let* ((headword (headword line))
           (related-words (cons headword (find-related-word line)))
           (refer-pairs (find-refer-patterns line))
           (related-pairs (mapcar #'(lambda (x) (list x headword)) related-words)))
        (union related-pairs  refer-pairs)))
;;(create-symbols-for-oneline "abstract   abstracted -> [abstracted], abstracting, abstractly -> [test], abstractness, abstracts")
;;=>
;;(("abstracts" "abstract") ("abstractness" "abstract") ("abstractly" "abstract") ("abstracting" "abstract") ("abstracted" "abstract") ("abstract" "abstract")
;; ("abstracted" "abstracted") ("abstractly" "test"))


;; read processed file into lines
(defvar lines (with-open-file (file "~/dict/lemmatized/2+2+3lem2.txt")
    (loop for line = (read-line file nil)
        while line collect line)) )


(defparameter *all-pairs* (with-open-file (file "~/dict/lemmatized/2+2+3lem2.txt")
    (loop for line = (read-line file nil)
        while line collect (get-pairs-of-oneline line))))

(defparameter *all-pairs2* ())

(dolist (x *all-pairs*) (setf *all-pairs2* (union x *all-pairs2*)))

(mapcar #'make-word-as-symbol *all-pairs2*)
