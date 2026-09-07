(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil) ;; defining the db as a global variable
(defvar *dbpath* "./cds.db")

(defun add-record (cd) (push cd *db*))


(defun dump-db ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (clear-input *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))


;; TODO: I think it would be beneficial to move all of what I wrote below to just an aside in a literate document
;; I think same goes for the rest of this chapter, to migrate a lot of the things i wrote/said in this file over to the literate doc


(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))


;; This is due to lisps ancestry stemming from concepts established in the lambda calculus
;; question, I feel like some other less functional languages support something simmilar to this
;; how do you enforce the shape/behavior of the function that gets passed so this doesn't blow up?
;;
;; short answer as far as my understanding goes, you cant really do that, due to the lazy evaluation nature of lisp
;; the only way for this function to blow up is when it gets called and remove-if-not tries to operate on whatever the selector-fn returns
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun hello () (format t "the"))


;; Below is technically perfectly legal, will just cause a stack error because
;; it evaluates to be functionally the same as if I were to just pass an empty
;; value to the remove-if-not function
;; (select (hello))


;; below is basically the definition of a variable that is returning a function
;; in reality it is just a function that returns a function

;;(defun artist-selector (artist)
;;  #'(lambda (cd) (equal (getf cd :artist) artist)))
;;(defun album-selector (title)
;;  #'(lambda (cd) (equal (getf cd :title) title)))


;; we dont want to be doing the above though, that is clunky. Why do that when we can take advantage of
;; the metaprogramming available to us in lisp?

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title    (setf (getf row :title) title))
               (if artist   (setf (getf row :artist) artist))
               (if rating   (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*))))

;; so i think what is gonna happen is these above functions will get replaced by functions
;; that will somehow iterate over the structure of what a CD is supposed to be, and then
;; generate the mapcar etc statements based on that concept



;; it's important to build this out, bottom up.
;; In the selector function, our base case of every type of "if" branch we'd have would be some form of:
;; (equal (getf cd field) value)
;; so, we should make a function that when given the name of some field, and some value, builds out an expressioni like that
;; question, why is it not a macro?

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))


(defun make-comparisons-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))



