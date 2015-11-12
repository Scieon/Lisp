
(defparameter *student-names* "Anh Khoi")


(defparameter *student-surname* "Vu-Nguyen")


(defparameter *student-id* 27501072)


(defun make-graph()
	(cdr nil))
	;(cons nil nil)

(defun is-triple(l)
  (if(null l)
      nil
    (equal (length l) 3)))


;In case we need to define length
;(defun length2(lst)
                   ;(cond ((null lst) 0)
                       ; (t (+ 1 (length2(cdr lst))))))


(defun triple-has-nodes (l e)
  (if(is-triple l)
      (if (equal (car l) e)
          t
        (if (equal (car (cdr l)) e)
            t
          (if (equal (car (cdr (cdr l))) e)
              t
            nil)))))

(defun is-member(lst e)
  (cond ((null lst) nil)
        ((equal (car lst) e) t)
        (t (is-member(cdr lst) e))))





(defun push-unique(lst e)
  (if (is-member lst e)
      lst
    (append lst (list e))))

       


 (defun firoBAD (g e)
                  (if (null g) 
                      nil
                    (if (is-member(car g) e)
                        (cdr g)
                      (cons (car g)(firo (cdr g) e)))))

(defun firo (g e)
                   (if (null g)
                       nil
                     (if (is-member(car g) e)
                         (firo (cdr g) e)
                       (cons (car g) (firo (cdr g) e)))))



 (defun filter-visited (g e)
                  (if (null g) 
                      nil
                    (if (is-member(car g) e)
                        (cdr g)
                      (cons (car g)(filter-visited (cdr g) e))))) ;(filter-visited '((a 1 b) (b 2 c)(c 3 d)) 'a)


(defun remove-from-lst(lst el)
                   (cond ((null lst) nil)
                         ((equal(car lst) el) (remove-from-lst(cdr lst) el))
                         (t(cons(car lst)(remove-from-lst(cdr lst)el)))))

(defun triple-first (triple)
  (if (is-triple triple)
      (car triple)
    nil))

(defun  triple-third (triple)
  (if (is-triple triple)
      (car (cdr (cdr triple)))
    nil))

(defun triple-to-els(g)
  (cond ((null g) nil)
        (t(append (append (list (triple-first (car g))) (list (triple-third (car g)))) (triple-to-els (cdr g))))))


;(append (append (list 'a) (list 'b)) (list 'c))




