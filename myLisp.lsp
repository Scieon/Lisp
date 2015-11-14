
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

       




(defun firo (g e)
                   (if (null g)
                       nil
                     (if (is-member(car g) e)
                         (firo (cdr g) e)
                       (cons (car g) (firo (cdr g) e)))))

(defun firo2(g e)
  (if (or (null g) (not (listp g))) ;if g is the empty list or g is not even a list, return nil.
      nil
    (if (is-member(car g) e)
        (firo2 (cdr g) e)
      (cons (car g)(firo2(cdr g) e)))))


(defun alice (g e)
  (if (null e)
      g
    (alice (firo2 g (car e)) (cdr e))))

(defun teste (g e)
  (firo2 g (car e))) ; '((a 1 b) (c 2 d) (e 3 f) (g 4 h)) '(e g))


(defun teste2 (lst e)
  (len (teste lst (car e))))



(defun removal(lst el)
  (cond ((or (not (listp lst)) (null lst)) nil) ;if g is the empty list or g is not even a list, return nil.
        ((equal(car lst) el)(removal(cdr lst) el))
        (t(cons (car lst)(removal(cdr lst)el)))))


 ; (if (or (not (listp lst))(null lst))

(defun remove-all-from-lst (lst1 lst2)
  (if (null lst2)
      lst1
    (remove-all-from-lst(removal lst1 (car lst2)) (cdr lst2))))






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



(defun check(g q v c)
  (if (null g)
      nil
    (if (not (is-member (car g) c))
        (check (cdr g) q v c)
      (if (unique-match(triple-first (car g)) q v c)
         (triple-first(car g))
        (if(unique-match(triple-third(car g)) q v c) 
           (triple-third(car g))
          (check (cdr g) q v c))))))

(defun unique-match(node q v c)
  (if (and (not (equal node c))(not(is-member q node)) (not(is-member v node)))
      t
    nil))


(defun bfs2 (g e q v c) ;bad
  (if(null q)
      c
    (if (equal e c)
        (append v (list c))
      (if (not (equal nil (check g q v c)))
          (bfs2 g e (push-unique q (check g q v c)) v c)
        (bfs2 g e (cdr q) (push-unique v (cdr q)) (cdr q))))));handle length

(defun bfs3 (g e q v c) ;good
  (if (equal e c)
     (push-unique v c)
    (if (null (check g q v c))
        (bfs3 g e (cdr q) (push-unique v (car q)) (car q))
      (bfs3 g e (push-unique q (check g q v c)) v c))))

(defun bfs(g e q v c)
  (if (equal e c)
      (push-unique v c)
    (if (and (null q) (null(check g q v c)))
        nil
      (if(null(check g q v c))
          (bfs g e (cdr q)(push-unique v (car q))(car q))
        (bfs g e (push-unique q (check g q v c)) v c)))))


   

    
            




