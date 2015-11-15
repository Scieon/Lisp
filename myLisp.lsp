;; 2.
(defun make-graph()
	(cdr nil))
	;(cons nil nil)

;; 3.
(defun is-triple(l)
  (if(or(not(listp l))(null l))
      nil
    (equal (length l) 3)))

;; Auxiliary Function
(defun length2(lst)
  (cond ((null lst) 0)
        (t(+ 1 (length2(cdr lst))))))

;; 4.
(defun triple-has-node(l e) ;This one looks more aesthetic
  (if (is-triple l)
      (cond ((equal (car l) e) t)
            ((equal (car (cdr l )) e) t)
            ((equal (car (cdr (cdr l))) e) t)
            (t nil))
    nil))

;; 5.
(defun is-member(lst e)
  (cond ((or(null lst)(not (listp lst))) nil)
        ((equal (car lst) e) t)
        (t (is-member(cdr lst) e))))

;; 6.
(defun push-unique(lst e)
  (cond ((not (listp lst)) nil)
        ((is-member lst e) lst)
        (t(append lst (list e)))))


;; 7.
(defun filter-visited (g e)
  (if (or (not(listp g))(null g))
      nil
    (if (is-member (car g) e)
        (filter-visited (cdr g) e)
      (cons (car g) (filter-visited(cdr g) e)))))

;; 8.
(defun filter-all-visited (g els)
  (cond ((or (not (listp g)) (null g)) nil)
        ((null els) g)
        (t (filter-all-visited (filter-visited g (car els)) (cdr els)))))

;; 9.
(defun remove-from-lst(lst el)
  (cond ((or (not (listp lst)) (null lst)) nil) ;if g is the empty list or g is not even a list, return nil.
        ((equal(car lst) el)(remove-from-lst(cdr lst) el))
        (t(cons (car lst)(remove-from-lst(cdr lst)el)))))


;; 10.
(defun remove-all-from-lst (lst1 lst2)
  (if (null lst2)
      lst1
    (remove-all-from-lst(remove-from-lst lst1 (car lst2)) (cdr lst2))))


;; 11.
(defun triple-first (triple)
  (if (is-triple triple)
      (car triple)
    nil))


;; 12.
(defun  triple-third (triple)
  (if (is-triple triple)
      (car (cdr (cdr triple)))
    nil))


;; 13.
(defun triple-to-els(g)
  (cond ((null g) nil)
        (t(append (append (list (triple-first (car g))) (list (triple-third (car g)))) (triple-to-els (cdr g))))))



;;;; Bonus Functions

;;; This function attempts to find the next node in a BFS search and returns it only if it has not yet been visited or on the search queue.
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


;;; This function determines if the next node found in a BFS is unique to the search queue or the visited list.
(defun unique-match(node q v c)
  (if (and (not (equal node c))(not(is-member q node)) (not(is-member v node)))
      t
    nil))


(defun bfs(g e q v c)
  (if (equal e c)
      (push-unique v c)
    (if (and (null q) (null(check g q v c)))
        nil
      (if(null(check g q v c))
          (bfs g e (cdr q)(push-unique v (car q))(car q))
        (bfs g e (push-unique q (check g q v c)) v c)))))


   

    
            




