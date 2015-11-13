(defun check (g q c)
                  (if (null g)
                      nil
                    (if (not(is-member(car g) c))
                        nil ;If he his not member do this
                      (if (equal(triple-first (car g)) c) ;Must check if unique
                          (if (is-member(triple-first(car g)) q)
                              (check (cdr g) q v c)
                            (triple-first(car g)))))))

(defun check (g q c)
                   (if (null g)
                       nil
                     (if (is-member (car g) c)
                         (if (equal (car (car g)) c)
                             (if(is-member q (triple-third (car g)))
                                 (check (cdr g) q c)
                               (triple-third(car g)))
                           (if(is-member q (triple-first (car g)))
                               (check(cdr g) q c)
                             (triple-first (car g)))))))

(check '((a 1 b) (a 2 c) (c 4 d) (b 3 d) (d 5 e)) '(a b) 'a)

(defun check (g q c)
                   (if (null g)
                       nil
                     (if (is-member (car g) c)
                         (if (equal (car (car g)) c)
                             (if(is-member q (triple-third (car g)))
                                 (check (cdr g) q c)
                               (triple-third(car g)))
                           (if(is-member q (triple-first (car g)))
                               (check(cdr g) q c)
                             (triple-first (car g))))
                       (check (cdr g) g c))))
