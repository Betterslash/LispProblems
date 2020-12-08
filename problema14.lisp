(defun parcurg_st (arb nv nm)
 (cond
    ((null arb) nil)
    ((= nv (+ 1 nm)) nil)
    (t (cons (car arb) (cons (cadr arb) (parcurg_st (cddr arb) (+ 1 nv) (+ (cadr arb) nm)))))
 )
)
(defun stang (arb)
 (parcurg_st (cddr arb) 0 0)
)

(defun parcurg_dreapta (arb nv nm)
 (cond
    ((null arb) nil)
    ((= nv (+ 1 nm)) (cons (car arb) (cons (cadr arb) (parcurg_dreapta (cddr arb) 1 0))))
    (T (parcurg_dreapta (cddr arb) (+ 1 nv) (+ (cadr arb) nm)))
 )
)

(defun drept (arb)
 (parcurg_dreapta (cddr arb) 0 0)
)

(defun post_order (arbore)
    (cond 
        ((null arbore) nil)
        (T (append (cons (post_order (stang arbore)) (post_order (drept arbore))) (list(car arbore))))
    )
)

(defun get_atoms (lista)
    (cond 
        ((null lista) nil)
        ((listp (car lista)) (append (get_atoms (car lista)) (get_atoms (cdr lista))))
        (T (cons (car lista) (get_atoms (cdr lista))))
    )
)

(defun t_post_order (arb)
    (get_atoms (post_order arb))
)

(print (t_post_order'(a 2 b 2 c 1 i 0 f 1 g 0 d 2 e 0 h 2 q 0 r 0)))