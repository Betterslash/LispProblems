(defun insert (lista elem pos)
    (cond 
        ((null lista) nil)
        ((equal(mod pos 2) 0) 
            (cons elem (cons (car lista) (insert (cdr lista) elem (+ 1 pos))))
        )
        (T (cons (car lista) (insert (cdr lista) elem (+ pos 1))))
    )
)

(defun rez (lista elem)
    (insert lista elem 1)
)


(defun add_list (lista1 lista2)
    (cond
        ((and (null lista1) (null lista2)) nil)
        ((not (null lista1))(cons (car lista1) (add_list (cdr lista1) lista2)))
        (T (cons (car lista2) (add_list lista1 (cdr lista2))))
    )
)

(defun per_lista (lista)
    (cond 
        ((null lista) nil)
        ((listp (car lista)) (add_list (per_lista (car lista)) (per_lista (cdr lista))))
        (T (cons (car lista)(per_lista (cdr lista))))
    )
)

(defun reve (lista lista1)
    (cond
        ((null lista) lista1)
        (T  (reve (cdr lista) (cons (car lista) lista1)))
    )
)

(defun gcdP (A B)
    (cond 
        ((and (not (numberp A)) (not (numberp B))) nil)
        ((not (numberp A)) B)
        ((not (numberp B)) A)
        ((equal A 0) B)
        ((equal B 0) A)
        ((> A B) (gcdP (- A B) B))
        ((< B A) (gcdP A (- B A)))
        (T (equal A B) A)
    )
)

(defun lenlista (lista)
    (cond 
        ((null lista) 0)
        (T (+ 1 (lenlista (cdr lista))))
    )
)

(defun gcd_lista (lista enter)
    (cond 
        ((equal (lenlista lista) 0) enter)
        (T(gcd_lista (cdr lista) (gcdP enter (car lista))))
    )
)

(defun occur (lista at)
    (cond
        ((null lista) 0)
        ((and (atom (car lista)) (equal (car lista) at)) (+ 1 (occur (cdr lista) at)))
        (T (occur (cdr lista) at))
    )
)

(print( rez '(1 2 3 4 5 6) 22))
(print(reve ( per_lista '(A (44 (11 2)) 22 33 C)) ()))
(print (gcd_lista '(C 33 22 242 121 44 A) 0))
(print (occur (per_lista'(1 (3 (5 4 3) (5 3)) 3 3 (3 1 3))) 3))

