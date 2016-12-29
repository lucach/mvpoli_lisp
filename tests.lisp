;;;; as-monomial

(equal (as-monomial '(* 3 y w (expt t 3))) '(M 3 5 ((V 3 T) (V 1 W) (V 1 Y))) )
(equal (as-monomial '(* y (expt s 3) (expt t 3))) '(M 1 7 ((V 3 S) (V 3 T) (V 1 Y))) )
(equal (as-monomial 42) '(M 42 0 NIL))
(equal (as-monomial '(* (* 2 (cos 0)) x)) '(M 2.0 1 ((V 1 X))))
(equal (as-monomial '(cos 0)) '(M 1.0 0 NIL))
(equal (as-monomial '(* (expt x 0))) '(M 1 0 NIL))
(equal (as-monomial '(* (expt x 3) (expt y 0) (expt z 2))) '(M 1 5 ((V 3 X) (V 2 Z))))
(equal (as-monomial '(* (expt a 2) (expt a 2))) '(M 1 4 ((V 4 A))) )
(equal (as-monomial '(* (expt radius 2))) '(M 1 2 ((V 2 RADIUS))))
(equal (as-monomial '(* -1 x)) '(M -1 1 ((V 1 X))))
(equal (as-monomial '(* -2 x)) '(M -2 1 ((V 1 X))))

;;;; coefficients

(equal (coefficients '(POLY ((M 1 3 ((V 3 X))))) ) '(1))
(equal (coefficients '(* (expt x 3))) '(1))
(equal (coefficients '(POLY ((M -4 0 NIL) (M 1 2 ((V 1 X) (V 1 Y))) (M 1 7 ((V 3 S) (V 3 T) (V 1 Y)))))) '(-4 1 1))

;;;; variables

(equal (variables '(POLY ((M -1 1 ((V 1 X))) (M 1 2 ((V 1 X) (V 1 Y)))))) '(X Y))
(equal (variables '(POLY ((M 5 2 ((V 1 Y) (V 1 Z))) (M 4 3 ((V 2 X) (V 1 Y))) (M 4 4 ((V 1 R) (V 3 W)))))) '(R W X Y Z))
(equal (variables '(+ (* -1 (expt x 2)) (* 2 x y))) '(X Y))

;;;; maxdegree

(equal (maxdegree '(POLY ((M 5 2 ((V 1 Y) (V 1 Z))) (M 4 3 ((V 2 X) (V 1 Y))) (M 4 4 ((V 1 R) (V 3 W)))))) 4)
(equal (maxdegree '(+ (* 5 y z) (* 4 (expt x 2) y) (* 4 r (expt w 3)))) 4)
(equal (maxdegree '(POLY ())) 0)

;;;; mindegree

(equal (mindegree '(POLY ((M 5 2 ((V 1 Y) (V 1 Z))) (M 4 3 ((V 2 X) (V 1 Y))) (M 4 4 ((V 1 R) (V 3 W)))))) 2)
(equal (mindegree '(+ (* 5 y z) (* 4 (expt x 2) y) (* 4 r (expt w 3)))) 2)
(equal (mindegree '(POLY ())) 0)

;;;; as-polynomial

(equal (as-polynomial '(+ (* (expt y 4) z (expt x 5)) (* -1 y z r) (* (expt y 4) r (expt z 5)))) '(POLY ((M -1 3 ((V 1 R) (V 1 Y) (V 1 Z))) (M 1 10 ((V 1 R) (V 4 Y) (V 5 Z))) (M 1 10 ((V 5 X) (V 4 Y) (V 1 Z))))))
(equal (as-polynomial '(+ (* y (expt s 3) (expt t 3)) -4 (* x y))) '(POLY ((M -4 0 NIL) (M 1 2 ((V 1 X) (V 1 Y))) (M 1 7 ((V 3 S) (V 3 T) (V 1 Y))))))
(equal (as-polynomial '(+ (* -1 x) (* x y))) '(POLY ((M -1 1 ((V 1 X))) (M 1 2 ((V 1 X) (V 1 Y))))))
(equal (as-polynomial '(+ (* x (expt y 2)) (* (expt y 2) x))) '(POLY ((M 2 3 ((V 1 X) (V 2 Y))))))
(equal (as-polynomial '(+ (* a c) (* (expt a 2)) (* a b) (* a))) '(POLY ((M 1 1 ((V 1 A))) (M 1 2 ((V 1 A) (V 1 B))) (M 1 2 ((V 1 A) (V 1 C))) (M 1 2 ((V 2 A))))))
(equal (as-polynomial '(+ (* 0 a) (* x))) '(POLY ((M 1 1 ((V 1 X))))))
(equal (as-polynomial '(+ (* (expt a 2)) (* a c) (* a) (* a z))) '(POLY ((M 1 1 ((V 1 A))) (M 1 2 ((V 1 A) (V 1 C))) (M 1 2 ((V 1 A) (V 1 Z))) (M 1 2 ((V 2 A))))))
(equal (as-polynomial '(+ 0 0)) '(POLY NIL))

;;;; polyval

(equal (polyval '(* (expt x 2) y) '(2 2)) 8)
(equal (polyval '(+ (* (expt x 2) y) (* 3 x)) '(2 2)) 14)
(equal (polyval '(M 1 2 ((V 2 A))) '(3)) 9)

;;;; polyplus

(equal (polyplus '(m 2 1 ((v 1 a))) '(m 3 1 ((v 1 a)))) '(POLY ((M 5 1 ((V 1 A))))) )
(equal (polyplus '(m 3 1 ((v 1 a))) '(m -3 1 ((v 1 a)))) '(POLY ()) )
(equal (polyplus '(* 3 a) '(POLY ())) '(POLY ((M 3 1 ((V 1 A))))))
(equal (polyplus '(+ (* x) (* y)) '(+ (* 2 x) (* -1 y))) '(POLY ((M 3 1 ((V 1 X))))))

;;;; polyminus

(equal (polyminus '(m 2 1 ((v 1 a))) '(m 3 1 ((v 1 a)))) '(POLY ((M -1 1 ((V 1 A))))) )
(equal (polyminus '(m 3 1 ((v 1 a))) '(m 3 1 ((v 1 a)))) '(POLY ()) )
(equal (polyminus '(* 3 a) '(POLY ())) '(POLY ((M 3 1 ((V 1 A))))))
(equal (polyminus '(+ (* x) (* y)) '(+ (* x) (* y))) '(POLY ()))

;;;; polytimes

(equal (polytimes '(m 1 7 ((v 3 s) (v 3 t) (v 1 y))) '(POLY ((M -1 1 ((V 1 X))) (M 1 2 ((V 1 X) (V 1 Y))))) ) '(POLY ((M -1 8 ((V 3 S) (V 3 T) (V 1 X) (V 1 Y))) (M 1 9 ((V 3 S) (V 3 T) (V 1 X) (V 2 Y))))) )
(equal (polytimes '(POLY ((m 1 1 ((V 1 A))) (m 1 1 ((V 1 B))))) '(POLY ((m 1 1 ((V 1 A))) (m 1 1 ((V 1 B))))) ) '(POLY ((M 2 2 ((V 1 A) (V 1 B))) (M 1 2 ((V 2 A))) (M 1 2 ((V 2 B))))) )
(equal (polytimes '(* (expt x 2)) '(+ (* x) (* y))) '(POLY ((M 1 3 ((V 2 X) (V 1 Y))) (M 1 3 ((V 3 X))))) )

;;;; monomials

(equal (monomials '(POLY ((M 1 2 ((V 1 Y) (V 1 Z))) (M 1 2 ((V 1 A) (V 1 B)))))) '((M 1 2 ((V 1 A) (V 1 B))) (M 1 2 ((V 1 Y) (V 1 Z)))) )
(equal (monomials '(+ (* x) (* x))) '((M 2 1 ((V 1 X)))) )
