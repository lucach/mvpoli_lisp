; as-monomial

(equal (as-monomial 42) '(M 42 0 NIL))
(equal (as-monomial '(* y (expt s 3) (expt t 3))) '(M 1 7 ((V 3 S) (V 3 T) (V 1 Y))) )
(equal (as-monomial '(* 3 y w (expt t 3))) '(M 3 5 ((V 3 T) (V 1 W) (V 1 Y))) )
