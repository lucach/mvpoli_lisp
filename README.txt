mvpoli_lisp
-----------

This is an implementation of a Common Lisp library to handle polynomials.

Author
------
Luca Chiodini (luca <at> chiodini <dot> org)

Development
-----------
This library has been developed using GIT versioning system. It has been tested
using sbcl (SBCL 1.3.3).
Report of "unit tests" is available at https://jenkins.chiodini.org/ .

Usage
-----
Many functions are provided to do basic algebraic operations with polynomials.
Here are some examples:

- as_monomial parses a superficial representation of a monomial and returns it
  in the standard form.

  * (as-monomial '(* y (expt s 3) (expt t 3)))
  (M 1 7 ((V 3 S) (V 3 T) (V 1 Y)))

- as_polynomial parses a superficial representation of a polynomial and returns
  it in the standard form.

  * (as-polynomial '(+ (* -1 x) (* x y))) 
  (POLY ((M -1 1 ((V 1 X))) (M 1 2 ((V 1 X) (V 1 Y)))))

- coefficients returns a list of the coefficients appearing in a polynomial.

  * (coefficients '(POLY ((M -4 0 NIL) (M 1 2 ((V 1 X) (V 1 Y))))))
  (-4 1)

- variables returns a list of all variable symbols appearing in a polynomial.

  * (variables '(+ (* -1 (expt x 2)) (* 2 x y))) 
  (X Y)

- maxdegree returns the maximum degree of a monomial appearing in the
  polynomial.

  * (maxdegree '(+ (* 5 y z) (* 4 (expt x 2) y) (* 4 r (expt w 3))))
  4

- mindegree returns the minium degree of a monomial appearing in the
  polynomial.

  * (mindegree '(+ (* 5 y z) (* 4 (expt x 2) y) (* 4 r (expt w 3))))
  2

- polyval returns the value of the polynomial computed in the n-dimensional
  point provided as second argument.

  * (polyval '(+ (* (expt x 2) y) (* 3 x)) '(2 2))
  14

- polyplus returns the polynomial sum of the two polynomials provided.

  * (polyplus '(+ (* x) (* y)) '(+ (* 2 x) (* -1 y)))
  (POLY ((M 3 1 ((V 1 X)))))

- polyminus returns the polynomial difference of the two polynomials
  provided.

  * (polyminus '(+ (* x) (* y)) '(+ (* x) (* y)))
  (POLY ())

- polytimes returns the polynomial product of the two polynomials provided.

  * (polytimes '(* (expt x 2)) '(+ (* x) (* y)))
  (POLY ((M 1 3 ((V 2 X) (V 1 Y))) (M 1 3 ((V 3 X)))))

- monomials/2 returns a list of monomials appearing in a polynomial.

  * (monomials '(+ (* x) (* x)))
  ((M 2 1 ((V 1 X))))

