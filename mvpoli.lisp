;;;; 806976 Chiodini Luca

;;      varpowers (monomial)
;; Returns the varpowers VPs of a well-formed monomial (M coeff td VPs).

(defun varpowers (monomial)
    (fourth monomial)
)

;;      monomial-degree (monomial)
;; Returns the degree of a well-formed monomial (M coeff td VPs).

(defun monomial-degree (monomial)
    (third monomial)
)

;;      monomial-coefficient (monomial)
;; Returns the coefficient of a well-formed monomial (M coeff td VPs).

(defun monomial-coefficient (monomial)
    (second monomial)
)

;;      varpower-power (vp)
;; Returns the power p of a well-formed varpower (V p s).

(defun varpower-power (vp)
    (second vp)
)

;;      varpower-symbol (vp)
;; Returns the symbol s of a well-formed varpower (V p s).

(defun varpower-symbol (vp)
    (third vp)
)

;;      is-varpower (vp)
;; Returns T if vp is a well-formed varpower, NIL otherwise.
;; A well-formed vp is a list in the form (V p s) where p is a non-negative
;; integer and s is a symbol.

(defun is-varpower (vp)
    (and (listp vp)
         (eq 'V (first vp))
         (let ((p (varpower-power vp))
               (v (varpower-symbol vp))
               )
            (and (integerp p)
               (>= p 0)
               (symbolp v)
            )
         )
    )
)

;;      is-monomial (m)
;; Returns T if p is a well-formed monomial, NIL otherwise.
;; A well-formed monomial is a list in the form (M coeff td VPs) where
;; td is a non-negative integer and VPs is a list of well-formed varpowers
;; (see is-varpower).

(defun is-monomial (m)
    (and (listp m)
        (eq 'M (first m))
        (let ((mtd (monomial-degree m))
              (vps (varpowers m))
             )
            (and (integerp mtd)
                 (>= mtd 0)
                 (listp vps)
                 (every #'is-varpower vps)
            )
        )
    )
)

;;      poly-monomials (p)
;; Returns the list of monomials in a well-formed polynomial p.

(defun poly-monomials (p)
    (second p)
)

;;      monomial-to-poly (m)
;; Given a well-formed monomial m, returns a well-formed polynomial in the
;; form (POLY (m)).

(defun monomial-to-poly (m)
    (list 'POLY (list m))
)

;;      monomials-to-poly (ms)
;; Given a list ms of well-formed monomials, returns a well-formed polynomial
;; in the form (POLY ms).

(defun monomials-to-poly (ms)
    (list 'POLY ms)
)

;;      is-polynomial (p)
;; Returns T if p is a well-formed polynomial, NIL otherwise.
;; A well-formed polynomial is a list in the form (POLY (M1 M2 ... Mn)) where
;; each Mi is a well-formed monomial (see is-monomial).

(defun is-polynomial (p)
    (and (listp p)
         (eq 'POLY (first p))
         (let ((ms (poly-monomials p)))
            (and (listp ms)
                 (every #'is-monomial ms)
            )
         )
    )
)

;;      vars-of (monomial)
;; Given a monomial, returns a list of variable symbols appearing in its
;; varpowers.

(defun vars-of (monomial)
    (if (is-monomial monomial)
        (mapcar #'varpower-symbol (varpowers monomial))
        (error "VARS-OF called with invalid argument")
    )
)

;;      compute-totaldegree (varpowers)
;; Returns the total degree of a monomial, i.e. the sum of the powers of each
;; varpower.

(defun compute-totaldegree (varpowers)
    (if (null varpowers)
        0
        (+ (second (first varpowers)) (compute-totaldegree (rest varpowers)))
    )
)

;;      expt-to-vp (expression)
;; When expression is (EXPT symbol exponent), returns (V exponent symbol).
;; Othewise, when expression is not a list, consider it as a symbol and thus
;; the function returns (EXPT 1 expression).

(defun expt-to-vp (expression)
    (if (listp expression)
        (list 'V (third expression) (second expression))
        (list 'V 1 expression)
    )
)

;;      parse-varpowers (expression)
;; Given an expression in the form (VP1 VP2 ... VPn), returns a well-formed
;; list (pVP1 pVP2 pVPn) where each pVPi is the result of parsing the i-th VP.

(defun parse-varpowers (expression)
    (mapcar #'expt-to-vp expression)
)

;;      exponentCompareVP (VP1 VP2)
;; Returns 0 if VP1 and VP2 have the same exponent.
;; Returns 1 if the exponent of VP1 is less than the exponent of VP2.
;; Returns -1 if the exponent of VP1 is greater than the exponent of VP2.

(defun exponentCompareVP (VP1 VP2)
    (let (
            (diff (- (varpower-power VP1) (varpower-power VP2)))
        )
        (cond ((= diff 0) 0)
              ((> diff 0) -1)
              ((< diff 0) 1)
        )
    )
)

;;      lexicographicallyCompareVP (VP1 VP2)
;; Returns 1 if the symbol of VP1 comes before the symbol of VP2 in a
;; lex order. Returns -1 if the former comes after the latter, in a lex
;; order. Returns the result of exponentCompareVP when the two symbols are
;; the same.

(defun lexicographicallyCompareVP (VP1 VP2)
    (let (
            (var1 (varpower-symbol VP1))
            (var2 (varpower-symbol VP2))
         )
        (cond
            ((string< var1 var2) 1)
            ((string> var1 var2) -1)
            (T (exponentCompareVP VP1 VP2))
        )
    )
)

;;      booleanLexicographicallyCompareVP (VP1 VP2)
;; Returns T when lexicographicallyCompareVP returns 0 or 1, NIL otherwise.
;; In other words, this function "groups" together the cases when the VP1 has
;; to come before VP2 or when they are the same.
;; This "predicate" is needed by parse-varpowers-and-sort which uses the
;; standard sort function.

(defun booleanLexicographicallyCompareVP (VP1 VP2)
    (if (>= (lexicographicallyCompareVP VP1 VP2) 0)
        T
        NIL
    )
)

;;      parse-varpowers-and-sort (expression)
;; Parses an expression using parse-varpowers, then sorts the result using
;; booleanLexicographicallyCompareVP.

(defun parse-varpowers-and-sort (expression)
    (sort (copy-seq (parse-varpowers expression))
          'booleanLexicographicallyCompareVP
    )
)

;;      eval-if-possible (expression)
;; Returns the evaluation of expression when there are no errors.
;; Otherwise returns NIL.

(defun eval-if-possible (expression)
    (ignore-errors (eval expression))
)

;;      has-zero-exp (vp)
;; Given a varpower vp, returns T if vp has a zero exponent, NIL otherwise.

(defun has-zero-exp (vp)
    (= (varpower-power vp) 0)
)

;;      remove-zero-exp (vps)
;; Given a list of varpowers, returns the same list without those with a zero
;; exponent.

(defun remove-zero-exp (varpowers)
    (remove-if #'has-zero-exp varpowers)
)

;;      simplify-similar-varpowers (varpowers)
;; Given a list of varpowers, returns the same varpowers combining varpowers
;; with the same symbol.
;; Two varpowers with the same symbol can be compressed in one varpower whose
;; exponent is the sum of the two original exponents.
;; Note: this function assumes that the list is sorted using poly-sort.

(defun simplify-similar-varpowers (varpowers)
    (cond
        ((null varpowers) NIL)
        ((null (rest varpowers)) varpowers)
        (T
            (let ((vp-s1 (varpower-symbol (first varpowers)))
                  (vp-s2 (varpower-symbol (second varpowers)))
                  (vp-p1 (varpower-power (first varpowers)))
                  (vp-p2 (varpower-power (second varpowers)))
                 )
                (if (equal vp-s1 vp-s2)
                    (simplify-similar-varpowers (cons
                            (list 'V (+ vp-p1 vp-p2) vp-s1)
                            (rest (rest varpowers))
                        )
                    )
                    (cons (first varpowers)
                          (simplify-similar-varpowers (rest varpowers))
                    )
                )
            )
        )
    )
)

;;      parse-monomial (expression)
;; Parses expression and returns a list with two elements. The first is the
;; number representing the coefficient. The second is a list of well-formed
;; varpowers.
;; Expression can be a single number (i.e., the coefficient) or a list where
;; the first element is '*', possibily followed by a number intepreted as
;; coefficient. The rest of the items in the list are varpowers, i.e. lists in
;; the form (expt varsymbol power) or varsymbols (with an implied power of 1).

(defun parse-monomial (expression)
    (if (numberp (eval-if-possible expression))
        (list (eval expression) NIL)
        (if (numberp (eval-if-possible (second expression)))
            (list
                (eval (second expression))
                (parse-varpowers-and-sort (rest (rest expression)))
            )
            (list
                1
                (parse-varpowers-and-sort (rest expression))
            )
        )
    )
)

;;      normalize-zero (coeff-and-vps)
;; Returns the argument unmodified if it has a non-zero coefficient.
;; Otherwise, returns the coefficient and the varpowers of the standard zero
;; monomial, i.e. (0 NIL).

(defun normalize-zero (coeff-and-vps)
    (if (= (first coeff-and-vps) 0)
        (list 0 NIL)
        coeff-and-vps
    )
)

;;      as-monomial (expression)
;; Parses expression with parse-monomial, then returns a well-formed monomial.
;; Varpowers with a zero exponent are removed and varpowers with the same
;; symbol are collapsed.
;; See is-monomial to get an explanation about well-formed monomials.

(defun as-monomial (expression)
    (let ((coeff-and-vps (normalize-zero (parse-monomial expression))))
        (list
            'M
            (first coeff-and-vps)
            (compute-totaldegree (second coeff-and-vps))
            (simplify-similar-varpowers
                (remove-zero-exp (second coeff-and-vps))
            )
        )
    )
)

;;      lexicographicallyCompareMonomials (VPs1 VPs2)
;; Returns T if lexicographicallyCompareVP applied to the first elements of
;; each list returns 1. Returns NIL if lexicographicallyCompareVP returns -1.
;; Otherwise, when lexicographicallyCompareVP returns 0, this function
;; recursively calls itself discarding the first elements of the lists.

(defun lexicographicallyCompareMonomials (VPs1 VPs2)
    (let (
            (VP1 (first VPs1))
            (VP2 (first VPs2))
         )
        (cond
            ((null VP1) T)
            ((null VP2) NIL)
            ((= (lexicographicallyCompareVP VP1 VP2) 1) T)
            ((= (lexicographicallyCompareVP VP1 VP2) -1) NIL)
            (T (lexicographicallyCompareMonomials (rest VPs1) (rest VPs2)))
        )
    )
)

;;      degreeCompareMonomials (m1 m2)
;; Returns T if m1 has a total degree less than total degree of m2,
;; or NIL if m1 has a total degree greater than total degree of m2.
;; When m1 and m2 have the same total degree, returns the result of
;; lexicographicallyCompareMonomials applied to the varpowers of m1 and m2.

(defun degreeCompareMonomials (m1 m2)
    (let (
            (deg1 (monomial-degree m1))
            (deg2 (monomial-degree m2))
         )
        (cond ((< deg1 deg2) T)
              ((> deg1 deg2) NIL)
              (T (lexicographicallyCompareMonomials
                    (varpowers m1) (varpowers m2))
              )
        )
    )
)

;;      poly-sort (monomials)
;; Sort a list of monomials using degreeCompareMonomials as comparator.

(defun poly-sort (monomials)
    (sort (copy-seq monomials) 'degreeCompareMonomials)
)

;;      simplify-similar-monomials (monomials)
;; Given a list of monomials, returns the same monomials combining similar
;; terms.
;; Definition: two monomials are similar iff they share the same
;;             varpowers; i.e., they differ only from coefficient.
;; Two similar monomials can be compressed in one monomial whose
;; coefficient is the sum of the two original coefficient.
;; Note: this function assumes that the list is sorted.

(defun simplify-similar-monomials (monomials)
    (cond
        ((null monomials) NIL)
        ((null (rest monomials)) monomials)
        (T
            (let ((m1 (first monomials))
                  (m2 (second monomials))
                 )
                (if (equal (varpowers m1) (varpowers m2))
                    (let ((c1 (monomial-coefficient m1))
                          (c2 (monomial-coefficient m2))
                          (deg (monomial-degree m1))
                          (vp (varpowers m1))
                         )
                         (simplify-similar-monomials (cons
                                            (list 'M (+ c1 c2) deg vp)
                                            (rest (rest monomials))
                                      )
                         )

                    )
                    (cons m1 (simplify-similar-monomials (rest monomials))
                    )
                )
            )
        )
    )
)

;;      has-zero-coeff (m)
;; Given a monomial m, returns T if m has a zero coefficient, NIL otherwise.

(defun has-zero-coeff (m)
    (= (monomial-coefficient m) 0)
)

;;      remove-zero-coeff (monomials)
;; Given a list of monomials, returns the same list without monomials with a
;; zero coefficient.

(defun remove-zero-coeff (monomials)
    (remove-if #'has-zero-coeff monomials)
)

;;      poly-reduce (monomials)
;; Retruns a list of monomials "reduced", i.e. similar monomials are
;; simplified using simplify-similar-monomials and monomials with a zero
;; coefficient are stripped.

(defun poly-reduce (monomials)
    (remove-zero-coeff (simplify-similar-monomials monomials))
)

;;      as-polynomial (expression)
;; Returns a polynomial, which is a list where the first element is P and the
;; second element is a list of valid monomials objects.
;; The expression has to be a list where the first element is the plus sign
;; "+" and the second element is a list of expressions as defined in
;; as-monomial.
;; The expression can also be the superficial representation of a single
;; monomial, which is parsed in turn with as-monomial.

(defun as-polynomial (expression)
    (if (and
            (listp expression)
            (equal (first expression) '+)
        )
        (monomials-to-poly
                (poly-reduce
                    (poly-sort (mapcar #'as-monomial (rest expression)))
                )
        )
        (monomials-to-poly
                (poly-reduce
                    (poly-sort (list (as-monomial expression)))
                )
        )
    )
)

;;      parse-if-possible (expr)
;; Returns a well-formed representation of the polynomial corresponding to
;; expr when possible. Otherwise returns NIL.

(defun parse-if-possible (expr)
    (ignore-errors (as-polynomial expr))
)

;;      varpower-to-string (vp)
;; Returns a string representing vp. If vp has a trivial exponent, i.e. 1, it
;; is omitted. The full form is "variable^exponent".

(defun varpower-to-string (vp)
    (if (= (varpower-power vp) 1)
        (format NIL "~A" (varpower-symbol vp))
        (format NIL "~A^~D" (varpower-symbol vp) (varpower-power vp))
    )
)

;;      varpowers-to-string (vps)
;; Given a list of varpowers vps, returns a string of them where each varpower
;; is "stringify"-ed using varpower-to-string. Varpowers are separated with a
;; space. No space is added after the last varpower.

(defun varpowers-to-string (vps)
    (cond
        ((null vps) "")
        ((null (rest vps)) (varpower-to-string (first vps)))
        (T (format NIL "~A ~A" (varpower-to-string (first vps))
                        (varpowers-to-string (rest vps))
            )
        )
    )
)

;;      monomial-to-string (m)
;; Returns a string that begins with the coefficient of m (omitted when it is
;; trivially 1). After that, varpowers of m are "stringify"-ed using
;; varpowers-to-string.

(defun monomial-to-string (m)
    (if (= (monomial-coefficient m) 1)
        (format NIL "~A" (varpowers-to-string (varpowers m)))
        (format NIL "~D ~A" (monomial-coefficient m)
                          (varpowers-to-string (varpowers m))
        )
    )
)

;;      monomials-to-string (monomials)
;; Returns a string where every monomial is "stringify"-ed using
;; monomial-to-string. Monomials are separated with the plus sign "+"
;; with a space before and after that sign. No sign is added after the last
;; monomial.

(defun monomials-to-string (monomials)
    (cond ((null monomials) "")
          ((null (rest monomials))
            (format NIL "~A" (monomial-to-string (first monomials)))
          )
          (T (format NIL "~A + ~A" (monomial-to-string (first monomials))
                                 (monomials-to-string (rest monomials))
             )
          )
    )
)

;;      pprint-polynomial (p)
;; Prints a traditional representation of p to stdout, then returns NIL.
;; This function uses monomials-to-string to get a string representing the
;; monomials in p.

(defun pprint-polynomial (p)
    (format t "~A" (monomials-to-string (poly-monomials p)))
)

;;      monomials (p)
;; Returns the (sorted, if not already) list of monomials appearing in p.
;; The argument p can also be a single monomial.

(defun monomials (p)
    (cond
        ((is-polynomial p)
            (if (null (poly-monomials p))
                (list '(M 0 0 NIL))
                (poly-sort (poly-monomials p))
            )
        )
        ((is-monomial p) (list p))
        (T (let ((parsed (parse-if-possible p)))
                (if (null parsed)
                    (error "MONOMIALS called with invalid argument")
                    (monomials parsed)
                )
            )
        )
    )
)

;;      coefficients (p)
;; Returns a list where the i-th element is the coefficient of the i-th
;; monomial in polynomial p. The argument p can also be a single monomial.

(defun coefficients (p)
    (cond
        ((is-polynomial p) (mapcar #'monomial-coefficient (monomials p)))
        ((is-monomial p) (list (monomial-coefficient p)))
        (T (let ((parsed (parse-if-possible p)))
                (if (null parsed)
                    (error "COEFFICIENTS called with invalid argument")
                    (coefficients parsed)
                )
            )
        )
    )
)

;;      maxdegree (p)
;; Returns the maximum degree among monomials in p. The argument p can also
;; be a single monomial.

(defun maxdegree (p)
    (cond
        ((is-polynomial p)
            (let ((ms (poly-monomials p)))
                (if (null ms)
                    0
                    (reduce #'max (mapcar #'monomial-degree ms))
                )
            )
        )
        ((is-monomial p) (monomial-degree p))
        (T (let ((parsed (parse-if-possible p)))
                (if (null parsed)
                    (error "MAXDEGREE called with invalid argument")
                    (maxdegree parsed)
                )
            )
        )
    )
)

;;      mindegree (p)
;; Returns the minimum degree among monomials in p. The argument p can also
;; be a single monomial.

(defun mindegree (p)
    (cond
        ((is-polynomial p)
            (let ((ms (poly-monomials p)))
                (if (null ms)
                    0
                    (reduce #'min (mapcar #'monomial-degree ms))
                )
            )
        )
        ((is-monomial p) (monomial-degree p))
        (T (let ((parsed (parse-if-possible p)))
                (if (null parsed)
                    (error "MINDEGREE called with invalid argument")
                    (mindegree parsed)
                )
            )
        )
    )
)

;;      polyplus (p1 p2)
;; Returns the polynomial sum of polynomials p1 and p2. Note that p1 and p2
;; can also be single monomials.

(defun polyplus (p1 p2)
    (cond
        ((is-monomial p1) (polyplus (monomial-to-poly p1) p2))
        ((is-monomial p2) (polyplus p1 (monomial-to-poly p2)))
        ((and (is-polynomial p1) (is-polynomial p2))
            (monomials-to-poly
                (poly-reduce (poly-sort (
                    append (poly-monomials p1) (poly-monomials p2))
                ))
            )
        )
        (T (let ((parsed1 (if (is-polynomial p1) p1 (parse-if-possible p1)))
                 (parsed2 (if (is-polynomial p2) p2 (parse-if-possible p2)))
                )
                (if (or (null parsed1) (null parsed2))
                    (error "POLYPLUS called with invalid arguments")
                    (polyplus parsed1 parsed2)
                )
            )
        )
    )
)

;;      negate-coeff (m)
;; Given a well-formed monomial m, returns the same monomial with the
;; negated coefficient.

(defun negate-coeff (m)
    (list 'M
        (* -1 (monomial-coefficient m)) (monomial-degree m) (varpowers m)
    )
)

;;      polyminus (p1 p2)
;; Returns the polynomial diff. of polynomials p1 and p2. Note that p1 and p2
;; can also be single monomials.

(defun polyminus (p1 p2)
    (cond
        ((is-monomial p1) (polyminus (monomial-to-poly p1) p2))
        ((is-monomial p2) (polyminus p1 (monomial-to-poly p2)))
        ((and (is-polynomial p1) (is-polynomial p2))
            (monomials-to-poly
                (poly-reduce
                    (poly-sort
                        (append
                            (poly-monomials p1)
                            (mapcar #'negate-coeff (poly-monomials p2))
                        )
                    )
                )
            )
        )
        (T (let ((parsed1 (if (is-polynomial p1) p1 (parse-if-possible p1)))
                 (parsed2 (if (is-polynomial p2) p2 (parse-if-possible p2)))
                )
                (if (or (null parsed1) (null parsed2))
                    (error "POLYMINUS called with invalid arguments")
                    (polyminus parsed1 parsed2)
                )
            )
        )
    )
)

;;      varpowers-reduce (varpowers)
;; Given a list of varpowers, returns the same varpowers combining those with
;; the same symbol.
;; Two varpowers that share the same symbol can be compressed in one varpower
;; whose exponent is the sum of the two original exponents.
;; Note: this function assumes that the list is sorted using
;; booleanLexicographicallyCompareVP.

(defun varpowers-reduce (varpowers)
    (cond
        ((null varpowers) NIL)
        ((null (rest varpowers)) varpowers)
        (T
            (let ((vp1 (first varpowers))
                  (vp2 (second varpowers))
                 )
                (if (equal (varpower-symbol vp1) (varpower-symbol vp2))
                    (let ((p1 (varpower-power vp1))
                          (p2 (varpower-power vp2))
                          (s (varpower-symbol vp1))
                         )
                         (varpowers-reduce (cons
                                                (list 'V (+ p1 p2) s)
                                                (rest (rest varpowers))
                                           )
                         )

                    )
                    (cons vp1 (varpowers-reduce (rest varpowers))
                    )
                )
            )
        )
    )
)

;;      monomial-times-monomial (m1 m2)
;; Returns the monomial coming from the product of m1 times m2. The resulting
;; monomial coefficient is the product of the coefficients, varpowers are the
;; union of the original varpowers (reduced if needed).

(defun monomial-times-monomial (m1 m2)
    (if (or (null m1) (null m2))
        NIL
        (let ((c1 (monomial-coefficient m1))
              (c2 (monomial-coefficient m2))
              (vp (varpowers-reduce
                    (sort
                        (copy-seq (append (varpowers m1) (varpowers m2)))
                        'booleanLexicographicallyCompareVP
                    )
                  )
              )
             )
             (list 'M (* c1 c2) (compute-totaldegree vp) vp)
        )
    )
)

;;      monomial-times-poly (m p)
;; Returns the polynomial coming from the product of the monomial m times the
;; polynomial p (given in form of list of monomials). The resulting polynomial
;; is the sum of m times each monomial in p.

(defun monomial-times-poly (m p)
    (if (null p)
        NIL
        (append
            (list (monomial-times-monomial m (first p)))
            (monomial-times-poly m (rest p))
        )
    )
)

;;      poly-times-poly (p1 p2)
;; Returns the polynomial coming from the product of p1 times p2. That product
;; is obtained as the sum of each monomial in p1 multiplied with p2 using
;; monomial-times-poly. The resulting polynomial is then sorted and reduced
;; with poly-reduce if needed.

(defun poly-times-poly (p1 p2)
    (if (null p1)
        NIL
        (poly-reduce
            (poly-sort
                (append
                    (monomial-times-poly (first p1) p2)
                    (poly-times-poly (rest p1) p2)
                )
            )
        )
    )
)

;;      polytimes (p1 p2)
;; Returns the polynomial product of p1 times p2. Note that p1 and p2 can
;; also be single monomials.

(defun polytimes (p1 p2)
    (cond
        ((is-monomial p1) (polytimes (monomial-to-poly p1) p2))
        ((is-monomial p2) (polytimes p1 (monomial-to-poly p2)))
        ((and (is-polynomial p1) (is-polynomial p2))
            (monomials-to-poly
                (poly-times-poly (poly-monomials p1) (poly-monomials p2))
            )
        )
        (T (let ((parsed1 (if (is-polynomial p1) p1 (parse-if-possible p1)))
                 (parsed2 (if (is-polynomial p2) p2 (parse-if-possible p2)))
                )
                (if (or (null parsed1) (null parsed2))
                    (error "POLYMINUS called with invalid arguments")
                    (polytimes parsed1 parsed2)
                )
            )
        )
    )
)

;;      variables(p)
;; Given a polynomial p, returns a list of variables appearing in every
;; monomial in p, sorted and without duplicates. Note that poly can also
;; be a single monomial.

(defun variables (p)
    (cond
        ((is-polynomial p)
            (sort
                (copy-seq
                    (remove-duplicates (mapcan #'vars-of (poly-monomials p)))
                )
                #'string<
            )
        )
        ((is-monomial p) (variables (monomial-to-poly p)))
        (T (let ((parsed (parse-if-possible p)))
                (if (null parsed)
                    (error "VARIABLES called with invalid argument")
                    (variables parsed)
                )
            )
        )
    )
)

;;      varpowerval(vp variables-with-values)
;; Given a varpower vp and an associative list of symbols-values, returns the
;; variable value (found as the second element in the associative list using
;; the symbol as key) raised to the power-th power.

(defun varpowerval (vp variables-with-values)
    (let
        ((base (rest (assoc (varpower-symbol vp) variables-with-values))))
        (expt base (varpower-power vp))
    )
)

;;      varpowersval(vps variables-with-values)
;; Given a list of varpowers vps and an associative list of symbols-values,
;; returns the product of all varpower values in vps, calculated using
;; varpowerval.

(defun varpowersval (vps variables-with-values)
    (if (null vps)
        1
        (* (varpowerval (first vps) variables-with-values)
           (varpowersval (rest vps) variables-with-values)
        )
    )
)

;;      monomialval(m variabes-with-values)
;; Given a monomial m and an associative list of symbols-values, returns the
;; product between the coefficient of m and the value of its varpowers
;; calculated using varpowersval.

(defun monomialval (m variables-with-values)
    (* (monomial-coefficient m)
       (varpowersval (varpowers m) variables-with-values)
    )
)

;;      monomialsval(ms variabes-with-values)
;; Given a list of monomials ms and an associative list of symbols-values,
;; returns the *sum* of values of every monomial in ms, calculated using
;; monomialval.

(defun monomialsval (ms variables-with-values)
    (if (null ms)
        0
        (+ (monomialval (first ms) variables-with-values)
           (monomialsval (rest ms) variables-with-values)
        )
    )
)

;;      polyval(p variablevalues)
;; Given a polynomial p and a list of variable values, returns the value of p
;; in the n-dimensional point represented by the list variablevalues. The i-th
;; value in variablevalues matches with the i-th variable resulting from
;; variables.
;; Polynomial p can also be a single monomial.
;; Exceeding values in variablevalues are ignored.

(defun polyval (p variablevalues)
    (cond
        ((is-polynomial p)
            (let ((l1 (list-length (variables p)))
                  (l2 (list-length variablevalues))
                 )
                (if (> l1 l2)
                    (error "POLYVAL called with invalid arguments")
                    (let ((new-variablevalues (subseq variablevalues 0 l1)))
                        (monomialsval (monomials p)
                            (pairlis (variables p) new-variablevalues)
                        )
                    )
                )
            )
        )
        ((is-monomial p) (polyval (monomial-to-poly p) variablevalues))
        (T (let ((parsed (parse-if-possible p)))
                (if (null parsed)
                    (error "POLYVAL called with invalid arguments")
                    (polyval parsed variablevalues)
                )
            )
        )
    )
)
