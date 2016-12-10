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
;; Returns T if vp is a well-formed varpower, nil otherwise.
;; A well-formed vp is a list in the form (V p s) where p is a non-negative
;; integer and s is a symbol.

(defun is-varpower (vp)
    (and (listp vp)
         (eq 'v (first vp))
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
;; Returns T if p is a well-formed monomial, nil otherwise.
;; A well-formed monomial is a list in the form (M coeff td VPs) where
;; td is a non-negative integer and VPs is a list of well-formed varpowers
;; (see is-varpower).

(defun is-monomial (m)
    (and (listp m)
        (eq 'm (first m))
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

;;      is-polynomial (p)
;; Returns T if p is a well-formed polynomial, nil otherwise.
;; A well-formed polynomial is a list in the form (POLY (M1 M2 ... Mn)) where
;; each Mi is a well-formed monomial (see is-monomial).

(defun is-polynomial (p)
    (and (listp p)
         (eq 'poly (first p))
         (let ((ms (poly-monomials p)))
            (and (listp ms)
                 (every #'is-monomial ms)
            )
         )
    )
)

;;      vars-of-varpowers (varpowers)
;; Given a list of varpowers in the form (VP1 VP2 ... VPn), returns a list
;; (S1 S2 ... Sn) where each Si is the symbol of the i-th VP.

(defun vars-of-varpowers (varpowers)
    (if (null varpowers)
        nil
        (cons
            (third (first varpowers))
            (vars-of-varpowers (rest varpowers))
        )
    )
)

;;      vars-of (monomial)
;; Given a monomial, returns a list of variable symbols appearing in its
;; varpowers.

(defun vars-of (monomial)
    (vars-of-varpowers (varpowers monomial))
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
;; Returns T when lexicographicallyCompareVP returns 0 or 1, nil otherwise.
;; In other words, this function "groups" together the cases when the VP1 has
;; to come before VP2 or when they are the same.
;; This "predicate" is needed by parse-varpowers-and-sort which uses the
;; standard sort function.

(defun booleanLexicographicallyCompareVP (VP1 VP2)
    (if (>= (lexicographicallyCompareVP VP1 VP2) 0)
        t
        nil
    )
)

;;      parse-varpowers-and-sort (expression)
;; Parses an expression using parse-varpowers, then sorts the result using
;; booleanLexicographicallyCompareVP.

(defun parse-varpowers-and-sort (expression)
    (sort (parse-varpowers expression) 'booleanLexicographicallyCompareVP)
)

(defun as-monomial (expression)
    (if (numberp expression)
        (list 'M expression 0 NIL)
        (if (numberp (second expression))
            (append
                (list 'M (second expression))
                (let
                    ((vp (parse-varpowers-and-sort (rest (rest expression)))))
                    (list (compute-totaldegree vp) vp)
                )
            )
            (append
                (list 'M 1)
                (let ((vp (parse-varpowers-and-sort (rest expression))))
                    (list (compute-totaldegree vp) vp)
                )
            )
        )
    )
)

;;      lexicographicallyCompareMonomials (VPs1 VPs2)
;; Returns T if lexicographicallyCompareVP applied to the first elements of
;; each list returns 1. Returns nil if lexicographicallyCompareVP returns -1.
;; Otherwise, when lexicographicallyCompareVP returns 0, this function
;; recursively calls itself discarding the first elements of the lists.

(defun lexicographicallyCompareMonomials (VPs1 VPs2)
    (let (
            (VP1 (first VPs1))
            (VP2 (first VPs2))
         )
        (cond
            ((null VP1) t)
            ((null VP2) nil)
            ((= (lexicographicallyCompareVP VP1 VP2) 1) t)
            ((= (lexicographicallyCompareVP VP1 VP2) -1) nil)
            (T (lexicographicallyCompareMonomials (rest VPs1) (rest VPs2)))
        )
    )
)

;;      degreeCompareMonomials (m1 m2)
;; Returns T if m1 has a total degree less than total degree of m2,
;; or nil if m1 has a total degree greater than total degree of m2.
;; When m1 and m2 have the same total degree, returns the result of
;; lexicographicallyCompareMonomials applied to the varpowers of m1 and m2.

(defun degreeCompareMonomials (m1 m2)
    (let (
            (deg1 (monomial-degree m1))
            (deg2 (monomial-degree m2))
         )
        (cond ((< deg1 deg2) t)
              ((> deg1 deg2) nil)
              (t (lexicographicallyCompareMonomials (varpowers m1) (varpowers m2)))
        )
    )
)

;;      poly-sort (monomials)
;; Sort a list of monomials using degreeCompareMonomials as comparator.

(defun poly-sort (monomials)
    (sort monomials 'degreeCompareMonomials)
)

;;      poly-reduce (monomials)
;; Given a list of monomials, returns the same monomials combining similar
;; terms.
;; Definition: two monomials are similar iff they share the same
;;             varpowers; i.e., they differ only from coefficient.
;; Two similar monomials can be compressed in one monomial whose
;; coefficient is the sum of the two original coefficient.
;; Note: this function assumes that the list is sorted using poly-sort.

(defun poly-reduce (monomials)
    (cond
        ((null monomials) nil)
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
                         (poly-reduce (cons
                                            (list 'M (+ c1 c2) deg vp)
                                            (rest (rest monomials))
                                      )
                         )

                    )
                    (cons m1 (poly-reduce (rest monomials))
                    )
                )
            )
        )
    )
)

;;      has-zero-coeff (m)
;; Given a monomial m, returns T if m has a zero coefficient, nil otherwise.

(defun has-zero-coeff (m)
    (= (monomial-coefficient m) 0)
)

;;      poly-remove-zero-coeff (monomials)
;; Given a list of monomials, returns the same list without monomials with a
;; zero coefficient.

(defun poly-remove-zero-coeff (monomials)
    (remove-if #'has-zero-coeff monomials)
)

;;      as-polynomial (expression)
;; Returns a polynomial, which is a list where the first element is P and the
;; second element is a list of valid monomials objects.
;; The expression has to be a list where the first element is the plus sign
;; "+" and the second element is a list of expressions as defined in
;; as-monomial.

(defun as-polynomial (expression)
    (cons 'POLY
        (poly-remove-zero-coeff (
            poly-reduce (poly-sort (mapcar #'as-monomial (rest expression)))
        ))
    )
)

;;      varpower-to-string (vp)
;; Returns a string representing vp. If vp has a trivial exponent, i.e. 1, it
;; is omitted. The full form is "variable^exponent".

(defun varpower-to-string (vp)
    (if (= (varpower-power vp) 1)
        (format nil "~A" (varpower-symbol vp))
        (format nil "~A^~D" (varpower-symbol vp) (varpower-power vp))
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
        (T (format nil "~A ~A" (varpower-to-string (first vps))
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
        (format nil "~A" (varpowers-to-string (varpowers m)))
        (format nil "~D ~A" (monomial-coefficient m)
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
            (format nil "~A" (monomial-to-string (first monomials)))
          )
          (T (format nil "~A + ~A" (monomial-to-string (first monomials))
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

;;      coefficients (p)
;; Returns a list where the i-th element is the coefficient of the i-th
;; monomial in polynomial p. The argument p can also be a single monomial.

(defun coefficients (p)
    (cond
        ((is-polynomial p) (mapcar #'monomial-coefficient (poly-monomials p)))
        ((is-monomial p) (list (monomial-coefficient p)))
        (T (error "COEFFICIENTS called with invalid argument"))
    )
)
