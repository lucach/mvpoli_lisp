(defun varpowers (monomial)
    (fourth monomial)
)

(defun monomial-degree (monomial)
    (third monomial)
)

(defun monomial-coefficient (monomial)
    (second monomial)
)


(defun varpower-power (vp)
    (second vp)
)

(defun varpower-symbol (vp)
    (third vp)
)

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

(defun poly-monomials (p)
    (second p)
)

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

(defun vars-of-varpowers (varpowers)
    (if (null varpowers)
        nil
        (cons
            (third (first varpowers))
            (vars-of-varpowers (rest varpowers))
        )
    )
)

(defun vars-of (monomial)
    (vars-of-varpowers (varpowers monomial))
)

(defun compute-totaldegree (varpowers)
    (if (null varpowers)
        0
        (+ (second (first varpowers)) (compute-totaldegree (rest varpowers)))
    )
)

(defun expt-to-vp (expression)
    (if (listp expression)
        (list 'V (third expression) (second expression))
        (list 'V 1 expression)
    )
)

(defun parse-varpowers (expression)
    (mapcar #'expt-to-vp expression)
)

(defun lexicographicallyCompareVPs (VP1 VP2)
    (let (
            (var1 (varpower-symbol VP1))
            (var2 (varpower-symbol VP2))
         )
        (string< var1 var2)
    )
)

(defun parse-varpowers-and-sort (expression)
    (sort (parse-varpowers expression) 'lexicographicallyCompareVPs)
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
