(defun sum (n1 n2)
    "Returns the sum of two nonnegative integers."
    (assert
        (and (integerp n1) (>= n1 0))
        (n1)
        "N1 must be a nonnegative integer, but it’s ~S."
        n1
    )
    (assert
        (integerp n2)
        (n2)
        "N2 must be an integer, instead it’s ~S."
        n2
    )
    (if (zerop n1) n2
        (1+ (sum (1- n1) n2))
    )
)

(defun product (n1 n2)
    "Returns the product of two integers."
    (assert
        (and (integerp n1) (>= n1 0))
        (n1)
        "N1 must be a nonnegative integer, instead it's ~S."
        n1
    )
    (assert 
        (integerp n2)
        (n2)
        "N2 must be an integer, instead it's ~S."
        n2
    )
    (if (= 1 n1) n2
        (sum n2 (product (1- n1) n2))
    )
)

(defun power (n i)
    "Returns the value n raised to the power i."
    (assert
        (and (integerp n) (>= n 0))
        (n)
        "N must be a nonnegative integer, instead it's ~S."
        n
    )
    (assert
        (and (integerp i) (>= i 0))
        (i)
        "I must be a nonnegative integer, instead it's ~S."
        i
    )
    (if (= 1 i) n
        (product n (power n (1- i)))
    )
)
