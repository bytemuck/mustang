(defun double (x)
    (+ x x))
(defun quadruple (x)
    (double (double x)))

(let d2 (double 2))
(let d4 (double 4))
(let q6 (quadruple 6))

(printfn d2)
(printfn d4)
(printfn q6)