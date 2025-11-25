(let z 1)

(defun impure (x y)
    (+ x y z))

(defun fun (l)
    (map impure l)
)

(fun (list 1 2))