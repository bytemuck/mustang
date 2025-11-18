(defun my-map (func lst)
    (if (null lst) 
        nil
        (:
            (func (head lst)) 
            (my-map func (tail lst))
        )
    )
)

(defun double (x)
    (+ x x)
)

(let e (my-map double (list 1 2 3)))
(printfn e)