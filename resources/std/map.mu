(defun map1 (f l)
    (if (null l) 
        nil
        (:
            (f (head l)) 
            (map1 f (tail l))
        )
    )
)

(defun map (f l) (map1 f l))
