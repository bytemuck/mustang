(defun foldl (f a l)
    (if (null l)
        a
        (foldl f (f a (head l)) (tail l))
    )
)

(defun reduce (f l)
    (if (null l)
        nil
        (foldl f (head l) (tail l))
    )
)