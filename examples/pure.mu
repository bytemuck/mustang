(let v0 0)
(let v1 1)
(let v2 2)

(defunc f0 (a)
    (+ a w)
)

(defunc f1 (a) 
    (let b 3)
    (+ a b)
)

(defunc f2 (a)
    (set v1 2)
    (f0 (f1 a) v2)
)

(defunc f3 (a)
    (f2 2) 
)

(defunc f4 (a)
    (f0 v0) 
)