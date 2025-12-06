(defun add-taxes (price)
    (+ price
        (/ (* price 15) 100)))

(defun calculate (prices taxable?)
    (let prices-with-tax
        (if taxable?
            (map add-taxes prices)
            prices))
    (reduce + prices-with-tax))

(calculate (list 100 5 15) t)