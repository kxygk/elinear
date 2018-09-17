
(load-file "matrix.el")

(defun mean (row-vector)
  "Get the mean of the elements of data"
  (let ((sum-length 
         (reduce 
          (lambda (sum-length new-value)
            (let ((sum (first sum-length))
                  (length (second sum-length)))
              (list
               (+ sum new-value)
               (+ length 1))))
          data
          :initial-value (list 0 0))))
    (/ (first sum-length)
       (second sum-length))))

(defun mean (data)
  "Get the mean of the elements of data"
  (let ((sum-length 
         (reduce 
          (lambda (sum-length new-value)
            (let ((sum (first sum-length))
                  (length (second sum-length)))
              (list
               (+ sum new-value)
               (+ length 1))))
          data
          :initial-value (list 0 0))))
    (/ (first sum-length)
       (second sum-length))))
