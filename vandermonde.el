
(load-file "matrix.el")

(defun matrix-vandermonde (list-of-xs number-of-points)
  "Build a Vandermonde matrix of the appropriate rank from a LIST-OF-Xs"
  (defun matrix-build-polynomial-list (x degree)
    "Build a list of (X,X^2,X^3,..,X^DEGREE)"
    (cond
     ((zerop degree)
      '(1))
     (t
      (cons
       (expt x degree)
       (matrix-build-polynomial-list x (1- degree))
       ))))
  (defun matrix-vandermonde-data (list-of-xs degree)
    "Builds the data vector of the Vandermonde matrix"
    (cond
     ((null list-of-xs)
      '())
     (t
      (append
       (reverse
        (matrix-build-polynomial-list
         (car list-of-xs)
         degree))
       (matrix-vandermonde-data
        (cdr list-of-xs)
        degree)))))
  (matrix-from-data-list
   number-of-points
   number-of-points
   (matrix-vandermonde-data
    list-of-xs
    (1- number-of-points))))

(defun matrix-fit-polynomial (x-coordinates y-coordinates)
"Given a list of x and y coordinates, solve for a polynomial that fits them using a Vandermonde matrixs. The result is a vector of factors 'a' that should be used in the standard order: a_1+a_2*x+a_3*x^2+a_4*x^3+... etc"
(let* ((number-of-points (length x-coordinates))
       (vandermonde-matrix
        (matrix-vandermonde x-coordinates number-of-points))
       (PLU (matrix-PLU-decomposition vandermonde-matrix)))

  (matrix-solve-for-input
   PLU
   (matrix-from-data-list
    number-of-points
    1
    y-coordinates))))
