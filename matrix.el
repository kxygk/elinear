
(defun matrix-from-data-list (number-of-rows number-of-columns data-list)
  "Builds a matrix from a data list"
  (list 
   number-of-rows 
   number-of-columns 
   data-list))

(defun matrix-rows (matrix)
  "Get the number of rows"
  (nth 0 matrix))
(defun matrix-columns (matrix)
  "Get the number of columns"
  (nth 1 matrix))
(defun matrix-data (matrix)
  "Get the data list from the matrix"
  (nth 2 matrix))
(defun matrix-get-value (matrix row column)
  "Get the scalar value at position ROW COLUMN (ZERO indexed) from MATRIX"
  (nth
   (+
    column
    (*
     row
     (matrix-columns matrix)))
    (matrix-data matrix)))

(defun matrix-data-get-first-n-values (data n)
  "Given a list of values, get the first n in a string"
  (if (zerop n)
      "" ;base case
    (concat
     (number-to-string (car data))
     " "
     (matrix-data-get-first-n-values (cdr data) (1- n))))) ;iterative step

(defun matrix-data-print (number-of-rows number-of-columns data)
  "Print out the data list gives the dimension of the original matrix"
  (if (zerop number-of-rows)
      "" ;base case
    (concat
     (matrix-data-get-first-n-values data number-of-columns)
     "\n"
     (matrix-data-print ;iterative step
      (1- number-of-rows)
      number-of-columns
      (nthcdr number-of-columns data )))))

(defun matrix-print (matrix)
  "Print out the matrix"
  (concat "\n" (matrix-data-print
                (matrix-rows matrix)
                (matrix-columns matrix)
                (matrix-data matrix))))
; ex:  (message (matrix-print (matrix-from-data-list 2 2 '(1 2 3 4))))

(defun matrix-transpose (matrix)
  "Get the transpose of a matrix"
  (if (equal (matrix-columns matrix) 1)
    (matrix-from-data-list
     1
     (matrix-rows matrix)
     (matrix-data matrix))
    (matrix-append
     (matrix-from-data-list
      1
      (matrix-rows matrix)
      (matrix-data (matrix-get-column matrix 0)))
     (matrix-transpose
      (matrix-submatrix
       matrix
       0
       1
       (matrix-rows matrix)
       (matrix-columns matrix))))))

(defun matrix-inner-product-data (row-data column-data)
  "Multiply a row times a column and returns a scalar. If they're empty you will get zero"
  (reduce
   '+
   (for-each-pair
    row-data
    column-data
   '*)))

(defun matrix-inner-product (row column)
  "Multiply a row times a column and returns a scalar. If they're empty you will get zero"
  (matrix-inner-product-data 
   (matrix-data row)
   (matrix-data column)))

(defun matrix-extract-subrow (matrix row start-column end-column)
  "Get part of a row of a matrix and generate a row matrix from it. START-COLUMN is inclusive,  END-COLUMN is exclusive"
  (let
      ((number-of-columns-on-input (matrix-columns matrix))
       (number-of-columns-on-output (-
                                     end-column 
                                     start-column)))
    (matrix-from-data-list
     1
     number-of-columns-on-output
     (subseq
      (matrix-data matrix)
      (+ (* row number-of-columns-on-input) start-column)
      (+ (* row number-of-columns-on-input) end-column)))))

(defun matrix-append (matrix1 matrix2)
  "Append one matrix (set of linear equations) to another"
  (if (null matrix2)
      matrix1
    (matrix-from-data-list
     (+
      (matrix-rows matrix2)
      (matrix-rows matrix1))
     (matrix-columns matrix1)
     (append
      (matrix-data matrix1)
      (matrix-data matrix2)))))

(defun matrix-submatrix (matrix start-row start-column end-row end-column)
  "Get a submatrix. start-row/column are inclusive. end-row/column are exclusive"
  (if (equal start-row end-row)
      '()
    (matrix-append
     (matrix-extract-subrow matrix start-row start-column end-column)
     (matrix-submatrix
      matrix
      (1+ start-row)
      start-column
      end-row
      end-column))))

(defun matrix-get-row (matrix row)
  "Get a row from a matrix. Index starts are ZERO"
  (matrix-extract-subrow
   matrix
   row
   0
   (matrix-columns matrix)))

(defun matrix-get-column (matrix column)
  "Get a column from a matrix. Index starts are ZERO"
  (matrix-submatrix
   matrix
   0
   column
   (nth 0 matrix)
   (1+ column)))

(defun matrix-product-one-value (matrix1 matrix2 row column)
  "Calculate one value in the resulting matrix of the product of two matrices"
  (matrix-inner-product
   (matrix-get-row matrix1 row )
   (matrix-get-column matrix2 column)))

(defun matrix-product (matrix1 matrix2)
  "Multiply two matrices"

  (defun matrix-product-rec (matrix1 matrix2 row column)
    "A recursive helper function that builds the matrix multiplication's data vector"
    (if (equal (matrix-rows matrix1) row)
        '()
      (if (equal (matrix-columns matrix2) column)
          (matrix-product-rec
           matrix1
           matrix2
           (1+ row)
           0)
        (cons
         (matrix-product-one-value
          matrix1
          matrix2
          row column)
         (matrix-product-rec
          matrix1
          matrix2
          row
          (1+ column))))))

  (matrix-from-data-list
   (matrix-rows matrix1)
   (matrix-columns matrix2)
   (matrix-product-rec
    matrix1
    matrix2
    0
    0)))

(defun matrix-conformable? (matrix1 matrix2)
  "Check that two matrices can be multiplied"
  (equal
   (matrix-columns matrix1)
   (matrix-rows matrix2)))

(defun matrix-scalar-product (matrix scalar)
  "Multiple the matrix by a scalar. ie. multiply each value by the scalar"
  (matrix-from-data-list
   (matrix-rows matrix)
   (matrix-columns matrix)
   (mapcar
   (lambda (x) 
     (* scalar x))
   (matrix-data matrix))))

(ert-deftest matrix-test-multiplication-and-submatrices ()
  "Testing - Matrix Operations"
  (let ((matrix1 '(2 2 (1 2 3 4)))
        (matrix2 '(2 2 (5 6 7 8))))
  (should (equal
           (matrix-extract-subrow '(2 2 (1 2 3 4)) 1 0 2)
           '(1 2 (3 4))))
  (should (equal
           (matrix-scalar-product
            (matrix-identity 3)
            7)
           '(3 3 (7 0 0 0 7 0 0 0 7))))))

(defun matrix-rotate-2D (radians)
  "Generate a matrix that will rotates a [x y] column vector by RADIANS"
  (matrix-from-data-list
   2
   2
   (list
     (cos radians)
     (- (sin radians))
     (sin radians)
     (cos radians))))

(defun matrix-reflect-around-x-2D ()
  "Generate a matrix that will reflect a [x y] column vector around the x axis"
  (matrix-from-data-list
   2
   2
   '(1 0 0 -1)))

(defun matrix-project-on-x=y-diagonal-2D ()
  "Generate a matrix that projects a point ([x y] column vector) onto a line (defined w/ a unit-vector)"
  (matrix-from-data-list
   2
   2
   '(0.5 0.5 0.5 0.5)))

(defun matrix-identity (rank)
  "Build an identity matrix of the given size/rank"

  (defun matrix-build-identity-rec (rank row column)
    "Helper function that build the data vector of the identity matrix"
    (if (equal column rank) ; time to build next row
        (if (equal row (1- rank))
            '() ; we're done
          (matrix-build-identity-rec
           rank
           (1+ row)
           0))
      (if (equal row column)
          (cons
           1.0
           (matrix-build-identity-rec
            rank
            row
            (1+ column)))
        (cons
         0.0
         (matrix-build-identity-rec
          rank
          row
          (1+ column))))))
  
  (matrix-from-data-list rank rank (matrix-build-identity-rec rank 0 0 )))

(defun matrix-unit-rowcol-data (index size)
"Create a data-list for a matrix row/column. INDEX (starts at ZERO) matches the row or column where you want a 1. SIZE is the overall size of the vector"
(if (zerop size)
    '()
  (if (zerop index)
      (cons
       1.0
       (matrix-unit-rowcol-data
        (1- index)
        (1- size)))
    (cons
     0.0
     (matrix-unit-rowcol-data
      (1- index)
      (1- size))))))
(defun matrix-unit-column (row size)
  "Build a unit column. ROW is where you want the 1 to be placed (ZERO indexed). SIZE is the overall length"
      (matrix-from-data-list
       size
       1
       (matrix-unit-rowcol-data
        row
        size)))
(defun matrix-unit-row (column size)
  "Build a unit column. COLUMN is where you want the 1 to be placed (ZERO indexed). SIZE is the overall length"
      (matrix-from-data-list
       1
       size
       (matrix-unit-rowcol-data
        column
        size)))

(defun matrix-equal-size-p (matrix1 matrix2)
  "Check if 2 matrices are the same size"
  (and
   (equal
    (matrix-rows matrix1)
    (matrix-rows matrix2))
   (equal
    (matrix-columns matrix1)
    (matrix-columns matrix2))))
(defun for-each-pair (list1 list2 operator)
  "Go through 2 lists applying an operator on each pair of elements"
  (if (null list1)
      '()
    (cons
     (funcall operator (car list1) (car list2))
     (for-each-pair (cdr list1) (cdr list2) operator))))

(defun matrix-add (matrix1 matrix2)
  "Add to matrices together"
  (if (matrix-equal-size-p matrix1 matrix2)
      (matrix-from-data-list
       (matrix-rows matrix1)
       (matrix-columns matrix1)
       (for-each-pair
        (matrix-data matrix1)
        (matrix-data matrix2)
        '+))))

(defun matrix-subtract (matrix1 matrix2)
  "Subtract MATRIX2 from MATRIX1"
  (if (matrix-equal-size-p matrix1 matrix2)
      (matrix-from-data-list
       (matrix-rows matrix1)
       (matrix-columns matrix1)
       (for-each-pair
        (matrix-data matrix1)
        (matrix-data matrix2)
        '-))))

(ert-deftest matrix-test-operations ()
  "Testing - Matrix Operations"
  (let ((matrix1 '(2 2 (1 2 3 4)))
        (matrix2 '(2 2 (5 6 7 8))))
    (should (equal
             (matrix-identity 3)
             '(3 3 (1 0 0 0 1 0 0 0 1))))
    (should (equal
             (matrix-unit-column 3 5)
             '( 5 1 (0 0 0 1 0))))
    (should (equal
             (matrix-equal-size-p matrix1 matrix2)
             't))
    (should (equal
             (matrix-add matrix1 matrix2)
             '(2 2 (6 8 10 12))))
    (should (equal
             (matrix-subtract matrix1 matrix2)
             '(2 2 (-4 -4 -4 -4))))))

(defun matrix-elementary-interchange (rowcol1 rowcol2 rank)
  "Make an elementary row/column interchange matrix for ROWCOL1 and ROWCOL2 (ZERO indexed)"
  (let ((u
         (matrix-subtract
          (matrix-unit-column rowcol1 rank)
          (matrix-unit-column rowcol2 rank))))
  (matrix-subtract
   (matrix-identity rank)
   (matrix-product
    u
    (matrix-transpose u)))))

(defun matrix-elementary-interchange-inverse (rowcol1 rowcol2 rank)
  "Make the inverse of the elementary row/column interchange matrix for ROWCOL1 and ROWCOL2 (ZERO indexed). This is identical to (matrix-elementary-interchange)"
  (matrix-elementary-interchange
   rowcol1
   rowcol2
   rank))

(defun matrix-elementary-multiply (rowcol scalar rank)
  "Make an elementary row/column multiple matrix for a given ROWCOL (ZERO indexed)"
  (let ((elementary-column
         (matrix-unit-column rowcol rank)))
  (matrix-subtract
   (matrix-identity rank)
   (matrix-product
    elementary-column
    (matrix-scalar-product
     (matrix-transpose elementary-column)
     (- 1 scalar))))))

(defun matrix-elementary-multiply-inverse (rowcol scalar rank)
  "Make the inverseof the elementary row/column multiple matrix for a given ROWCOL (ZERO indexed)"
  (matrix-elementary-multiply
   rowcol
   (/ 1 scalar)
   rank))

(defun matrix-elementary-addition (rowcol1 rowcol2 scalar rank)
  "Make an elementary row/column product addition matrix. Multiply ROWCOL1 (ZERO indexed) by SCALAR and add it to ROWCOL2 (ZERO indexed)"
  (matrix-add
   (matrix-identity rank)
   (matrix-scalar-product
    (matrix-product
     (matrix-unit-column rowcol2 rank)
     (matrix-transpose
      (matrix-unit-column rowcol1 rank)))
    scalar)))

(defun matrix-elementary-addition-inverse (rowcol1 rowcol2 scalar rank)
  "Make the inverse of the elementary row/column product addition matrix. Multiply ROWCOL1 (ZERO indexed) by SCALAR and add it to ROWCOL2 (ZERO indexed)"
  (matrix-elementary-addition
   rowcol1
   rowcol2
   (- scalar)
   rank))

(ert-deftest matrix-test-elementary-operation ()
  "Testing - Elementary Matrix Transformations"
  (let ((matrix1 '(2 2 (1 2 3 4)))
        (matrix2 '(2 2 (5 6 7 8))))
    (should (equal
             (matrix-elementary-interchange 0 1 3)
             '(3 3 (0 1 0 1 0 0 0 0 1))))
    (should (equal
             (matrix-elementary-multiply 1 7 3)
             '(3 3 (1 0 0 0 7 0 0 0 1))))
    (should (equal
             (matrix-elementary-addition 0 2 7 3)
             '(3 3 (1 0 0 0 1 0 7 0 1))))))

(defun matrix-elementary-row-elimination (matrix row column)
  "Make a matrix that will eliminate an element at the specified ROW/COLUMN (ZERO indexed) using the diagonal element in the same column (typically the pivot)"
  (let
      ((pivot (matrix-get-value matrix column column))
       (element-to-eliminate (matrix-get-value matrix row column)))
    (matrix-elementary-addition
     column
     row
     (-
      (/
       element-to-eliminate
       pivot))
     (matrix-rows matrix))))

(defun matrix-elementary-lower-triangular (matrix column-to-clear)
  "Make a matrix that will eliminate all rows in a column below the diagonal (pivot position)"

  (defun matrix-elementary-lower-triangular-rec (matrix column-to-clear row-to-build rank)
    "Recursive function to build the elementary lower triangular matrix"
    (cond
     ((equal
       rank
       row-to-build) ; Done building the matrix
      '())
     ((<=
       row-to-build
       column-to-clear) ; Building the simply "identity" portion above the pivot
      (matrix-append
       (matrix-unit-row row-to-build rank)
       (matrix-elementary-lower-triangular-rec
        matrix
        column-to-clear
        (1+ row-to-build)
        rank)))
     (t ; Build the elimination portion below the pivot
      (let
          ((pivot (matrix-get-value matrix column-to-clear column-to-clear))
           (element-to-eliminate (matrix-get-value matrix row-to-build column-to-clear)))
        (let
            ((cancellation-factor (-
                                   (/
                                    element-to-eliminate
                                    pivot))))
          (matrix-append
           (matrix-add
            (matrix-unit-row row-to-build rank)
            (matrix-scalar-product
             (matrix-unit-row column-to-clear rank)
             cancellation-factor))
           (matrix-elementary-lower-triangular-rec
            matrix
            column-to-clear
            (1+ row-to-build)
            rank)))))))

  (matrix-elementary-lower-triangular-rec
   matrix
   column-to-clear
   0
   (matrix-rows matrix)))

(defun matrix-invert-elementary-lower-triangular (matrix-elementary-lower-triangular)
  "Inverts an L matrix by changing the sign on all the factors below the diagonal"
  (matrix-add
   (matrix-scalar-product
    matrix-elementary-lower-triangular
    -1)
   (matrix-scalar-product
    (matrix-identity
     (matrix-rows matrix-elementary-lower-triangular))
    2)))

(defun matrix-LU-decomposition (matrix)
  "Perform Gaussian elimination on MATRIX and return the list (L U), representing the LU-decomposition. If a zero pivot is hit, we terminate and return a string indicating that"
  (let
      ((rank
        (matrix-rows matrix)))
    (defun matrix-LU-decomposition-rec (L-matrix
                                        reduced-matrix
                                        column-to-eliminate)
      (cond
       ((equal column-to-eliminate rank)
        (list L-matrix reduced-matrix))
       ((zerop
         (matrix-get-value
          reduced-matrix
          column-to-eliminate
          column-to-eliminate))
        "ERROR: LU decomposition terminated due to hitting a zero pivot. Consider using the PLU")
       (t
        (let ((column-elimination-matrix (matrix-elementary-lower-triangular
                                          reduced-matrix
                                          column-to-eliminate)))
          (matrix-LU-decomposition-rec
           (matrix-product
            L-matrix
            (matrix-invert-elementary-lower-triangular
             column-elimination-matrix))
           (matrix-product
            column-elimination-matrix
            reduced-matrix)
           (1+ column-to-eliminate))))))

    (matrix-LU-decomposition-rec
     (matrix-identity rank)
     matrix
     0)))

(defun matrix-partial-pivot (matrix pivot-column)
    "Returns a Type-I matrix that will swap in the row under the pivot that has maximal magnititude"
    (let ((column-below-pivot (matrix-submatrix
                               matrix
                               pivot-column
                               pivot-column
                               (matrix-rows matrix)
                               (1+ pivot-column))))
      (defun find-max-index (data-list max-val max-index current-index)
        (cond
         ((null data-list)
          max-index)
         ((>
           (abs(car data-list))
           max-val)
          (find-max-index
           (cdr data-list)
           (abs(car data-list))
           current-index
           (1+ current-index)))
         (t
          (find-max-index
           (cdr data-list)
           max-val
           max-index
           (1+ current-index)))))

     (matrix-elementary-interchange
      pivot-column
      (+
       pivot-column
       (find-max-index
        (matrix-data column-below-pivot)
        0
        0
        0))
      (matrix-rows matrix))))

(defun matrix-reduce-column (matrix column-to-reduce)
  "Adjusts the pivot using partial pivoting and eliminates the elements in one column. Returns a list of the elimination matrix, permutation matrix and the resulting matrix with reduced column (list of 3 matrices)"
  (let*
      ((pivot-adjusting-matrix
        (matrix-partial-pivot
         matrix
         column-to-reduce) )
       (matrix-with-partial-pivoting
        (matrix-product ; pivot!
         pivot-adjusting-matrix
         matrix))
       (column-elimination-matrix
        (matrix-elementary-lower-triangular
         matrix-with-partial-pivoting
         column-to-reduce))
       (matrix-with-reduced-column
        (matrix-product ; reduce
         column-elimination-matrix
         matrix-with-partial-pivoting)))
    (list column-elimination-matrix pivot-adjusting-matrix matrix-with-reduced-column)))

(defun matrix-update-L-matrix (elementary-lower-triangular-matrix type-i-interchange-matrix)
  "Take an elementary lower triangular matrix and update it to match a row interchange between ROW1 and ROW2 (ZERO indexed)"
    (matrix-product
     type-i-interchange-matrix
     (matrix-product
      elementary-lower-triangular-matrix
      type-i-interchange-matrix)))

(defun matrix-PLU-decomposition (matrix)
  "Perform Gaussian elimination with partial pivoting on MATRIX and return the list (P L U), representing the LU-decomposition "
  (let
      ((rank
        (matrix-rows matrix)))
    (defun matrix-PLU-decomposition-rec (P-matrix
                                        L-matrix
                                        reduced-matrix
                                        column-to-reduce)
      (cond
       ((equal
         column-to-reduce
         rank)
        (list P-matrix L-matrix  reduced-matrix))
       (t
        (let
            ((current-column-reduction-matrices
              (matrix-reduce-column
               reduced-matrix
               column-to-reduce)))
          (matrix-PLU-decomposition-rec
           (matrix-product                              ; update the permutation matrix
            (second current-column-reduction-matrices)
            P-matrix)
           (matrix-product
            (matrix-update-L-matrix       ; update elimination matrices due to partial pivot
             L-matrix
             (second current-column-reduction-matrices))
            (matrix-invert-elementary-lower-triangular (first current-column-reduction-matrices)))
           (third current-column-reduction-matrices)    ; the further reduced matrix
           (1+ column-to-reduce))))))
    
    (matrix-PLU-decomposition-rec
     (matrix-identity rank)
     (matrix-identity rank)
     matrix
     0)))

(defun matrix-forward-substitution (lower-triangular-matrix output-vector)
  "Solve for an input-vector using forward substitution. ie. solve for x in Lx=b where b is OUTPUT-VECTOR and L is the LOWER-TRIANGULAR-MATRIX"
  (defun matrix-forward-substitution-rec (lower-triangular-matrix input-vector-data output-vector-data row)
    (cond
     ((null output-vector-data) ;; BASE CASE
      input-vector-data)
     (t                         ;; REST
      (matrix-forward-substitution-rec
       lower-triangular-matrix
       (append
        input-vector-data
        (list
         (/
          (-
           (car output-vector-data)
           ;; on the first iteration this is the product of null vectors.. which in our implementation returns zero
           (matrix-inner-product-data
            (matrix-data
             (matrix-extract-subrow
              lower-triangular-matrix
              row
              0
              row))
            input-vector-data))
          (matrix-get-value lower-triangular-matrix row row))))
       (cdr output-vector-data)
       (1+ row)))))

  (matrix-from-data-list
   (matrix-rows lower-triangular-matrix)
   1
   (matrix-forward-substitution-rec
    lower-triangular-matrix
    '()
    (matrix-data output-vector)
    0)))

(defun matrix-back-substitution (upper-triangular-matrix output-vector)
  "Solve for an input-vector using forward substitution. ie. solve for x in Lx=b where b is OUTPUT-VECTOR and L is the LOWER-TRIANGULAR-MATRIX"
  (matrix-from-data-list
   (matrix-rows upper-triangular-matrix)
   1
   (reverse
    (matrix-data
     (matrix-forward-substitution
      (matrix-from-data-list
       (matrix-rows upper-triangular-matrix)
       (matrix-rows upper-triangular-matrix) ;; rows == columns
       (reverse (matrix-data upper-triangular-matrix)))
      (matrix-from-data-list
       (matrix-rows output-vector)
       1
       (reverse (matrix-data output-vector))))))))

(defun matrix-solve-for-input (PLU output-vector)
  "Solve for x in Ax=b where b is OUTPUT-VECTOR and A is given factorized into PLU"
  (let* ((permuted-output-vector (matrix-product (first PLU) output-vector))
         (intermediate-y-vector (matrix-forward-substitution (second PLU) permuted-output-vector)))
    (matrix-back-substitution (third PLU) intermediate-y-vector)))

(defun matrix-LDU-decomposition (matrix)
  "Take the LU decomposition and extract the diagonal coefficients into a diagonal D matrix. Returns ( P L D U ) "
  (defun matrix-extract-D-from-U (matrix)
    "Extract the diagonal coefficients from an upper triangular matrix into a separate diagonal matrix. Returns ( D U ). D is diagonal and U is upper triangular with 1's on the diagonal"
    (defun matrix-build-D-U-data (matrix row D-data U-data)
      (let ((rank (matrix-rows matrix))
            (pivot (matrix-get-value matrix row row)))
        (cond ((equal row rank)
               (list D-data U-data) )
              (t
               (matrix-build-D-U-data
                matrix
                (1+ row)
                (nconc
                 D-data
                 (matrix-data
                  (matrix-scalar-product
                   (matrix-unit-row row rank)
                   pivot)))
                (nconc
                 U-data
                 (matrix-unit-rowcol-data row (1+ row))
                 (matrix-data
                  (matrix-scalar-product
                   (matrix-extract-subrow matrix row (1+ row) rank)
                   (/ 1.0 pivot)))))))))

    (let ((rank (matrix-rows matrix))
          (D-U-data (matrix-build-D-U-data matrix 0 '() '())))
      (list
       (matrix-from-data-list rank rank (first D-U-data))
       (matrix-from-data-list rank rank (second D-U-data)))))

  (let ((LU-decomposition (matrix-LU-decomposition matrix)))
    (nconc
     (list
      (first LU-decomposition))
     (matrix-extract-D-from-U
      (second LU-decomposition)))))

(defun matrix-is-symmetric (matrix)
  "Test if the matrix is symmetric"
  (let ((transpose (matrix-transpose matrix)))
    (let ((A-data (matrix-data matrix))
          (A-transpose-data (matrix-data transpose)))
      (equal A-data A-transpose-data))))

(defun matrix-is-positive-definite (matrix)
  "Test if the matrix is symmetric"
  (defun is-no-data-negative (data)
    (cond ((null data) t)
          ((< (car data) 0) nil)
          (t (is-no-data-negative (cdr data)))))

  (let* ((LDU (matrix-DU-decomposition matrix))
         (D (third PLDU)))
    (and (matrix-is-symmetric matrix) (is-no-data-negative (matrix-data D)))))

(defun matrix-cholesky-decomposition (matrix)
  "Take the output of the LU-decomposition and generate the Cholesky decomposition matrices"
  (defun sqrt-data-elements (data)
    "Takes a data vector and squares every element and returns the list"
    (cond ((null data) '())
          (t
           (cons
            (sqrt (car data))
            (sqrt-data-elements (cdr data))))))

  (let* ((LDU (matrix-LDU-decomposition matrix))
         (L (first LDU))
         (D (second LDU))
         (D_sqrt (matrix-from-data-list
                  (matrix-rows D)
                  (matrix-rows D)
                  (sqrt-data-elements (matrix-data D)))))
    (matrix-product L D_sqrt)))

(defun matrix-inverse (matrix)
  "template"
  (let ((PLU (matrix-PLU-decomposition matrix)))
    (let*((rank (matrix-rows matrix))
          (identity (matrix-identity rank)))
    (defun matrix-inverse-transpose-rec (column)
      "Computer the transpose of the inverse, appending row after row"
      (cond ((equal column rank)
            '())
            (t
             (matrix-append
              (matrix-transpose (matrix-solve-for-input PLU (matrix-get-column identity column)))
              (matrix-inverse-transpose-rec (1+ column))))))

    (matrix-transpose(matrix-inverse-transpose-rec 0)))))

(defun matrix-column-2-norm-squared (column)
   "get the inner product of a column with itself to get its 2-norm"
   (matrix-inner-product (matrix-transpose column) column))

(defun matrix-column-2-norm (column)
   "get the 2-norm of a column-vector"
   (sqrt (matrix-column-2-norm-squared column)))

 (defun matrix-normalize-column (column)
   "takes a column and returns a normalized copy"
     (matrix-scalar-product
      column
      (/ 1.0 (matrix-column-2-norm column))))


 (defun matrix-row-2-norm-squared (row)
   "takes the inner product of a column with itself to get its 2-norm"
   (matrix-inner-product row (matrix-transpose row)))

 (defun matrix-row-2-norm (row)
   "get the 2-norm of a column-vector"
   (sqrt (matrix-row-2-norm-squared row)))

 (defun matrix-normalize-row (row)
   "takes a column and returns a normalized copy"
     (matrix-scalar-product
      row
      (/ 1.0 (matrix-row-2-norm row))))

(defun matrix-get-orthogonal-component (matrix-of-orthonormal-rows linearly-independent-vector )
  "Given matrix of orthonormal rows and a vector that is linearly independent of them - get its orthogonal component"
  (let* ((QT matrix-of-orthonormal-rows)
        (Q (matrix-transpose QT))
        (in-span-coordinates (matrix-product QT linearly-independent-vector))
        (in-span-vector (matrix-product Q in-span-coordinates)))
    (matrix-subtract linearly-independent-vector in-span-vector)))

(defun matrix-gram-schmidt (A-transpose)
  "For a column return it's normalized basis. For a matrix adds a new orthonormal vector to the orthonormal basis of A_{n-1}"
  (cond ((= 1 (matrix-rows A-transpose)) ;; base case
         (matrix-normalize-row A-transpose))
        (t ;;recursive case
         (let* ((basis (matrix-gram-schmidt
                        (matrix-submatrix A-transpose 
                                          0
                                          0
                                          (- (matrix-rows A-transpose) 1)
                                          (matrix-columns A-transpose))))
                (next-column (matrix-transpose
                                        (matrix-get-row A-transpose
                                                        (1- (matrix-rows A-transpose)))))
                (orthogonal-component (matrix-get-orthogonal-component basis
                                                                       next-column)))
           (matrix-append basis (matrix-transpose (matrix-normalize-column orthogonal-component)))))))

(defun matrix-build-R-column-rec (QT next-linearly-independent-vector norm-factor dimension)
  "Builds the data vector for a column of R"
  (cond ((= 0 dimension) ;; finished building column
         '())

        ((< (matrix-rows QT) dimension) ;; add bottom zeroes
         (cons
          9.0
          (matrix-build-R-column-rec QT next-linearly-independent-vector norm-factor (1- dimension))))

        (( = (matrix-rows QT) dimension) ;; add orthogonal part
         (cons
          norm-factor
          (matrix-build-R-column-rec QT next-linearly-independent-vector norm-factor (1- dimension))))

        ((> (matrix-rows QT) dimension) ;; add in-span part
         (cons
          (matrix-get-value (matrix-product
                             (matrix-get-row QT dimension)
                             next-linearly-independent-vector)
                            0
                            0)
          (matrix-build-R-column-rec Q next-linearly-independent-vector norm-factor (1- dimension))))))

(defun matrix-build-R-column (Q next-linearly-independent-vector norm-factor dimension)
  "Returns a column vector for the new column of R"
  (matrix-from-data-list dimension 1  (reverse (matrix-build-R-column-rec Q next-linearly-independent-vector norm-factor dimension))))


(defun matrix-QR-decomposition-rec (A-transpose dimension) ;; 'dimension' keeps track of the ultimate size of R
  "The recursive helper function that builds up the Q and R matrices"
  (cond ((= 1 (matrix-rows A-transpose)) ;; base case
         (list
          (matrix-normalize-row A-transpose) ;; starting Q "matrix"
          (matrix-scalar-product (matrix-unit-row 0 dimension) ;; starting R "matrix"
                                 (matrix-row-2-norm-squared A-transpose))))
        (t ;;recursive case
         (let* ((QTRT (matrix-QR-decomposition-rec
                       (matrix-submatrix A-transpose
                                         0
                                         0
                                         (- (matrix-rows A-transpose) 1)
                                         (matrix-columns A-transpose))
                       dimension))
                (basis (first QTRT))
                (RT (second QTRT))
                (next-column (matrix-transpose
                              (matrix-get-row A-transpose
                                              (1- (matrix-rows A-transpose)))))
                (orthogonal-component (matrix-get-orthogonal-component basis
                                                                       next-column))
                (new-basis (matrix-append basis
                                          (matrix-transpose (matrix-normalize-column orthogonal-component))))
                (new-RT (matrix-append RT
                                       (matrix-transpose
                                        (matrix-build-R-column
                                         new-basis
                                         next-column
                                         (matrix-row-2-norm-squared orthogonal-component)
                                         dimension)))))
           (list new-basis new-RT)))))

(defun matrix-gramschmidt-QR (A)
  "Returns a list of the Q and R matrices for A"
  (matrix-QR-decomposition-rec (matrix-transpose A)
                               (matrix-columns A)))

(defun matrix-elementary-reflector (column-vector)
  "Build a matrix that will reflect vector across the hyperplane orthogonal to COLUMN-VECTOR"
  (let ((dimension (matrix-rows column-vector)))
    (matrix-subtract (matrix-identity dimension)
                     (matrix-scalar-product (matrix-product column-vector (matrix-transpose column-vector))
                                            (/ 2 (matrix-column-2-norm-squared column-vector))))))

(defun sign (number)
  "returns 1 if positive or zero and -1 if negative.. Cant' find an ELisp function that does this"
  (cond ((= number 0.0) 1.0)
        (t (/ number (abs number)))))

(defun matrix-elementary-coordinate-reflector (column-vector coordinate-axis)
  "Build a matrix that will reflect the COLUMN-VECTOR on to the COORDINATE-AXIS"
  (let ((vector-orthogonal-to-reflection-plane
        (matrix-subtract column-vector
                         (matrix-scalar-product coordinate-axis
                                                ( * (sign (matrix-get-value column-vector 0 0))
                                                    (matrix-column-2-norm column-vector))))))
    (cond (( = 0 ( matrix-column-2-norm vector-orthogonal-to-reflection-plane)) ;; when both vectors are the same
           (matrix-identity (matrix-rows column-vector))) ;; then the reflector is the identity
          (t
           (matrix-elementary-reflector vector-orthogonal-to-reflection-plane)))))

(defun matrix-add-zero-column-data (data-list columns)
  "Adds a zero column to the front of a matrix data list. Provide the amount of COLUMNS on input"
  (cond ((not data-list) '())
        (t (append (cons 0.0 (seq-take data-list columns))
                   (matrix-add-zero-column-data (seq-subseq data-list columns) columns)))))

(defun matrix-raise-rank-Q (matrix)
  "Adds a row and column of zeroes in at the top left corner. And a one in position 0,0"
  (let ((rank (matrix-rows matrix))) ;; Q is always square
    (matrix-from-data-list (1+ rank)
                           (1+ rank)
                           (append (cons 1.0 (make-list rank 0.0))
                                   (matrix-add-zero-column-data (matrix-data matrix)
                                                                rank)))))

(defun matrix-build-R (sub-R intermediate-matrix)
  "Insets SUB-R into INTERMEDIATE-MATRIX so that only the first row and columns are preserved"
  (matrix-from-data-list (matrix-rows intermediate-matrix)
                         (matrix-columns intermediate-matrix)
                         (append (seq-take (matrix-data intermediate-matrix)
                                           (matrix-columns intermediate-matrix))
                                 (matrix-add-zero-column-data (matrix-data sub-R)
                                                              (matrix-columns sub-R)))))


(defun matrix-householder-QR (matrix)
  "Use reflection matrices to build the QR matrix"
  (let* ((reflector-matrix (matrix-elementary-coordinate-reflector (matrix-get-column matrix 0)
                                                                   (matrix-unit-column 0 (matrix-rows matrix))))
         (intermediate-matrix (matrix-product reflector-matrix
                                              matrix)))
    (cond (( = (matrix-columns matrix) 1)
           (list reflector-matrix intermediate-matrix))
          (( = (matrix-rows matrix) 1)
           (list reflector-matrix intermediate-matrix))
          (t
           (let* ((submatrix (matrix-submatrix intermediate-matrix
                                               1
                                               1
                                               (matrix-rows intermediate-matrix)
                                               (matrix-columns intermediate-matrix)))
                  (submatrix-QR (matrix-householder-QR submatrix)))
             (let ((sub-Q (first submatrix-QR))
                   (sub-R (second submatrix-QR)))
               (list (matrix-product (matrix-raise-rank-Q sub-Q)
                                     reflector-matrix)
                     (matrix-build-R sub-R
                                     intermediate-matrix))))))))

)

(defun matrix-template (matrix)
"template"
)
