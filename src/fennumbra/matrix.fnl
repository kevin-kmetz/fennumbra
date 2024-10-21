;; fennumbra/matrix.fnl

(local Vector (require :fennumbra.vector))

(local Matrix {:_mt {}})
(set Matrix._mt.__index Matrix)

(fn Matrix.new [row-count column-count initializer-func]
  "A constructor for a matrix that initializes elements by a function taking row and column indices."
  (let [new-matrix {:column-count column-count
                    :row-count row-count
                    :rows {}}]
    (for [row 1 row-count]
      (set (. new-matrix :rows row)
             (Vector.new-from-array
               (fcollect [column 1 column-count]
                 (initializer-func row column)))))
    ;; The following odd sequence of three lines enables elements of matrices
    ;; to be accessed like so in Lua m[row#][column#] or in Fennel (. m row# column#),
    ;; which is similar to how multi-dimensional arrays are accessed in other languages.
    ;; This sequence avoids me having to cobble together a custom metatable.
    ;; I have considered that I could just move the :rows subtable to the numerical indices
    ;; of the matrix instance itself, but I'll keep it as-is for now. I like having the
    ;; rows/vectors clearly specified as such, but it's probably not necessary.
    (set (. new-matrix :_mt) {:__index new-matrix.rows})
    (setmetatable new-matrix new-matrix._mt)
    (setmetatable new-matrix.rows Matrix._mt)
    new-matrix))

(fn Matrix.new-from-vectors [...]
  "A constructor for a matrix that creates a matrix from vector and array varargs of the same dimensionality."
  (let [new-matrix {:column-count (length (. [...] 1))
                    :row-count (length [...])
                    :rows [...]}]
    (if (faccumulate [same-size? true i 2 (length [...]) &until (not same-size?)]
          (and same-size? (= (length (. [...] i))
                             (length (. [...] (- i 1))))))
      new-matrix
      nil)))

(setmetatable Matrix {:__call
  (fn [_ ...]
    "Invokes one of the two matrix constructors based on what is passed in as arguments."
    (if (and (= (type (?. [...] 1)) :number)
             (= (type (?. [...] 2)) :number)
             (= (type (?. [...] 3)) :function))
      (Matrix.new ...)
      (Matrix.new-from-vectors ...)))})

;; The following method is used in overloaded matrix arithmetic
;; operators to 'intelligently' dispatch the arguments to the proper
;; arithmetic subfunction based on whether one of the two arguments is
;; is scalar, vector/array, or matrix (one operand always has to be a matrix).
(fn Matrix.algebraic-type [object]
  "Determines (makes an 'informed' guess) if an object is a matrix, vector, or scalar - returns nil otherwise."
  (case (type object)
    :number :scalar
    (where :table
           (= (type (?. object :rows))
              :table))
            :matrix
    :table  :vector
    _       nil))

Matrix
