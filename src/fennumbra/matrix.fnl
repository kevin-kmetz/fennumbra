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
    (setmetatable new-matrix Matrix._mt)
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

Matrix
