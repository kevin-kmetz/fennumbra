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
  )

Matrix
