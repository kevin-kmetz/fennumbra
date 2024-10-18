;; fennumbra/vector.fnl

(local Vector {:_mt {}})
(tset Vector._mt :__index Vector)

(fn Vector.new [...]
  "A constructor for a vector class - elements are passed in as varargs."
  (let [new-vector [...]]
    (setmetatable new-vector Vector._mt)
    new-vector))

(setmetatable Vector
  {:__call
    (fn [self ...]
      (Vector.new ...))})
