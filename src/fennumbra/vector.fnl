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

(fn Vector.copy [vector]
  "Returns a deep copy (only one level deep, so no traversing of tables) of a provided vector."
  (Vector.new (table.unpack vector)))

(fn Vector.dimensionality [vector]
  "Gets the dimensionality (number of components) of a given vector."
  (length vector))

(fn Vector._mt.__eq [a b]
  (if (not= (length a) (length b))
    false
    (do
      (var difference-found? false)
      (var i 1)
      (while (and (<= i (length a)) (not difference-found?))
        (if (not= (. a i) (. b i))
          (set difference-found? true)
          (set i (+ i 1))))
      (not difference-found?))))

Vector
