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

;; This is to act as a constructor that takes an array.
;; It is kept separate from the primary constructor so that
;; needless ifs and conditional can be avoided.
(set Vector.new-from-array Vector.copy)

(fn Vector.dimensionality [vector]
  "Gets the dimensionality (number of components) of a given vector."
  (length vector))

(fn Vector.same-dimensionality? [a b]
  "Returns true if the two provided vectors have the same number of components."
  (= (length a) (length b)))

(fn Vector._mt.__eq [a b]
  "Returns true if two vectors or arrays have the same dimensionality and have equal components."
  (if (not (Vector.same-dimensionality? a b))
    false
    (do
      (var difference-found? false)
      (var i 1)
      (while (and (<= i (length a)) (not difference-found?))
        (if (not= (. a i) (. b i))
          (set difference-found? true)
          (set i (+ i 1))))
      (not difference-found?))))

(fn Vector._mt.__add [a b]
  "Sums two vectors or arrays of the same dimensionality."
  (if (Vector.same-dimensionality? a b)
    (Vector.new-from-array
      (icollect [i v (ipairs a)]
        (+ v (. b i))))
    nil))

(fn Vector._mt.__sub [a b]
  "For two vectors or arrays of the same dimensionality, subtracts the second from the first."
  (if (Vector.same-dimensionality? a b)
    (Vector.new-from-array
      (icollect [i v (ipairs a)]
        (- v (. b i))))
    nil))

(fn Vector._mt.__unm [vector]
  "Negates all components of a vector or array."
  (Vector.new-from-array
    (icollect [i v (ipairs vector)]
      (- v))))

(fn Vector._mt.__mul [a b]
  "Multiplies two vectors or arrays of the same dimensionality."
  (if (Vector.same-dimensionality? a b)
    (Vector.new-from-array
      (icollect [i v (ipairs a)]
        (* v (. b i))))
    nil))

(fn Vector._mt.__div [a b]
  "Divides the second vector or array into the first, assuming they have the same dimensionality."
  (if (Vector.same-dimensionality? a b)
    (Vector.new-from-array
      (icollect [i v (ipairs a)]
        (/ v (. b i))))
    nil))

;; Convenience aliases for use with non-vector arrays.
(set Vector.equals? Vector._mt.__eq)
(set Vector.add Vector._mt.__add)
(set Vector.subtract Vector._mt.__sub)
(set Vector.negate Vector._mt.__unm)
(set Vector.multiply Vector._mt.__mul)
(set Vector.divide Vector._mt.__div)

Vector
