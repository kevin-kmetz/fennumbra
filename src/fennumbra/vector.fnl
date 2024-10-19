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

;; This variant of the dimensionality method is needed in order
;; to allow seamless usage of scalars in vector arithmetic operations.
(fn Vector.dimensionality-protected [vector]
  "Gets the dimensionality of a vector with a protected call, returning nil if an error was generated."
  (let [[no-error? dimensionality]
          (table.pack
            (pcall
              (fn [v] (length v))
              vector))]
    (if no-error?
      dimensionality
      nil)))

(fn Vector.scalar-to-vector [scalar dimensionality]
  "Converts a scalar value into a vector of specified dimensions where all components are the scalar value."
  (fcollect [i 1 dimensionality]
    scalar))

;; The method enables scalars to be used by vector arithmetic operators without
;; dedicated and separate methods to do so.
(fn Vector.convert-if-scalar [a b]
  "Takes two values which can be either vectors or scalars - if one is scalar, converts it to same dimensions as the vector."
  (case [(Vector.dimensionality-protected a)
         (Vector.dimensionality-protected b)]
    [nil dim] [(Vector.scalar-to-vector a dim) b]
    [dim nil] [a (Vector.scalar-to-vector b dim)]
    [nil nil] (error "Impossible to convert two scalars to any meaningful vectors.")
    _         [a b]))

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
  (let [[a b] (Vector.convert-if-scalar a b)]
    (if (Vector.same-dimensionality? a b)
      (Vector.new-from-array
        (icollect [i v (ipairs a)]
          (+ v (. b i))))
      nil)))

(fn Vector._mt.__sub [a b]
  "For two vectors or arrays of the same dimensionality, subtracts the second from the first."
  (let [[a b] (Vector.convert-if-scalar a b)]
    (if (Vector.same-dimensionality? a b)
      (Vector.new-from-array
        (icollect [i v (ipairs a)]
          (- v (. b i))))
      nil)))

(fn Vector._mt.__unm [vector]
  "Negates all components of a vector or array."
  (Vector.new-from-array
    (icollect [i v (ipairs vector)]
      (- v))))

(fn Vector._mt.__mul [a b]
  "Multiplies two vectors or arrays of the same dimensionality."
  (let [[a b] (Vector.convert-if-scalar a b)]
    (if (Vector.same-dimensionality? a b)
      (Vector.new-from-array
        (icollect [i v (ipairs a)]
          (* v (. b i))))
      nil)))

(fn Vector._mt.__div [a b]
  "Divides the second vector or array into the first, assuming they have the same dimensionality."
  (let [[a b] (Vector.convert-if-scalar a b)]
    (if (Vector.same-dimensionality? a b)
      (Vector.new-from-array
        (icollect [i v (ipairs a)]
          (/ v (. b i))))
      nil)))

;; Convenience aliases for use with non-vector arrays.
(set Vector.equals? Vector._mt.__eq)
(set Vector.add Vector._mt.__add)
(set Vector.subtract Vector._mt.__sub)
(set Vector.negate Vector._mt.__unm)
(set Vector.multiply Vector._mt.__mul)
(set Vector.divide Vector._mt.__div)

(fn Vector.sum-components [vector]
  "Sums the components of the provided vector or array."
  (faccumulate [sum 0 component-index 1 (length vector)]
    (+ sum (. vector component-index))))

(fn Vector.dot [a b]
  "Returns the dot product of the two provided vectors or arrays."
  (Vector.sum-components (Vector.multiply a b)))

(fn Vector.magnitude [vector]
  "Returns the length/magnitude/norm of a given vector or array."
  (math.sqrt (Vector.dot vector vector)))

;; Convenience aliases for the vector magnitude function.
;;
;; There is merit in not overriding the default __len metamethod for
;; vectors, since having the __len method return dimensionality makes
;; many of these methods compatible with arrays without having to have
;; them cast as vectors.
(set Vector.length Vector.magnitude)
(set Vector.norm Vector.magnitude)

(fn Vector.normalize [vector]
  "Returns a normalized version (same direction/orientation/angle, but as unit vector) of a given vector or array."
  (Vector.divide vector (Vector.magnitude vector)))

Vector
