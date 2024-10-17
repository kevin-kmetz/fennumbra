;; fennumbra/complex.fnl

(local Complex {:_mt {}})
(tset Complex._mt :__index Complex)

(fn Complex.new [a b]
  "A constructor for a class representing complex numbers."
  (assert (and (= (type a) :number) (= (type b) :number))
    "Invalid types passed in as arguments to Complex constructor.")
  (let [new-complex {:a a :b b}]
    (setmetatable new-complex Complex._mt)
    new-complex))

(setmetatable Complex
  {:__call
    (fn [self a b]
      (Complex.new a b))})

(fn Complex._mt.__tostring [self]
  (.. self.a " + " self.b "i"))

(fn Complex._mt.__len [self]
  (math.sqrt (+ (* self.a self.a) (* self.b self.b))))

(fn Complex._mt.__eq [self other]
  (and (= self.a other.a) (= self.b other.b)))

(fn Complex.get-conjugate [self]
  "Returns the conjugate of the provided complex number."
  (Complex.new self.a (- self.b)))

Complex
