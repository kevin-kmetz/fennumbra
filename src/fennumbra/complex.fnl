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

(fn Complex.complex? [object]
  "Returns true if the object is a complex number."
  (let [metatable (getmetatable object)]
    (if (and (= (type metatable) :table) (= metatable.__index Complex))
      true
      false)))

(fn Complex.to-complex [number]
  "Converts numbers to complex numbers; if passed a complex number, it returns the argument."
  (case number
    (where unknown (= (type unknown) :number)) (Complex.new number 0)
    (where unknown (Complex.complex? unknown)) number
    _ nil))

(fn Complex._mt.__tostring [self]
  (.. self.a " + " self.b "i"))

(fn Complex._mt.__len [self]
  (math.sqrt (+ (* self.a self.a)
                (* self.b self.b))))

(fn Complex._mt.__eq [a b]
    (and (= a.a b.a) (= a.b b.b)))

(fn Complex._mt.__add [a b]
  ;; All complex operation metamethods make no assumption about which of the
  ;; arguments is the actual complex number, to address how Lua will try to
  ;; invoke the metamethod if a non-complex is in the first position if it
  ;; lacks a suitable metamethod itself.
  ;;
  ;; Equality is the exception, since equality in Lua behaves differently in a
  ;; way that opposes what I'm trying to do.
  (let [a (Complex.to-complex a)
        b (Complex.to-complex b)]
    (Complex.new (+ a.a b.a) (+ a.b b.b))))

(fn Complex._mt.__sub [a b]
  (let [a (Complex.to-complex a)
        b (Complex.to-complex b)]
    (Complex.new (- a.a b.a) (- a.b b.b))))

(fn Complex._mt.__unm [self]
  (Complex.new (- self.a) (- self.b)))

(fn Complex._mt.__mul [a b]
  (let [a (Complex.to-complex a)
        b (Complex.to-complex b)]
    (Complex.new (- (* a.a b.a) (* a.b b.b))
                 (+ (* a.a b.b) (* a.b b.a)))))

(fn Complex._mt.__div [a b]
  (let [a (Complex.to-complex a)
        b (Complex.to-complex b)
        mag-squared (+ (* b.a b.a) (* b.b b.b))
        real-numerator (+ (* a.a b.a) (* a.b b.b))
        imaginary-numerator (- (* a.b b.a) (* a.a b.b))]
    (Complex.new (/ real-numerator mag-squared)
                 (/ imaginary-numerator mag-squared))))

(fn Complex.get-reciprocal [self]
  "Returns the reciprocal of the provided complex number."
  (/ 1 self))

(fn Complex.get-conjugate [self]
  "Returns the conjugate of the provided complex number."
  (Complex.new self.a (- self.b)))

Complex
