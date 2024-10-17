;; fennumbra/complex.fnl

(local Complex {:mt-complex {}})
(tset Complex.mt-complex "__index" Complex)

(fn Complex.new [a b]
  "A constructor for a class representing complex numbers."
  (assert (and (= (type a) "number") (= (type b) "number"))
    "Invalid types passed in as arguments to Complex constructor.")
  (let [new-complex {:a a :b b}]
    (setmetatable new-complex Complex.mt-complex)
    new-complex))

Complex
