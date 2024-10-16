;; fennumbra/algebraic.fnl

(fn calc-discriminant [a b c]
  "Calculates and returns the discriminant of a quadratic polynomial."
  (- (* b b) (* 4 a c)))

(fn num-of-real-roots [a b c]
  "Determines and returns the number of unique real roots for a quadratic polynomial."
  (case (calc-discriminant a b c)
    (where result (> result 0)) 2
    (where result (= result 0)) 1
    (where result (< result 0)) 0))

{:calc-discriminant calc-discriminant
 :num-of-real-roots num-of-real-roots}
