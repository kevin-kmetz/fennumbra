;; fennumbra/numtheoretic/digitsum.fnl
;;
;; Takes any positive integer, and sums the digits.

(fn num-qualifies? [number]
  (and (= (type number) "number")
       (= (math.type number) "integer")
       (> number 0)
       (>= (# (tostring number)) 1)))

(fn digit-sum [number]
  (if (num-qualifies? number)
    (if (< number 10)
      number
      (let [numstring (tostring number)]
        (var sum 0)
        (for [i 1 (# numstring)]
          (set sum (+ sum (tonumber (string.sub numstring i i)))))
        sum))
    nil))

(fn digital-root [number]
  (if (num-qualifies? number)
    (do
      (var sum number)
      (while (and (num-qualifies? sum) (>= sum 10))
        (set sum (digit-sum sum)))
      sum)
    nil))

(fn do-for-each-index [func start-index end-index ?step]
  (for [i start-index end-index (or ?step 1)]
    (func i)))

(fn display-digit-sum [start-index end-index ?step]
  (do-for-each-index
    (lambda [index]
      (io.write index
                ": "
                (digit-sum index)
                "\n"))
      start-index
      end-index
      ?step))

(fn display-digital-root [start-index end-index ?step]
  (do-for-each-index
    (lambda [index]
      (io.write index
                ": "
                (digital-root index)
                "\n"))
      start-index
      end-index
      ?step))

{:digit-sum            digit-sum
 :digital-root         digital-root
 :do-for-each-index    do-for-each-index
 :display-digit-sum    display-digit-sum
 :display-digital-root display-digital-root

 ;; Convenience aliases/synonyms for the above functions:
 :ds                   digit-sum
 :dr                   digital-root
 :dfei                 do-for-each-index
 :dds                  display-digit-sum
 :ddr                  display-digital-root}

