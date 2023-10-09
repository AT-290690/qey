; (math lib)
(defun math (do
  ; modules
  ; E
  (deftype E (Lambda (Or (Number))))
  (defun E 2.718281828459045)
  ; PI
  (deftype PI (Lambda (Or (Number))))
  (defun PI 3.141592653589793)
  ; circumference
  (deftype circumference (Lambda (Or (Number)) (Or (Number))))
  (defun circumference radius (* 3.141592653589793 (* radius 2)))
  ; positive?
  (deftype positive? (Lambda (Or (Number)) (Or (Boolean))))
  (defun positive? num (> num 0))
  ; negative?
  (deftype negative? (Lambda (Or (Number)) (Or (Boolean))))
  (defun negative? num (< num 0))
  ; zero?
  (deftype zero? (Lambda (Or (Number)) (Or (Boolean))))
  (defun zero? num (= num 0))
  ; divisible?
  (deftype divisible? (Lambda (Or (Number)) (Or (Number))  (Or (Boolean))))
  (defun divisible? a b (= (mod a b) 0))
  ; nth-digit
  (deftype nth-digit (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
  (defun nth-digit digit n (| (mod (/ digit (power 10 (- n 1))) 10) 0.5))
  ; max
  (deftype max (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
  (defun max a b (if (> a b) a b))
  ; min
  (deftype min (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
  (defun min a b (if (< a b) a b))
  ; maximum
  (deftype maximum (Lambda (Or (Array (Number))) (Or (Number))))
  (defun maximum array (reduce array (lambda a b . . (max a b)) -9007199254740991))
  ; minimum
  (deftype minimum (Lambda (Or (Array (Number))) (Or (Number))))
  (defun minimum array (reduce array (lambda a b . . (min a b)) 9007199254740991))
  ; normalize 
  (deftype normalize (Lambda (Or (Number)) (Or (Number)) (Or (Number)) (Or (Number))))
  (defun normalize value min max (* (- value min) (/ (- max min))))
  ; linear-interpolation
  (deftype linear-interpolation (Lambda (Or (Number)) (Or (Number)) (Or (Number)) (Or (Number))))
  (defun linear-interpolation a b n (+ (* (- 1 n) a) (* n b)))
  ; gauss-sum
  (deftype gauss-sum (Lambda (Or (Number)) (Or (Number))))
  (defun gauss-sum n (* n (+ n 1) 0.5))
  ; gauss-sum-sequance
  (deftype gauss-sum-sequance (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
  (defun gauss-sum-sequance a b (* (+ a b) (+ (- b a) 1) 0.5))
  ; clamp
  (deftype clamp (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
  (defun clamp x limit (if (> x limit) limit x))
  ; odd?
  (deftype odd? (Lambda (Or (Number)) (Or (Boolean))))
  (defun odd? x (= (mod x 2) 1))
  ; even?
  (deftype even? (Lambda (Or (Number)) (Or (Boolean))))
  (defun even? x (= (mod x 2) 0))
  ; sign 
  (deftype sign (Lambda (Or (Number)) (Or (Number))))
  (defun sign n (if (< n 0) -1 1))
  ; radians
  (deftype radians (Lambda (Or (Number)) (Or (Number))))
  (defun radians deg (* deg 3.141592653589793 (/ 180)))
  ; binomial-coefficient
  (deftype binomial-coefficient (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
  (defun binomial-coefficient n k 
    (or 
      (cond 
        (or (< k 0) (> k n)) 0
        (or (= k 0) (= k n)) 1
        (or (= k 1) (= k (- n 1))) n) (do 
          (when (< (- n k) k) (setf k (- n k)))
          (loop defun iterate i res (if (<= i k) (iterate (+ i 1) (* res (- n i -1) (/ i))) res))
          (round (iterate 2 n)))))
  ; sin
  (deftype sin (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
  (defun sin rad terms (do
    (defvar sine 0)
    (loop defun inc i 
      (do 
        (setf sine 
          (+ sine 
            (* 
              (/ (factorial (+ (* 2 i) 1))) 
              (power -1 i) 
              (power rad (+ (* 2 i) 1))))) 
        (if (< i terms) (inc (+ i 1)) sine)))
      (inc 0)))
   ; cos 
  (deftype cos (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
  (defun cos rad terms (do
    (defvar cosine 0)
    (loop defun inc i 
      (do 
        (setf cosine 
          (+ cosine 
            (* 
              (/ (factorial (* 2 i))) 
              (power -1 i) 
              (power rad  (* 2 i))))) 
        (if (< i terms) (inc (+ i 1)) cosine)))
    (inc 0)))
  ; square
  (deftype square (Lambda (Or (Number)) (Or (Number))))
  (defun square x (* x x))
  ; average
  (deftype average (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
  (defun average x y (* (+ x y) 0.5))
  ; sqrt
  (deftype sqrt (Lambda (Or (Number)) (Or (Number))))
  (defun sqrt x (do 
    (defconstant is-good-enough (lambda g x (< (abs (- (square g) x)) 0.01))
         improve-guess (lambda g x (average g (* x (/ g)))))
    (loop defun sqrt-iter g x 
        (if (is-good-enough g x) 
            g
            (sqrt-iter (improve-guess g x) x)))
  (sqrt-iter 1.0 x)))
  ; can-sum?
  (deftype can-sum? (Lambda (Or (Number)) (Or (Array (Number))) (Or (Boolean))))
  (defun can-sum? t values 
    (if (< t 0) 0 
      (if (= t 0) 1 
        (some? values (lambda x . . (can-sum? (- t x) values))))))
  ; how-can-sum
  (deftype how-can-sum (Lambda (Or (Number)) (Or (Array (Number))) (Or (Number))))
  (defun how-can-sum t values 
    (if (< t 0) 0 
      (if (= t 0) (Array) 
        (do 
          (defvar res 0)
          (some? value (lambda x . . (do
            (setf res (how-can-sum (- t x) values))
            (if (and (Array? res) (= -1 (array-index-of res x))) (set (length res) res x))))) 
          res))))
  ; abs
  (deftype abs (Lambda (Or (Number)) (Or (Number))))
  (defun abs n (- (^ n (>> n 31)) (>> n 31)))
  ; max-bit
  (deftype max-bit (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
  (defun max-bit a b (- a (& (- a b) (>> (- a b) 31))))
  ; max-bit
  (deftype min-bit (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
  (defun min-bit a b (- a (& (- a b) (>> (- b a) 31))))
  ; clamp-bit
  (deftype clamp-bit (Lambda (Or (Number)) (Or (Number)) (Or (Number)) (Or (Number))))
  (defun clamp-bit x min max (do 
     (setf x (- x (& (- x max) (>> (- max x) 31))))
     (- x (& (- x min) (>> (- x min) 31)))))
  ; bit-power-of-two? ; (and x (not (& x (- x 1)))
  (deftype bit-power-of-two? (Lambda (Or (Number)) (Or (Boolean))))
  (defun bit-power-of-two? value 
    (and 
      (= (& value (- value 1)) 0) 
      (not (= value 0))))
   ; average-bit 
  (deftype average-bit (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
  (defun average-bit a b (>> (+ a b) 1))
  ; toggle-bit
  (deftype toggle-bit (Lambda (Or (Number)) (Or (Number)) (Or (Number)) (Or (Number))))
  (defun toggle-bit n a b (^ a b n))
  ; odd-bit?
  (deftype odd-bit? (Lambda (Or (Number)) (Or (Number))))
  (defun odd-bit? n (= (& n 1) 1))
  ; same-sign-bit? 
  (deftype same-sign-bit? (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
  (defun same-sign-bit? a b (>= (^ a b) 0))
  ; modulo-bit
  (deftype modulo-bit (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
  (defun modulo-bit numerator divisor (& numerator (- divisor 1)))
  ; set-bit
  (deftype set-bit (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
  (defun set-bit n bit (| n (<< 1 bit)))
  ; clear-bit
  (deftype clear-bit (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
  (defun clear-bit n bit (& n (~ (<< 1 bit))))
  ; power-of-two-bit
  (deftype power-of-two-bit (Lambda (Or (Number)) (Or (Number))))
  (defun power-of-two-bit n (<< 2 (- n 1)))
  ; count-number-of-ones-bit
  (deftype count-number-of-ones-bit (Lambda (Or (Number)) (Or (Number))))
  (defun count-number-of-ones-bit n (do 
    (defvar count 0)
    (loop defun iter 
      (if n (do 
        (setf n (& n (- n 1)))
        (setf count (+ count 1))
        (iter)) count))
    (iter)))
  ; check-n-is-one-bit
  (deftype check-n-is-one-bit (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
  (defun check-n-is-one-bit N nth (type (& N (<< 1 nth)) Boolean))
  ; possible-subsets-bit
  (deftype possible-subsets-bit (Lambda (Or (Array)) (Or (Array))))
  (defun possible-subsets-bit A (do 
    (defconstant 
          items (Array) 
          N (length A))
    (defun iter-i i (do
      (when (< i (<< 1 N)) (do 
        (defconstant current (Array))
        (set items (length items) current)
        (iter-j 0 (lambda j (do (when (& i (<< 1 j)) (set current (length current) (get A j))))))
        (iter-i (+ i 1)))))) 
    (defun iter-j j cb (when (< j N) (do (cb j) (iter-j (+ j 1) cb)))) (iter-i 0) items))
  ; largest-power 
  (deftype largest-power (Lambda (Or (Number)) (Or (Number))))
  (defun largest-power N (do 
    ; changing all right side bits to 1.
    (setf N (| N (>> N 1)))
    (setf N (| N (>> N 2)))
    (setf N (| N (>> N 4)))
    (setf N (| N (>> N 8)))
    ; as now the number is 2 * x-1,
    ; where x is required answer,
    ; so adding 1 and dividing it by
    (>> (+ N 1) 1)))
  ; floor
  (deftype floor (Lambda (Or (Number)) (Or (Number))))
  (defun floor n (| n 0))
  ; round
  (deftype round (Lambda (Or (Number)) (Or (Number))))
  (defun round n (| (+ n 0.5) 0))
  ; euclidean-mod
  (deftype euclidean-mod (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
  (defun euclidean-mod a b (mod (+ (mod a b) b) b))
  ; euclidean-div
  (deftype euclidean-div (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
  (defun euclidean-div a b (do 
                      (defconstant q (* a (/ b)))
                      (if (< (mod a b) 0) (if (> b 0) (- q 1) (+ q 1)) q)))
  ; euclidean-distance
  (deftype euclidean-distance (Lambda (Or (Number)) (Or (Number)) (Or (Number)) (Or (Number)) (Or (Number))))
  (defun euclidean-distance x1 y1 x2 y2 (do
    (defconstant 
      a (- x1 x2) 
      b (- y1 y2))
    (sqrt (+ (* a a) (* b b)))))
  ; manhattan-distance
  (deftype manhattan-distance (Lambda (Or (Number)) (Or (Number)) (Or (Number)) (Or (Number)) (Or (Number))))
  (defun manhattan-distance x1 y1 x2 y2 (+ (abs (- x2 x1)) (abs (- y2 y1))))
  ; power
  (deftype power (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
  (defun power base exp 
    (if (< exp 0) 
        (if (= base 0) 
        (throw "Attempting to divide by 0 in (power)")
        (/ (* base (power base (- (* exp -1) 1))))) 
        (if (= exp 0) 1
          (if (= exp 1) base
            (* base (power base (- exp 1)))))))
  ; prime-factors
  (deftype prime-factors (Lambda (Or (Number)) (Or (Number))))
  (defun prime-factors n (do 
    (defvar a (Array) f 2)
    (loop defun iterate (if (> n 1) (do 
      (if (= (mod n f) 0) 
        (do 
          (set a (length a) f)
          (setf n (* n (/ f))))
        (setf f (+ f 1)))
      (iterate)) a))
      (iterate)))
    ; greatest-common-divisor
    (deftype greatest-common-divisor (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
    (defun greatest-common-divisor a b (do (loop defun gcd a b (if (= b 0) a (gcd b (mod a b)))) (gcd a b)))
    ; least-common-divisor
    (deftype least-common-divisor (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
    (defun least-common-divisor a b (* a b (/ (greatest-common-divisor a b))))
    ; remainder
    (deftype remainder (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
    (defun remainder n d (do (loop defun remain n d (if (< n d) n (remain (- n d) d))) (remain n d)))
    ; factorial
    (deftype factorial (Lambda (Or (Number)) (Or (Number))))
    (defun factorial n (if (<= n 0) 1 (* n (factorial (- n 1)))))
    ; fibonacci
    (deftype fibonacci (Lambda (Or (Number)) (Or (Number))))
    (defun fibonacci n (if (< n 2) n (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
    ; fibonacci-memoized
    (deftype fibonacci-memoized (Lambda (Or (Number)) (Or (Array (Array (Number)) (Or (Number))))))
    (defun fibonacci-memoized n memo (if (< n 2) n
        (if (hash-table? memo n) (hash-table-get memo n)
        (do
          (defconstant cache (+ (fibonacci-memoized (- n 1) memo) (fibonacci-memoized (- n 2) memo)))
          (hash-table-add! memo n cache)
          cache))))
    ; prime?
    (deftype prime? (Lambda (Or (Number)) (Or (Number))))
    (defun prime? n (do 
        (loop defun iter i end (do 
            (defconstant it-is (not (= (mod n i) 0)))
            (if (and (<= i end) it-is) (iter (+ i 1) end) it-is)))
      (or (= n 2) (iter 2 (sqrt n)))))
  
    ; chinese-remainder-theorem
    (deftype chinese-remainder-theorem (Lambda (Or (Array (Number))) (Or (Number))))
    (defun chinese-remainder-theorem items
      (do 
        (defvar result (car (car items)))
          (reduce 
            (cdr items) 
            (lambda step item . . (do 
            (defconstant 
                id (car item)
                index (car (cdr item)))
            (loop defun rem time
              (unless (= (mod (+ time index) id) 0) 
                (rem (+ time step)) 
                time))
            (setf result (rem result))
            (* step id))) 
            result)
          result))
    ; euclid-inverse-mod
    (deftype euclid-inverse-mod (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
    (defun euclid-inverse-mod a m (do 
          (defvar 
              m0 m 
              x0 0 
              x1 1)
          (unless (= m 1) (do 
              ; Apply extended Euclid Algorithm
              (loop defun euclid a (if (> a 1) (do
              ; q is quotient
              (defvar 
                  q (floor (* a (/ m))) 
                  t m)
              ; m is remainder now, process
              ; same as euclid's algo
              (setf m (mod a m))
              (setf a t)
              (setf t x0)
              (setf x0 (- x1 (* q x0)))
              (setf x1 t)
              (euclid a))))
              (euclid a)
              ; Make x1 positive
              (when (< x1 0) (setf x1 (+ x1 m0)))
              x1))))
    ;  range
      (deftype range (Lambda (Or (Number)) (Or (Number)) (Or (Array (Number)))))
      (defun range start end (do
        (defconstant array (Array))
        (loop defun iterate i bounds (do
          (set array (length array) (+ i start))
          (if (< i bounds) (iterate (+ i 1) bounds) array)))
        (iterate 0 (- end start))))
    ; sequance
      (deftype sequance (Lambda (Or (Number)) (Or (Number)) (Or (Number)) (Or (Array (Number)))))
      (defun sequance end start step (do
        (defconstant array (Array))
        (loop defun iterate i bounds (do
          (set array (length array) (+ i start))
          (if (< i bounds) (iterate (+ i step) bounds) array)))
        (iterate 0 (- end start))))
      ; arithmetic-progression
      (deftype arithmetic-progression (Lambda (Or (Number)) (Or (Number)) (Or (Array (Number)))))
      (defun arithmetic-progression n lim (do
        (defconstant array (Array))
        (loop defun iterate i bounds (do
          (set array (length array) (+ i n))
          (if (< i bounds) (iterate (+ i n) bounds) array)))
        (iterate 0 (- lim n))))
   ; map
      (deftype map (Lambda (Array (Or (Number))) (Or (Function)) (Or (Array (Number)))))
      (defun map array callback (do 
        (defconstant new-array (Array))
        (defvar i 0)
        (loop defun iterate i bounds (do
          (set new-array i (callback (get array i) i array))
          (if (< i bounds) (iterate (+ i 1) bounds) new-array)))
        (iterate 0 (- (length array) 1))))
    ; reduce
    (deftype reduce (Lambda (Array (Or (Number))) (Or (Function)) (Or (Number)) (Or (Number) (Array (Number)))))
    (defun reduce array callback initial (do
    (loop defun iterate i bounds (do
      (setf initial (callback initial (get array i) i array))
      (if (< i bounds) (iterate (+ i 1) bounds) initial)))
    (iterate 0 (- (length array) 1))))
  ; summation
  (deftype summation (Lambda (Or (Array (Number))) (Or (Number))))
  (defun summation array (reduce array (lambda a b . . (+ a b)) 0))
  ; product
  (deftype product (Lambda (Or (Array (Number))) (Or (Number))))
  (defun product array (reduce array (lambda a b . . (* a b)) 1))
  ; adjacent-difference
  (deftype adjacent-difference (Lambda (Or (Array (Number))) (Or (Function)) (Or (Array (Number)))))
  (defun adjacent-difference array callback (do 
    (defconstant len (length array))
    (unless (= len 1) 
      (do (defconstant result (Array (car array)))
      (loop defun iterate i (if (< i len) (do 
      (set result i (callback (get array (- i 1)) (get array i)))
      (iterate (+ i 1))) result))
      (iterate 1)) array)))
      ; permutations
      (deftype permutations (Lambda (Or (Array (Number))) (Or (Array (Array (Number))))))
      (defun permutations permutation (do 
        (defconstant  
          len (length permutation)
          result (Array (type permutation Array))
          c (Array len length))
        (defvar 
          k 0 
          p 0)
          (loop defun while-true i 
            (if (< i len)
              (do (if (< (get c i) i) 
                (do 
                  (setf k (and (mod i 2) (get c i)))
                  (setf p (get permutation i))
                  (set permutation i (get permutation k))
                  (set permutation k p)
                  (set c i (+ (get c i) 1))
                  (setf i 1)
                  (set result (length result) (type permutation Array)))
                  (set c i 0))
                  (while-true (+ i 1)))))
          (while-true 1)
          result))
  ; levenshtein-distance
  (deftype levenshtein-distance (Lambda (Or (String)) (Or (String)) (Or (Number))))
  (defun levenshtein-distance a b (do 
    (defconstant s (type a Array) 
            t (type b Array)) 
      
      (or (cond 
        (not (length s)) (length t)
        (not (length t)) (length a)
      ) (do 
        (defconstant arr (Array))
        (loop defun iterate-i i (when (<= i (length s)) (do
          (set arr i (Array i))
          (loop defun iterate-j j (when (<= j (length t)) (do 
            (set (get arr i) j 
              (if (= i 0) j 
                (minimum (Array 
                  (+ (get (get arr (- i 1)) j) 1)
                  (+ (get (get arr i) (- j 1)) 1)
                  (+ (get (get arr (- i 1)) (- j 1)) (not (= (get s (- j 1)) (get t (- i 1)))))))))
              (iterate-j (+ j 1)))))
            (iterate-j 1)
          (iterate-i (+ i 1)))))
        (iterate-i 0)
        (get (get arr (length t)) (length s))))))
    ; add-seconds
    (deftype add-seconds (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
    (defun add-seconds date-time seconds (+ date-time (* seconds 1000)))
    ; add-minutes
    (deftype add-minutes (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
    (defun add-minutes date-time minutes (+ date-time (* minutes 1000 60)))
    ; add-hours
    (deftype add-hours (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
    (defun add-hours date-time hours (+ date-time (* hours 1000 60 60)))
    ; add-days
    (deftype add-days (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
    (defun add-days date-time days (+ date-time (* days 1000 60 60 24)))
    ; add-months
    (deftype add-months (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
    (defun add-months date-time months (+ date-time (* months 1000 60 60 24 30)))
    ; add-years
    (deftype add-years (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
    (defun add-years date-time years (+ date-time (* years 1000 60 60 24 365)))
    ; sub-seconds
    (deftype sub-seconds (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
    (defun sub-seconds date-time seconds (- date-time (* seconds 1000)))
    ; sub-minutes
    (deftype sub-minutes (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
    (defun sub-minutes date-time minutes (- date-time (* minutes 1000 60)))
    ; sub-hours
    (deftype sub-hours (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
    (defun sub-hours date-time hours (- date-time (* hours 1000 60 60)))
    ; sub-days
    (deftype sub-days (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
    (defun sub-days date-time days (- date-time (* days 1000 60 60 24)))
    ; sub-months
    (deftype sub-months (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
    (defun sub-months date-time months (- date-time (* months 1000 60 60 24 30)))
    ; sub-years
    (deftype sub-years (Lambda (Or (Number)) (Or (Number)) (Or (Number))))
    (defun sub-years date-time years (- date-time (* years 1000 60 60 24 365)))
    (Array 
      (Array "max" max)
      (Array "min" min) 
      (Array "maximum" maximum)
      (Array "minimum" minimum) 
      (Array "odd?" odd?) 
      (Array "even?" even?) 
      (Array "abs" abs)
      (Array "floor" floor)
      (Array "round" round)
      (Array "euclidean-mod" euclidean-mod)
      (Array "euclidean-div" euclidean-div)
      (Array "range" range)
      (Array "sequance" sequance)
      (Array "arithmetic-progression" arithmetic-progression)
      (Array "summation" summation)
      (Array "product" product)
      (Array "greatest-common-divisor" greatest-common-divisor)
      (Array "least-common-divisor" least-common-divisor)
      (Array "remainder" remainder)
      (Array "factorial" factorial)
      (Array "fibonacci" fibonacci)
      (Array "fibonacci-memoized" fibonacci-memoized)
      (Array "can-sum?" can-sum?)
      (Array "how-can-sum" how-can-sum)
      (Array "adjacent-difference" adjacent-difference)
      (Array "clamp" clamp)
      (Array "manhattan-distance" manhattan-distance)
      (Array "euclidean-distance" euclidean-distance)
      (Array "normalize" normalize)
      (Array "linear-interpolation" linear-interpolation)
      (Array "power" power)
      (Array "radians" radians)
      (Array "sin" sin)
      (Array "cos" cos)
      (Array "sign" sign)
      (Array "square" square)
      (Array "average" average)
      (Array "sqrt" sqrt)
      (Array "prime?" prime?)
      (Array "euclid-inverse-mod" euclid-inverse-mod)
      (Array "chinese-remainder-theorem" chinese-remainder-theorem)
      (Array "gauss-sum-sequance" gauss-sum-sequance)
      (Array "gauss-sum" gauss-sum)
      (Array "prime-factors" prime-factors)
      (Array "levenshtein-distance" levenshtein-distance)
      (Array "binomial-coefficient" binomial-coefficient)
      (Array "max-bit" max-bit)
      (Array "min-bit" min-bit)
      (Array "clamp-bit" clamp-bit)
      (Array "bit-power-of-two?" bit-power-of-two?)
      (Array "average-bit" average-bit)
      (Array "toggle-bit" toggle-bit)
      (Array "odd-bit?" odd-bit?)
      (Array "same-sign-bit?" same-sign-bit?)
      (Array "modulo-bit" modulo-bit)
      (Array "set-bit" set-bit)
      (Array "clear-bit" clear-bit)
      (Array "power-of-two-bit" power-of-two-bit)
      (Array "count-number-of-ones-bit" count-number-of-ones-bit)
      (Array "check-n-is-one-bit" check-n-is-one-bit)
      (Array "possible-subsets-bit" possible-subsets-bit)
      (Array "largest-power" largest-power)
      (Array "permutations" permutations)
      (Array "reduce" reduce)
      (Array "map" map)
      (Array "PI" PI)
      (Array "E" E)
      (Array "circumference" circumference)
      (Array "positive?" positive?)
      (Array "negative?" negative?)
      (Array "zero?" zero?)
      (Array "divisible?" divisible?)
      (Array "nth-digit" nth-digit)
      (Array "add-seconds" add-seconds)
      (Array "add-minutes" add-minutes)
      (Array "add-hours" add-hours)
      (Array "add-days" add-days)
      (Array "add-months" add-months)
      (Array "add-years" add-years)
      (Array "sub-seconds" sub-seconds)
      (Array "sub-minutes" sub-minutes)
      (Array "sub-hours" sub-hours)
      (Array "sub-days" sub-days)
      (Array "sub-months" sub-months)
      (Array "sub-years" sub-years)
   )
))
; (/ math lib)