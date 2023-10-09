; (std lib)
(defun std (do
  ; modules
  ; push!  
  (deftype push! (Lambda (Or (Array)) (Or (Array) (Number) (Integer) (String) (Function)) (Or (Array))))
  (defun push! array value (set array (length array) value))
  ; pop!
  (deftype pop! (Lambda (Or (Array)) (Or (Array))))
  (defun pop! array (set array -1))
  ; drop!
  (deftype drop! (Lambda (Or (Array)) (Or (Array) (Number) (Integer) (String) (Function))))
  (defun drop! array (when (length array) (do (defconstant last (get array -1)) (set array -1) last)))
  ; array-in-bounds? 
  (deftype array-in-bounds? (Lambda (Or (Array)) (Or (Number)) (Or (Boolean))))
  (defun array-in-bounds? array index (and (< index (length array)) (>= index 0)))
  ; array-of-atoms?
  (deftype array-of-atoms? (Lambda (Or (Array)) (Or (Boolean))))
  (defun array-of-atoms? array (if (not (length array)) 1 (if (atom? (car array)) (array-of-atoms? (cdr array)) 0)))
  ; cartesian-product
  (deftype cartesian-product (Lambda (Or (Array)) (Or (Array)) (Or (Array (Array)))))
  (defun cartesian-product a b (reduce a (lambda p x . . (merge p (map b (lambda y . . (Array x y))))) (Array)))
  ; neighborhood
  (deftype neighborhood (Lambda (Or (Array (Array (Number)))) (Or (Array (Array (Number)))) (Or (Number)) (Or (Number)) (Or (Function)) (Or (Number))))
  (defun neighborhood array directions y x callback
      (reduce directions (lambda sum dir . . (do
          (defconstant
              dy (+ (car dir) y)
              dx (+ (car (cdr dir)) x))
          (+ sum (when (and (array-in-bounds? array dy) (array-in-bounds? (get array dy) dx)) (callback (get (get array dy) dx) dir))))) 0))
      ; join
      (deftype join (Lambda (Or (Array)) (Or (String)) (Or (String))))
      (defun join array delim (reduce array (lambda a x i . (if (> i 0) (concatenate a delim (type x String)) (type x String))) ""))
      ; repeat
      (deftype repeat (Lambda (Or (Number)) (Or (Function)) (Or (Array))))
      (defun repeat n fn (map (Array n length) (lambda . . . (fn))))
      ; split-by-lines
      (deftype split-by-lines (Lambda (Or (String)) (Or (Array (String)))))
      (defun split-by-lines string (regex-match string "[^\n]+"))
      ; split-by
      (deftype split-by (Lambda (Or (String)) (Or (String)) (Or (Array (String)))))
      (defun split-by string delim (regex-match string (concatenate "[^" delim "]+")))
      ; trim
      (deftype trim (Lambda (Or (String)) (Or (String))))
      (defun trim string (regex-replace string "^ +| +$" ""))
      ; array-of-numbers
      (deftype array-of-numbers (Lambda (Or (Array)) (Or (Array (Number)))))
      (defun array-of-numbers array (map array (lambda x . . (type x Number))))
      ; concat
      (deftype concat (Lambda (Or (Array)) (Or (Array)) (Or (Array))))
      (defun concat array1 array2 (do
        (loop defun iterate i bounds (do
        (when (< i (length array2)) (set array1 (length array1) (get array2 i)))
        (if (< i bounds) (iterate (+ i 1) bounds) array1)))
      (iterate 0 (- (length array2) 1))))
      ; merge
      (deftype merge (Lambda (Or (Array)) (Or (Array)) (Or (Array))))
      (defun merge array1 array2 (do
        (loop defun iterate i bounds (do
        (set array1 (length array1) (get array2 i))
        (if (< i bounds) 
          (iterate (+ i 1) bounds)
        array1
        )))
      (iterate 0 (- (length array2) 1))))
      ; map
      (deftype map (Lambda (Or (Array)) (Or (Function)) (Or (Array))))
      (defun map array callback (do
        (defconstant new-array (Array))
        (defvar i 0)
        (loop defun iterate i bounds (do
          (set new-array i (callback (get array i) i array))
          (if (< i bounds) (iterate (+ i 1) bounds) new-array)))
        (iterate 0 (- (length array) 1))))
      ; for-each
      (deftype for-each (Lambda (Or (Array)) (Or (Function)) (Or (Array) (Number) (Integer) (String) (Function))))
      (defun for-each array callback (do
        (loop defun iterate i bounds (do
          (callback (get array i) i array)
          (if (< i bounds) (iterate (+ i 1) bounds) array)))
        (iterate 0 (- (length array) 1))))
  ; for-n
  (deftype for-n (Lambda (Or (Number)) (Or (Function)) (Or (Array) (Number) (Integer) (String) (Function))))
  (defun for-n N callback (do
    (loop defun iterate i (do 
        (defconstant res (callback i))
        (if (< i N) (iterate (+ i 1)) res))) 
        (iterate 0)))
  ; for-range
  (deftype for-range (Lambda (Or (Number)) (Or (Number)) (Or (Function)) (Or (Array) (Number) (Integer) (String) (Function))))
  (defun for-range start end callback (do
    (loop defun iterate i (do 
        (defconstant res (callback i))
        (if (< i end) (iterate (+ i 1)) res))) 
        (iterate start)))
  ; count-of
  (deftype count-of (Lambda (Or (Array)) (Or (Function)) (Or (Number))))
  (defun count-of array callback (do
    (defvar amount 0)
    (loop defun iterate i bounds (do
      (defconstant current (get array i))
      (when (callback current i array) (setf amount (+ amount 1)))
      (if (< i bounds) (iterate (+ i 1) bounds) amount)))
    (iterate 0 (- (length array) 1))))
    ; count-of
  (deftype number-of (Lambda (Or (Array)) (Or (Function)) (Or (Number))))
  (defun number-of array callback (do
    (defvar amount 0)
    (loop defun iterate i bounds (do
      (defconstant current (get array i))
      (when (callback current) (setf amount (+ amount 1)))
      (if (< i bounds) (iterate (+ i 1) bounds) amount)))
    (iterate 0 (- (length array) 1))))
  ; partition 
  (deftype partition (Lambda (Or (Array)) (Or (Number)) (Or (Array (Array)))))
  (defun partition array n (reduce array (lambda a x i . (do 
        (if (mod i n) (set (defconstant last-a (get a -1)) (length last-a) x) (set a (length a) (Array x))) a)) 
        ()))
  ; select
  (deftype select (Lambda (Or (Array)) (Or (Function)) (Or (Array))))
  (defun select array callback (do
    (defconstant new-array (Array))
    (loop defun iterate i bounds (do
      (defconstant current (get array i))
      (when (callback current i array) 
        (set new-array (length new-array) current))
      (if (< i bounds) (iterate (+ i 1) bounds) new-array)))
    (iterate 0 (- (length array) 1))))
; take
(deftype take (Lambda (Or (Array)) (Or (Function)) (Or (Array))))
(defun take array callback (do 
  (loop defun iterate arr output
    (if (length arr) (iterate (cdr arr) (if (callback (car arr)) (set output (length output) (car arr)) output)) output))
  (iterate array ())))
; scan
(deftype scan (Lambda (Or (Array)) (Or (Function)) (Or (Array))))
(defun scan arr callback (do 
  (loop defun iterate arr output
    (if (length arr) (iterate (cdr arr) (set output (length output) (callback (car arr)))) output))
  (iterate arr ())))
; fold 
(deftype fold (Lambda (Or (Array)) (Or (Function)) (Or (Array) (Number) (Integer) (String)) (Or (Array) (Number) (Integer) (String))))
(defun fold arr callback initial (do 
  (loop defun iterate arr output
    (if (length arr) (iterate (cdr arr) (setf output (callback output (car arr)))) output))
  (iterate arr initial)))
; zip
(deftype zip (Lambda (Or (Array)) (Or (Array)) (Or (Array (Array)))))
(defun zip A B (do 
  (loop defun iterate a b output
    (if (and (length a) (length b)) (iterate (cdr a) (cdr b) (set output (length output) (Array (car a) (car b)))) output))
  (iterate A B ())))
  ; except
  (deftype except (Lambda (Or (Array)) (Or (Function)) (Or (Array))))
  (defun except array callback (do
    (defconstant new-array (Array))
    (loop defun iterate i bounds (do
      (defconstant current (get array i))
      (otherwise (callback current i array) 
        (set new-array (length new-array) current))
      (if (< i bounds) (iterate (+ i 1) bounds) new-array)))
    (iterate 0 (- (length array) 1))))
  ; keep
  (deftype keep (Lambda (Or (Array)) (Or (Function)) (Or (Array))))
  (defun keep array callback (do
    (defconstant new-array (Array))
    (loop defun iterate i bounds (do
      (defconstant current (get array i))
      (when (callback current) 
        (set new-array (length new-array) current))
      (if (< i bounds) (iterate (+ i 1) bounds) new-array)))
    (iterate 0 (- (length array) 1))))
  ; remmove
  (deftype remove (Lambda (Or (Array)) (Or (Function)) (Or (Array))))
  (defun remove array callback (do
    (defconstant new-array (Array))
    (loop defun iterate i bounds (do
      (defconstant current (get array i))
      (otherwise (callback current) 
        (set new-array (length new-array) current))
      (if (< i bounds) (iterate (+ i 1) bounds) new-array)))
    (iterate 0 (- (length array) 1))))
  ; every?
  (deftype every? (Lambda (Or (Array)) (Or (Function)) (Or (Boolean))))
  (defun every? array callback (do
      (defvar bol 1)
      (loop defun iterate i bounds (do
        (defconstant res (callback (get array i) i array))
        (boole bol (type res Boolean))
        (if (and res (< i bounds)) (iterate (+ i 1) bounds) bol)))
      (iterate 0 (- (length array) 1))))
  ; some?
  (deftype some? (Lambda (Or (Array)) (Or (Function)) (Or (Boolean))))
  (defun some? array callback (do
      (defvar bol 1)
      (loop defun iterate i bounds (do
        (defconstant res (callback (get array i) i array))
        (boole bol (type res Boolean))
        (if (and (not res) (< i bounds)) (iterate (+ i 1) bounds) bol)))
      (iterate 0 (- (length array) 1))))
  ; all?
  (deftype all? (Lambda (Or (Array)) (Or (Function)) (Or (Boolean))))
  (defun all? array callback (do
      (defvar bol 1)
      (loop defun iterate i bounds (do
        (defconstant res (callback (get array i)))
        (boole bol (type res Boolean))
        (if (and res (< i bounds)) (iterate (+ i 1) bounds) bol)))
      (iterate 0 (- (length array) 1))))
  ; any?
  (deftype any? (Lambda (Or (Array)) (Or (Function)) (Or (Boolean))))
  (defun any? array callback (do
      (defvar bol 1)
      (loop defun iterate i bounds (do
        (defconstant res (callback (get array i)))
        (boole bol (type res Boolean))
        (if (and (not res) (< i bounds)) (iterate (+ i 1) bounds) bol)))
      (iterate 0 (- (length array) 1))))
  ; reduce
  (deftype reduce (Lambda (Or (Array)) (Or (Function)) (Or (Array) (Number) (Integer) (String)) (Or (Array) (Number) (Integer) (String))))
  (defun reduce array callback initial (do
    (loop defun iterate i bounds (do
      (setf initial (callback initial (get array i) i array))
      (if (< i bounds) (iterate (+ i 1) bounds) initial)))
    (iterate 0 (- (length array) 1))))
    ; accumulate
    (deftype accumulate (Lambda (Or (Array)) (Or (Function)) (Or (Array) (Number) (Integer) (String))))
    (defun accumulate array callback (do
      (defvar initial (get array 0))
      (loop defun iterate i bounds (do
        (setf initial (callback initial (get array i) i array))
        (if (< i bounds) (iterate (+ i 1) bounds) initial)))
      (iterate 0 (- (length array) 1))))
    ; iteration 
    (deftype iteration (Lambda (Or (Function)) (Or (Number)) (Or (Array))))
    (defun iteration callback n (reduce (defconstant arr (Array n length)) (lambda a . i . (set a i (callback (get a -1) i))) arr))
    ; iteration 
    (defun repeated-apply initial callback i (do 
      (loop defun iterate result callback i (if (> i 0) (iterate (callback result) callback (- i 1)) result))
      (iterate initial callback i)))
    ; deep-flat
    (deftype deep-flat (Lambda (Or (Array)) (Or (Array))))
    (defun deep-flat arr (do 
      (defconstant new-array (Array)) 
      (defconstant flatten (lambda item 
        (if (and (Array? item) (length item)) 
              (for-each item (lambda x . . (flatten x))) 
              (otherwise (Array? item) (set new-array (length new-array) item)))))
      (flatten arr) 
      new-array))
    ; fetch
    (deftype fetch (Lambda (Or (Array)) (Or (Function)) (Or (Array) (Number) (Integer) (String) (Function))))
    (defun fetch array callback (do
            (loop defun iterate i bounds (do
              (defconstant 
                current (get array i)
                has (callback current))
              (if (and (not has) (< i bounds))
                (iterate (+ i 1) bounds) 
                (when has current))))
                (iterate 0 (- (length array) 1))))
     ; find
    (deftype find (Lambda (Or (Array)) (Or (Function)) (Or (Array) (Number) (Integer) (String) (Function))))
    (defun find array callback (do
            (loop defun iterate i bounds (do
              (defconstant 
                current (get array i) 
                has (callback current i array))
              (if (and (not has) (< i bounds))
                (iterate (+ i 1) bounds) 
                (when has current))))
                (iterate 0 (- (length array) 1))))
    ; find-index
    (deftype find-index (Lambda (Or (Array)) (Or (Function)) (Or (Number))))
    (defun find-index array callback (do
      (defvar idx -1 has-found 0)
      (loop defun iterate i bounds (do
        (defconstant current (get array i))
        (boole has-found (callback current i array))
        (if (and (not has-found) (< i bounds))
          (iterate (+ i 1) bounds) 
          (setf idx i))))
          (iterate 0 (- (length array) 1))
          (if has-found idx -1)))
    ; index-of
    (deftype index-of (Lambda (Or (Array)) (Or (Number) (Integer) (String)) (Or (Number))))
    (defun index-of array target (do
      (defvar idx -1 has-found 0)
      (loop defun iterate i bounds (do
        (defconstant current (get array i))
        (boole has-found (and (atom? current) (= target current)))
        (if (and (not has-found) (< i bounds))
          (iterate (+ i 1) bounds) 
          (setf idx i))))
          (iterate 0 (- (length array) 1))
          (if has-found idx -1)))
      ; last-index-of
      (deftype last-index-of (Lambda (Or (Array)) (Or (Number) (Integer) (String)) (Or (Number))))
      (defun last-index-of array target (do
        (defvar idx -1 has-found 0)
        (loop defun iterate i (do
          (defconstant current (get array i))
          (boole has-found (= target current))
          (if (and (not has-found) (>= i 0))
            (iterate (- i 1)) 
            (setf idx i))))
            (iterate (- (length array) 1))
            (if has-found idx -1)))
      ; index-of-starting-from
    (deftype index-of-starting-from (Lambda (Or (Array)) (Or (Number) (Integer) (String)) (Or (Number)) (Or (Number))))
    (defun index-of-starting-from array target start (do
      (defvar idx -1 has-found 0)
      (loop defun iterate i bounds (do
        (defconstant current (get array i))
        (boole has-found (= target current))
        (if (and (not has-found) (< i bounds))
          (iterate (+ i 1) bounds) 
          (setf idx i))))
          (iterate start (- (length array) 1))
          (if has-found idx -1)))
      ; last-index-of-ending-from
      (deftype last-index-of-ending-from (Lambda (Or (Array)) (Or (Number) (Integer) (String)) (Or (Number)) (Or (Number))))
      (defun last-index-of-ending-from array target end (do
        (defvar idx -1 has-found 0)
        (loop defun iterate i (do
          (defconstant current (get array i))
          (boole has-found (= target current))
          (if (and (not has-found) (>= i 0))
            (iterate (- i 1)) 
            (setf idx i))))
            (iterate (- (length array) 1 (* end -1)))
            (if has-found idx -1)))
      ; array-index-of
      (deftype array-index-of (Lambda (Or (Array)) (Or (Number) (Integer) (String)) (Or (Number))))
      (defun array-index-of array target 
        (do
          (if (= (length array) 0) -1 
            (do 
              (defvar idx -1 has-found 0)
              (loop defun iterate i bounds (do
                (defconstant current (get array i))
                (boole has-found (= target current))
                (if (and (not has-found) (< i bounds))
                  (iterate (+ i 1) bounds) 
                  (setf idx i))))
                  (iterate 0 (- (length array) 1))
                  (if has-found idx -1)))))
      ; quick-sort
      (deftype quick-sort (Lambda (Or (Array (Number)) (Array (String)) (Array (Integer))) (Or (Array (Number)) (Array (String)) (Array (Integer)))))
      (defun quick-sort arr (do
        (if (<= (length arr) 1) arr
        (do
          (defconstant 
            pivot (get arr 0) 
            left-arr (Array) 
            right-arr (Array))
        (loop defun iterate i bounds (do
          (defconstant current (get arr i))
          (if (< current pivot) 
              (set left-arr (length left-arr) current)
              (set right-arr (length right-arr) current))
          (when (< i bounds) (iterate (+ i 1) bounds))))
          (iterate 1 (- (length arr) 1))
      (defconstant left-sorted (go left-arr (quick-sort)))
      (go 
        left-sorted
        (set (length left-sorted) pivot)
        (concat (quick-sort right-arr)))))))
      ; reverse 
      (deftype reverse (Lambda (Or (Array)) (Or (Array))))
      (defun reverse array (do
        (defconstant len (length array))
        (if (> len 1) (do
        (defconstant 
          reversed (Array len length)
          offset (- len 1))
        (loop defun iterate i bounds (do
          (set reversed (- offset i) (get array i))
          (if (< i bounds) (iterate (+ i 1) bounds) reversed)))
        (iterate 0 offset)) array)))
      ; empty!
      (deftype empty! (Lambda (Or (Array)) (Or (Array))))
      (defun empty! array (do (loop defun iterate (if (length array) (do (set array -1) (iterate)) array)) (iterate)))
      ; empty?
      (deftype empty? (Lambda (Or (Array)) (Or (Boolean))))
      (defun empty? array (not (length array)))
      ; binary-search
      (deftype binary-search (Lambda (Or (Array)) (Or (Number) (Integer) (String)) (Or (Number) (Integer) (String) (Array))))
      (defun binary-search 
              array target (do
        (loop defun search 
              arr target start end (do
          (when (<= start end) (do 
              (defconstant 
                index (| (* (+ start end) 0.5) 0)
                current (get arr index))
              (if (= target current) target
                (if (> current target) 
                  (search arr target start (- index 1))
                  (search arr target (+ index 1) end))))))) 
        (search array target 0 (length array))))
      ; sort-by-length 
      (deftype sort-by-length (Lambda (Or (Array)) (Or (Array (Number))) (Or (Array))))
      (defun sort-by-length array order (map order (lambda x . . (find array (lambda y . . (= (- (length y) 1) x))))))
      ; order-array
      (deftype order-array (Lambda (Or (Array)) (Or (Array (Number))) (Or (Array))))
      (defun order-array array order (map (Array (length array) length) (lambda . i . (get array (get order i)))))
     
      (deftype left-pad (Lambda (Or (String)) (Or (Number)) (Or (String)) (Or (String))))
      (defun left-pad str n ch (do 
        (setf n (- n (length str)))
        (loop defun pad i str (if (< i n) (pad (+ i 1) (setf str (concatenate ch str))) str))
        (pad 0 str)))
        ; left-pad
      (deftype right-pad (Lambda (Or (String)) (Or (Number)) (Or (String)) (Or (String))))
      (defun right-pad str n ch (do 
        (setf n (- n (length str)))
        (loop defun pad i str (if (< i n) (pad (+ i 1) (setf str (concatenate str ch))) str))
        (pad 0 str)))
      ; occurances_count
      (deftype character-occurances-in-string (Lambda (Or (String)) (Or (String)) (Or (Number))))
      (defun character-occurances-in-string string letter (do
        (defvar 
          array (type string Array)
          bitmask 0
          zero (char-code "a" 0)
          count 0
          has-at-least-one 0)
        (loop defun iterate i bounds  (do
            (defconstant 
              ch (get array i)
              code (- (char-code ch 0) zero)
              mask (<< 1 code))
            (if (and (when (= ch letter) (boole has-at-least-one 1))
                (not (= (& bitmask mask) 0))) 
                (setf count (+ count 1))
                (setf bitmask (| bitmask mask)))
            (if (< i bounds) (iterate (+ i 1) bounds) 
            (+ count has-at-least-one))))
            (iterate 0 (- (length array) 1))))
    ;  to-upper-case
    (deftype to-upper-case (Lambda (Or (String)) (Or (String))))
    (defun to-upper-case str (do
     (defconstant 
            arr (Array) 
            n (length str))
      (loop defun iter i (if (< i n) (do 
        (defconstant current-char (char-code str i))
        (set arr i 
          (if (and (>= current-char 97) (<= current-char 122))
            (- current-char 32)
            current-char
        ))
        (iter (+ i 1))) 
        (make-string arr)))
        (iter 0)))
    ;  to-lower-case
    (deftype to-lower-case (Lambda (Or (String)) (Or (String))))
    (defun to-lower-case str (do
      (defconstant 
            arr (Array) 
            n (length str))
      (loop defun iter i (if (< i n) (do 
        (defconstant current-char (char-code str i))
        (set arr i 
          (if (and (>= current-char 65) (<= current-char 90))
            (+ current-char 32)
            current-char
        ))
        (iter (+ i 1))) 
        (make-string arr)))
        (iter 0)))
    ; split-by-n-lines
    (deftype split-by-n-lines (Lambda (Or (String)) (Or (Number)) (Or (Array (Array (String))))))
    (defun split-by-n-lines string n (go string (regex-replace (concatenate "(\n){" (type n String) "}") "௮") (regex-match "[^௮]+") (map (lambda x . . (regex-match x "[^\n]+")))))
    ; split
    (deftype split (Lambda (Or (String)) (Or (String)) (Or (Array (String)))))
    (defun split string separator (do
        (defconstant 
          sep-arr (type separator Array)
          array (type string Array)
          skip (length sep-arr))
        (defvar cursor "")
        (loop defun iterate result i bounds
          (if (< (if (every? sep-arr (lambda y j . (or (<= (length array) (+ i j)) (= (get array (+ i j)) y))))
                (do 
                  (setf i (+ i skip -1))
                  (set result (length result) cursor)
                  (setf cursor "")
                  i)
                (do (setf cursor (concatenate cursor (get array i))) i)) bounds) 
                    (iterate result (+ i 1) bounds) result))
        (set (defconstant iteration-result (iterate (Array) 0 (- (length array) 1))) (length iteration-result) cursor)))
      ; slice 
      (deftype slice (Lambda (Or (Array)) (Or (Number)) (Or (Number)) (Or (Array))))
      (defun slice array start end (do 
        (defconstant bounds (- end start) out (Array bounds length))
        (loop defun iterate i 
          (if (< i bounds) 
              (do 
                (set out i (get array (+ start i))) 
                (iterate (+ i 1)))
              out))
              (iterate 0)))
      (deftype clone (Lambda (Or (Array)) (Or (Array))))
      (defun clone array (do 
      (defconstant 
              bounds (length array) 
              out (Array bounds length))
        (loop defun iterate i 
          (if (< i bounds) 
              (do 
                (defconstant current (get array i))
                (set out i (if (Array? current) (clone current) current)) 
                (iterate (+ i 1)))
              out))
              (iterate 0)))
      ; slice-if-index
      (deftype slice-if-index (Lambda (Or (Array)) (Or (Function)) (Or (Array))))
      (defun slice-if-index array callback (reduce array (lambda a b i . (if (callback i) (set a (length a) b) a)) (Array)))
      ; slice-if
      (deftype slice-if (Lambda (Or (Array)) (Or (Function)) (Or Array)))
      (defun slice-if array callback (reduce array (lambda a b i . (if (callback b i) (set a (length a) b) a)) (Array)))
      ; window
      (deftype window (Lambda (Or (Array)) (Or (Number)) (Or (Array (Array)))))
      (defun window array n (go array
        (reduce (lambda acc current i all 
          (if (>= i n) 
            (set acc (length acc) (slice all (- i n) i)) acc)) (Array))))
      ; equal 
      (deftype equal? (Lambda (Or (Array) (Number) (Integer) (String)) (Or (Array) (Number) (Integer) (String)) (Or (Boolean))))
      (defun equal? a b 
      (or (and (atom? a) (atom? b) (= a b)) 
      (and (Array? a) 
            (= (length a) (length b)) 
              (not (some? a (lambda . i . (not (equal? (get a i) (get b i)))))))))
    (Array 
      (Array "push!" push!)
      (Array "pop!" pop!)
      (Array "drop!" drop!)
      (Array "sort-by-length" sort-by-length)  
      (Array "order-array" order-array)  
      (Array "array-in-bounds?" array-in-bounds?)  
      (Array "join" join)
      (Array "trim" trim)
      (Array "split-by-lines" split-by-lines)
      (Array "split-by" split-by)
      (Array "split-by-n-lines" split-by-n-lines)
      (Array "split" split)
      (Array "array-of-numbers" array-of-numbers)
      (Array "concat" concat)
      (Array "merge" merge)
      (Array "map" map)
      (Array "for-each" for-each)
      (Array "for-n" for-n)
      (Array "for-range" for-range)
      (Array "select" select)
      (Array "except" except)
      (Array "keep" keep)
      (Array "remove" remove)
      (Array "reduce" reduce)
      (Array "deep-flat" deep-flat)
      (Array "find" find)
      (Array "find-index" find-index)
      (Array "quick-sort" quick-sort)
      (Array "reverse" reverse)
      (Array "binary-search" binary-search)
      (Array "character-occurances-in-string" character-occurances-in-string)
      (Array "every?" every?)
      (Array "some?" some?)
      (Array "index-of" index-of)
      (Array "last-index-of" last-index-of)
      (Array "index-of-starting-from" index-of-starting-from)
      (Array "last-index-of-ending-from" last-index-of-ending-from)
      (Array "array-index-of" array-index-of)
      (Array "accumulate" accumulate)
      (Array "count-of" count-of)
      (Array "partition" partition)
      (Array "slice" slice)
      (Array "slice-if" slice-if)
      (Array "slice-if-index" slice-if-index)
      (Array "equal?" equal?)
      (Array "neighborhood" neighborhood)
      (Array "repeat" repeat)
      (Array "window" window)
      (Array "left-pad" left-pad)
      (Array "right-pad" right-pad)
      (Array "to-upper-case" to-upper-case)
      (Array "to-lower-case" to-lower-case)
      (Array "cartesian-product" cartesian-product)
      (Array "repeated-apply" repeated-apply)
      (Array "iteration" iteration)
      (Array "empty!" empty!)
      (Array "clone" clone)
      (Array "empty?" empty?)
      (Array "take" take)
      (Array "zip" zip)
      (Array "scan" scan)
      (Array "fold" fold)
      (Array "fetch" fetch)
      (Array "any?" any?)
      (Array "all?" all?)
      (Array "number-of" number-of)
  )))
; (/ std lib)