; (str lib)
(defun str (do
  ; modules
  ; join
  (deftype join (Lambda (Or (Array)) (Or (String)) (Or (String))))
  (defun join array delim (reduce array (lambda a x i . (if (> i 0) (concatenate a delim (type x String)) (type x String))) ""))
  ; trim
  (deftype trim (Lambda (Or (String)) (Or (String))))
  (defun trim string (regex-replace string "^ +| +$" ""))
  ; left-pad
  (deftype left-pad (Lambda (Or (String)) (Or (Number)) (Or (String)) (Or (String))))
  (defun left-pad str n ch (do 
    (setf n (- n (length str)))
    (loop defun pad i str (if (< i n) (pad (+ i 1) (setf str (concatenate ch str))) str))
    (pad 0 str)))
  ; right-pad
  (deftype right-pad (Lambda (Or (String)) (Or (Number)) (Or (String)) (Or (String))))
  (defun right-pad str n ch (do 
    (setf n (- n (length str)))
    (loop defun pad i str (if (< i n) (pad (+ i 1) (setf str (concatenate str ch))) str))
    (pad 0 str)))
  ; character-occurances-in-string
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
    ; to-upper-case
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
            current-char))
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
            current-char))
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
        ; split-by-lines
      (deftype split-by-lines (Lambda (Or (String)) (Or (Array (String)))))
      (defun split-by-lines string (regex-match string "[^\n]+"))
      ; split-by
      (deftype split-by (Lambda (Or (String)) (Or (String)) (Or (Array (String)))))
      (defun split-by string delim (regex-match string (concatenate "[^" delim "]+")))
    (Array 
      (Array "split-by-lines" split-by-lines)
      (Array "split-by" split-by)
      (Array "split-by-n-lines" split-by-n-lines)
      (Array "split" split)
      (Array "join" join)
      (Array "trim" trim)
      (Array "left-pad" left-pad)
      (Array "right-pad" right-pad)
      (Array "to-upper-case" to-upper-case)
      (Array "to-lower-case" to-lower-case)
      (Array "character-occurances-in-string" character-occurances-in-string)
  )))
; (/ str lib)