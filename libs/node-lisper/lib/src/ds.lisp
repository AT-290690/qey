; (ds lib)
(defun ds (do
; modules
  ; hash-index
  (deftype hash-index (Lambda (Or (Array (Array))) (Or (Number) (Integer) (String)) (Or (Number))))
  (defun hash-index 
    table key 
      (do
        (defconstant 
          prime-num 31
          key-arr (type (type key String) Array))
        (defvar total 0)
        (loop defun find-hash-index i bounds (do 
          (defconstant 
            letter (get key-arr i) 
            value (- (char-code letter 0) 96))
          (setf total (euclidean-mod (+ (* total prime-num) value) (length table)))
          (if (< i bounds) (find-hash-index (+ i 1) bounds) total)))
        (find-hash-index 0 
        (if (< (- (length key-arr) 1) 100) (- (length key-arr) 1) 100))))

    ; (Hash Table)
    ; (go 
    ;   (hash-table-make (Array 
    ;     (Array "name" "Anthony") 
    ;     (Array "age" 32) 
    ;     (Array "skills" 
    ;       (Array "Animation" "Programming"))))
    ;   (log)
    ; )
      ; hash-table-add!
    (deftype hash-table-add! (Lambda (Or (Array (Array (Array)))) (Or (Number) (Integer) (String)) (Or (Array) (Number) (String) (Integer) (Function)) (Or (Array (Array (Array))))))
    (defun hash-table-add! 
      table key value 
        (do
          (defconstant idx (hash-index table key))
          (otherwise (array-in-bounds? table idx) (set table idx (Array)))
          (defconstant 
            current (get table idx)
            len (length current)
            index (if len (find-index current (lambda x . . (= (get x 0) key))) -1)
            entry (Array key value))
          (if (= index -1)
            (set current (length current) entry)
            (set current index entry)
          )
          table))
    ; hash-table-remove!
    (deftype hash-table-remove! (Lambda (Or (Array (Array (Array)))) (Or (Number) (Integer) (String)) (Or (Array (Array (Array))))))
    (defun hash-table-remove! 
      table key 
        (do
          (defconstant idx (hash-index table key))
          (otherwise (array-in-bounds? table idx) (set table idx (Array)))
          (defconstant 
            current (get table idx)
            len (length current)
            index (if len (find-index current (lambda x . . (= (get x 0) key))) -1))
          (otherwise (= index -1) (and (set current index (get current -1)) (set current -1)))
          table))  
     ; hash-table-empty!
    (deftype hash-table-empty! (Lambda (Or (Array (Array (Array)))) (Or (Array (Array (Array))))))
    (defun hash-table-empty! 
      table 
        (do
          (map table (lambda x . . (empty! x)))))       
    ; hash table_has 
    (deftype hash-table? (Lambda (Or (Array (Array (Array)))) (Or (Number) (Integer) (String)) (Or (Boolean))))
    (defun hash-table? table key 
      (and (array-in-bounds? table (defconstant idx (hash-index table key))) (and (length (defconstant current (get table idx))) (>= (index-of (car current) key) 0))))
    ; hash-table-get
    (deftype hash-table-get (Lambda (Or (Array (Array (Array)))) (Or (Number) (Integer) (String)) (Or (Number) (Integer) (String) (Array))))
    (defun hash-table-get
      table key 
        (do
          (defconstant idx (hash-index table key))
          (if (array-in-bounds? table idx) 
            (do
              (defconstant current (get table idx))
              (go current
                (find (lambda x . . (= key 
                        (go x (get 0)))))
                (get 1))))))
    ; hash-table
    (deftype hash-table (Lambda (Or (Number)) (Or (Array (Array)))))
    (defun hash-table 
      size 
        (map (Array size length) (lambda . . . (Array))))
    ; hash-table-make
    (defun hash-table-make 
      items 
        (do
          (defconstant 
            len (- (length items) 1)
            table (hash-table (* len len)))
          (loop defun add i (do
            (defconstant item (get items i))
            (hash-table-add! table (get item 0) (get item 1))
          (if (< i len) (add (+ i 1)) table)))
          (add 0)))
      ; hash-set-add!
    (deftype hash-set-add! (Lambda (Or (Array (Array))) (Or (Number) (Integer) (String)) (Or (Array (Array)))))
    (defun hash-set-add! 
      table key 
        (do
          (defconstant idx (hash-index table key))
          (otherwise (array-in-bounds? table idx) (set table idx (Array)))
          (defconstant 
            current (get table idx)
            len (length current)
            index (if len (find-index current (lambda x . . (= x key))) -1)
            entry key)
          (if (= index -1)
            (set current (length current) entry)
            (set current index entry)
          )
          table))
    ; hash-set-remove!
    (deftype hash-set-remove! (Lambda (Or (Array (Array))) (Or (Number) (Integer) (String)) (Or (Array (Array)))))
    (defun hash-set-remove! 
      table key 
        (do
          (defconstant idx (hash-index table key))
          (otherwise (array-in-bounds? table idx) (set table idx (Array)))
          (defconstant 
            current (get table idx)
            len (length current)
            index (if len (find-index current (lambda x . . (= x key))) -1)
            entry key)
          (otherwise (= index -1) (and (set current index (get current -1)) (set current -1)))
          table))
    ; hash-set-empty!
    (deftype hash-set-empty! (Lambda (Or (Array (Array))) (Or (Array (Array)))))
    (defun hash-set-empty! 
      table 
        (do
          (map table (lambda x . . (empty! x)))))    
    ; hash table_has 
    (deftype hash-set? (Lambda (Or (Array (Array))) (Or (Number) (Integer) (String)) (Or (Boolean))))
    (defun hash-set? table key 
      (and (array-in-bounds? table (defconstant idx (hash-index table key))) (and (length (defconstant current (get table idx))) (>= (index-of current key) 0))))
    ; hash-set-get
    (deftype hash-set-get (Lambda (Or (Array (Array))) (Or (Number) (Integer) (String)) (Or (Number) (Integer) (String))))
    (defun hash-set-get table key (do
          (defconstant idx (hash-index table key))
          (if (array-in-bounds? table idx) (do
              (defconstant current (get table idx))
              (go current
                (find (lambda x . . (= key x))))))))
    ; hash-set
    (deftype hash-set (Lambda (Or (Number)) (Or (Array (Array)))))
    (defun hash-set size (map (Array size length) (lambda . . . (Array))))
    ; hash-set-make
    (defun hash-set-make items (do
        (defconstant 
          len (- (length items) 1)
          table (hash-set (* len len)))
        (loop defun add i (do
          (defconstant item (get items i))
          (hash-set-add! table item)
        (if (< i len) (add (+ i 1)) table)))
        (add 0)))
  ; (/ Hash Set)

    ; (Binary Tree)
    ; (go 
    ; (binary-tree-node 1)
    ; (binary-tree-set-left! (go 
    ;                         (binary-tree-node 2) 
    ;                         (binary-tree-set-left! 
    ;                           (go (binary-tree-node 4) 
    ;                               (binary-tree-set-right! 
    ;                               (binary-tree-node 5))))))
    ; (binary-tree-set-right! (binary-tree-node 3))
    ; (binary-tree-get-left)
    ; (binary-tree-get-left)
    ; (binary-tree-get-right))
    ; binary-tree-node
    (deftype binary-tree-node (Lambda 
      (Or (Number) (String) (Integer) (Array) (Function)) 
      (And 
       (Array (Array (String) (Number))) 
       (Array (Array (String) (String)))
       (Array (Array (String) (Integer)))
       (Array (Array (String) (Function)))
       (Array (Array (String) (Array))))))
    (defun binary-tree-node value 
      (Array 
        (Array "value" value)
        (Array "left"  (Array))
        (Array "right" (Array))))
    ; binary-tree-get-left
     (deftype binary-tree-get-left (Lambda 
      (And 
       (Array (Array (String) (Number))) 
       (Array (Array (String) (String)))
       (Array (Array (String) (Integer)))
       (Array (Array (String) (Function)))
       (Array (Array (String) (Array)))) 
      (And 
       (Array (Array (String) (Number))) 
       (Array (Array (String) (String)))
       (Array (Array (String) (Integer)))
       (Array (Array (String) (Function)))
       (Array (Array (String) (Array))))))
    (defun binary-tree-get-left node (get node 1))
    ; binary-tree-get-right
    (deftype binary-tree-get-right (Lambda 
      (And 
       (Array (Array (String) (Number))) 
       (Array (Array (String) (String)))
       (Array (Array (String) (Integer)))
       (Array (Array (String) (Function)))
       (Array (Array (String) (Array)))) 
      (And 
       (Array (Array (String) (Number))) 
       (Array (Array (String) (String)))
       (Array (Array (String) (Integer)))
       (Array (Array (String) (Function)))
       (Array (Array (String) (Array))))))
    (defun binary-tree-get-right node (get node 2))
    ; binary-tree-set-left!
    (deftype binary-tree-set-left! (Lambda 
      (And 
       (Array (Array (String) (Number))) 
       (Array (Array (String) (String)))
       (Array (Array (String) (Integer)))
       (Array (Array (String) (Function)))
       (Array (Array (String) (Array)))) 
      (And 
       (Array (Array (String) (Number))) 
       (Array (Array (String) (String)))
       (Array (Array (String) (Integer)))
       (Array (Array (String) (Function)))
       (Array (Array (String) (Array))))
      (And 
       (Array (Array (String) (Number))) 
       (Array (Array (String) (String)))
       (Array (Array (String) (Integer)))
       (Array (Array (String) (Function)))
       (Array (Array (String) (Array))))))
    (defun binary-tree-set-left! tree node (set tree 1 node))
    ; binary-tree-set-right!
    (deftype binary-tree-set-right! (Lambda 
      (And 
       (Array (Array (String) (Number))) 
       (Array (Array (String) (String)))
       (Array (Array (String) (Integer)))
       (Array (Array (String) (Function)))
       (Array (Array (String) (Array)))) 
      (And 
       (Array (Array (String) (Number))) 
       (Array (Array (String) (String)))
       (Array (Array (String) (Integer)))
       (Array (Array (String) (Function)))
       (Array (Array (String) (Array))))
      (And 
       (Array (Array (String) (Number))) 
       (Array (Array (String) (String)))
       (Array (Array (String) (Integer)))
       (Array (Array (String) (Function)))
       (Array (Array (String) (Array))))))
    (defun binary-tree-set-right! tree node (set tree 2 node)) 
    ; binary-tree-get-value
    (deftype binary-tree-get-right (Lambda 
      (And 
       (Array (Array (String) (Number))) 
       (Array (Array (String) (String)))
       (Array (Array (String) (Integer)))
       (Array (Array (String) (Function)))
       (Array (Array (String) (Array)))) 
      (Or (Number) (String) (Integer) (Function) (Array))))
    (defun binary-tree-get-value node (car (cdr (get node 0))))
    ; (/ Binary Tree)
  (Array 
    (Array "hash-index" hash-index)
    (Array "hash-table-add!" hash-table-add!)
    (Array "hash-table-remove!" hash-table-remove!)
    (Array "hash-table-empty!" hash-table-empty!)
    (Array "hash-table?" hash-table?)
    (Array "hash-table-get" hash-table-get)
    (Array "hash-table" hash-table)
    (Array "hash-table-make" hash-table-make)
    (Array "hash-set-add!" hash-set-add!)
    (Array "hash-set?" hash-set?)
    (Array "hash-set-remove!" hash-set-remove!)
    (Array "hash-set-empty!" hash-set-empty!)
    (Array "hash-set-get" hash-set-get)
    (Array "hash-set" hash-set)
    (Array "hash-set-make" hash-set-make)
    (Array "binary-tree-node" binary-tree-node)
    (Array "binary-tree-get-left" binary-tree-get-left)
    (Array "binary-tree-get-right" binary-tree-get-right)
    (Array "binary-tree-set-right!" binary-tree-set-right!)
    (Array "binary-tree-set-left!" binary-tree-set-left!)
    (Array "binary-tree-get-value" binary-tree-get-value)
   )))
; (/ ds lib)
