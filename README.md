# Qey

<p align="center">
<img width="100" src="./assets/images/key-logo.svg"/>
</p>

Share lisp programs in the browser

```lisp
(import std "map" "scan" "array-in-bounds?" "deep-flat" "for-each" "find" "index-of" "select" "except" "any?" "reduce")
(import math "euclidean-mod")
(import ds "hash-table" "hash-table?" "hash-table-add!" "hash-table-get")

(defconstant INSERT (function items (set (car items) (length (car items)) (cdr items))))
(defconstant CREATE (function args (do (defconstant columns (Array (cdr args))) (hash-table-add! TABLES (car args) columns) columns)))

(defun LOG value (if (atom? value) (' (' "Atom") (' value)) (do
(defconstant output ())
(set output (length output) (' "Array"))
(loop defun iterate array i (if (< i (length array)) (do (set output (length output) (' (get array i))) (iterate array (+ i 1))) output))
(iterate value 0))))

(defun TABLE name (if (hash-table? TABLES name) (hash-table-get TABLES name) ()))
(defun INC (do (defvar c 0) (lambda (setf c (+ c 1)))))
(defun NEXT_VAL seq (seq))
(defconstant TABLES (hash-table 10))
(defconstant employee_id_seq (INC))

(go
 (CREATE
  "Employees"
  "id" "Name" "Age" "Country")
 (INSERT (NEXT_VAL employee_id_seq) "Washington" "44" "USA")
 (INSERT (NEXT_VAL employee_id_seq) "Lincoln" "34" "USA")
 (INSERT (NEXT_VAL employee_id_seq) "Bush" "53" "USA")
)
(TABLE "Employees")
```
