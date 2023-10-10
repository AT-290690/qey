# Qey

<p align="center">
<img width="100" src="./assets/images/key-logo.svg"/>
</p>

Embark on a transformative coding experience with Qeq, an advanced environment meticulously crafted for Lisp enthusiasts. Discover the art of elegant Lisp programming in a user-friendly browser interface, where your creativity knows no limits.

Key Features:

🚀 Effortless Browser-Based Lisp Programming: Qeq streamlines the process of writing Lisp programs, offering a smooth and intuitive interface accessible directly from your browser. Experience the unparalleled convenience of coding without the hassle of installations or setups.

📊 Dynamic Sheet Creation and CSV Export: Elevate your data visualization game with Qeq's robust capabilities. Craft intricate tables and visual representations with ease, and seamlessly export them into professional-grade CSV files. Qeq ensures your data speaks volumes, making your insights impactful and visually compelling.

🔗 Secure and Collaborative Code Sharing: Foster collaboration effortlessly by sharing your Lisp programs securely via unique, shareable links. Qeq promotes a collaborative ecosystem where ideas flourish, enabling professionals to engage, refine, and expand their knowledge base collectively.

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
