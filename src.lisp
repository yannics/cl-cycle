;;;  cl-cycle version 1.21.3
;;;  use freely and at your own risk :)

(in-package :cl-cycle)

;;;---------------------------- LISP-CODE ----------------------------

(defparameter *cycle-default-value* :seq
  "This concerns only the algorithms KAPREKAR-CONSTANT, PEA-PATTERN, LORENZ-DISCRETISATION and COLLATZ-CONJECTURE.
Possible keywords are:
 :seq   ---> (append path cycle)
 :lst   ---> (list path cycle)
 :path  ---> path
 :cycle ---> cycle
The path (if it exists) is the prepended sequence of the cycle itself.
Otherwise the result is a cycle if it exists, except for the SYMMETRIC-GROUP which is a list of cycles.")

(defun singleton (lst) (and (listp lst) (= 1 (length lst))))

(defun res-l (l kw)
  (case kw
    (:seq (if (singleton l) (car l) (apply #'append l)))
    (:lst l)
    (:path (unless (singleton l) (car l)))
    (:cycle (car (last l)))))
      
;; http://rosettacode.org/wiki/Prime_decomposition#Common_Lisp
(defun factor (n)
  "Return a list of factors of n."
  (when (> n 1)
    (loop with max-d = (isqrt n)
	  for d = 2 then (if (evenp d) (+ d 1) (+ d 2)) do
	  (cond ((> d max-d) (return (list n))) ; n is prime
		((zerop (rem n d)) (return (cons d (factor (truncate n d)))))))))

(defun flatten (lst)
  (if (endp lst)
      lst
      (if (atom (car lst))
	  (append (list (car lst)) (flatten (cdr lst)))
	  (append (flatten (car lst)) (flatten (cdr lst))))))

(defun xor (a b)
  (if (eq a nil)
      (if (eq b nil) nil t)
    (if (eq b nil) t nil)))

(defun drop-element (e set)
  (cond ((null set) '())
	((equal e (first set)) (rest set))
	(t (cons (first set) (drop-element e (rest set))))))
                      
(defun complementary (subset set)
  (cond ((null subset) set)
	((member (first subset) set)
	 (complementary (rest subset) (drop-element (first subset) set)))
	(t (complementary (rest subset) set))))

(defmethod bc-test ((x list) (m null))
  (loop for i in x always (numberp i)))

(defmethod bc-test ((x number) (m number))
  (and (integerp x) (not (< x m))))

(defmethod bc-test ((x list) (m number))
  (loop for i in x always (bc-test i m)))

(defmethod bc-test ((x t) (m null)) nil)
(defmethod bc-test ((x t) (m t)) nil)
(defmethod bc-test ((x null) (m null)) nil)
(defmethod bc-test ((x null) (m t)) nil)

;;;;-------------------;;;;
;;;; kaprekar-constant ;;;;
;;;;-------------------;;;;

(defun a-kaprek (l n)
  (- (a-n->10 (sort (copy-tree l) '>) n) (a-n->10 (sort (copy-tree l) '<) n)))

(defun cycle-l (a)
  "The result is a list indicating the loop applied to the list a."
  (let (r)
    (loop for i in (cdr a) until (equalp i (car a))
       do (push i r))
    (cons (car a) r)))

(defun trans-l (a)
  "The result is a list indicating the transition - if it exists - to the loop of the list a."
  (let (r)
    (loop for i in (reverse a) until (equalp i (car a))
       do (push i r))
    (reverse r)))

(defun result-l (a)
  (if (null (trans-l a))
      (cycle-l a)
      (list (trans-l a) (cycle-l a))))

(defun a-kaprekar-l (list-k &optional (n 10))
      (cond ((not (bc-test n 2)) nil)
	    ((not (bc-test list-k 0)) nil)
	    ((not (> n (apply #'max list-k))) nil)
	    (t
	     (let ((r (list list-k)))
	       (loop until (member (car r) (cdr r) :test #'equalp)
		  do
		    (push
		     (append (make-list (- (length list-k) (length (a-10->n (a-kaprek (car r) n) n))) :initial-element 0) (a-10->n (a-kaprek (car r) n) n))
		     r))
	       (result-l r)))))

;;;;-----------------------;;;;
;;;; symmetric-permutation ;;;;
;;;;-----------------------;;;;

(defun perm (list-p code-list-p)
  (mapcar #'second (sort (mapcar #'list code-list-p list-p) #'< :key #'car)))

(defun perm-sym1 (list-p code-list-p)
  (if (bc-test code-list-p nil)
      (let ((r (list list-p)) (p nil))
        (loop until (eq t p)
              do
              (push (perm (car r) code-list-p) r)
              (dolist (e (cdr r))
                (if (equalp (car r) e)
                    (setf p t)
		    (setf p nil))))
        (reverse r))
    list-p))

;;;;----------------;;;;
;;;; rhythmic-sieve ;;;;
;;;;----------------;;;;

(defun groupn (lst n)
  (let (r)
    (dotimes (i n (reverse r)) (push (nth i lst) r))))

(defun all-sub-g (lst)
  (let (r)
    (loop for x from 1 to (- (length lst) 1)
          do
          (dotimes (y (+ (- (length lst) x) 1))
            (push (groupn (nthcdr y lst) x) r)))
    (remove-duplicates r :test #'equalp)))

(defun soust-mult-x (a b)
  (- (apply #'* a) (apply #'* (complementary a (factor b)))))
   
(defun voir-x (x)
  (let (r)
    (loop for i in (all-sub-g (factor x))
          do
          (when (and (= 1 (gcd (apply #'* i) (apply #'* (complementary i (factor x))))) (not (= 0 (soust-mult-x i x))))
            (push (list (abs (soust-mult-x i x)) i (complementary i (factor x))) r)))
    (car (mapcar #'cdr (sort r #'< :key #'car)))))

(defun i&j1 (a) ; a is equal to (max crible) or field
  (sort (list (apply #'* (car (voir-x a))) (apply #'* (cadr (voir-x a)))) '<))

(defun opt-plus (x)
  (if (not (= 0 (apply #'- (i&j1 x))))
      (if (< (abs (apply #'- (i&j1 (1+ x)))) (abs (apply #'- (i&j1 x))))
          (if (= 0 (apply #'- (i&j1 (1+ x)))) (i&j1 x)
            (opt-plus (1+ x)))
        (i&j1 x))
    (opt-plus (1+ x))))

(defun opt-moins (x)
  (if (not (= 0 (apply #'- (i&j1 x))))
      (if (< (abs (apply #'- (i&j1 (1- x)))) (abs (apply #'- (i&j1 x))))
          (if (= 0 (apply #'- (i&j1 (1- x)))) (i&j1 x)
            (opt-moins (1- x)))
        (i&j1 x))
    (opt-moins (1- x))))

(defun i&j (x &key key)
  (cond ((equal key :plus-no) (if (null (voir-x x)) (i&j1 (1+ x)) (i&j1 x)))
        ((equal key :plus-yes) (opt-plus x))
        ((equal key :moins-no) (if (null (voir-x x)) (i&j1 (1- x)) (i&j1 x)))
        ((equal key :moins-yes) (opt-moins x))
        (t nil)))

(defun iorj1 (k x)
  (when (> x (* 2 k))
    (if (= 0 (rem x k))
        (if (= 1 (gcd k (/ x k)))
            (list k (/ x k)) nil)
      nil)))

(defun iorj-plus (k x) ; k = i or j
  (when (> x (* 2 k))
    (if (= 0 (rem x k))
        (if (= 1 (gcd k (/ x k)))
            (sort (list k (/ x k)) '<)
          (iorj-plus k (1+ x)))
      (iorj-plus k (1+ x)))))

(defun iorj-moins (k x) ; k = i or j
  (when (> x (* 2 k))
    (if (= 0 (rem x k))
        (if (= 1 (gcd k (/ x k)))
            (sort (list k (/ x k)) '<)
          (iorj-moins k (1- x)))
      (iorj-moins k (1- x)))))

(defun in-f-x (a b k)
  (if (eq k nil)
      (let (r)
        (loop for i from 0 to (- b a)
              do
              (when (voir-x (+ i a))
                (push (list (- (cadr (i&j1 (+ i a))) (car (i&j1 (+ i a))))
                            (i&j1 (+ i a))) r)))
        (if (null r) nil
          (cadr (assoc (apply #'min (mapcar 'car r)) r))))
    (when (< k (/ b 2))
      (let (r)
        (loop for i from 0 to (- b a)
              do
              (when (not (equal (iorj1 k (+ i a)) nil))
                (push (list (abs (- (cadr (iorj1 k (+ i a))) (car (iorj1 k (+ i a)))))
                            (iorj1 k (+ i a))) r)))
        (when (not (null r)) (cadr (assoc (apply #'min (mapcar 'car r)) r)))))))
    
(defun list-module (a)
  (let ((r nil))
    (dotimes (i a (reverse r)) (push i r))))

(defun mod-by1 (i j)
  "make a list of i element in modulo i*j"
  (let (r)
    (loop for a in (list-module i)
       do (push
	   (mod (* a (+ i 1)) (* i j))
	   r))
    (reverse r)))

(defun mod-by2 (i j list-from-mod-by1)
  "make a new list like mod-by1 from its last element (list-from-mod-by1)"
  (let ((r) (n (list (1+ (car (last list-from-mod-by1))))))
    (loop for a in (mapcar '* (make-list i :initial-element (+ i 1)) (list-module i))
       do (push
	   (mod (+ a (car n)) (* i j))
	   r))
    (reverse r)))

(defun x-matrice-mod (i j)
  "build a matrice of dimension i and j"
  (let ((r (list (mod-by1 i j))))
    (loop repeat (- j 1)
       do (push (mod-by2 i j (car r)) r))
    (reverse r)))

(defun mat-pairlst (i j)
  "assigns the matrix elements in their respective position"
  (mapcar #'list (flatten (x-matrice-mod i j)) (list-module (* i j))))

(defun place-crible-mat (cribl i j)
  "assigns the crible elements in their respective position"
  (let (r)
    (dolist (e cribl (reverse r))
      (push (assoc e (mat-pairlst i j)) r))))

(defun decall-crible-mat (new-crible i j)
  (let (r)
    (dolist (e (place-crible-mat new-crible i j) (reverse r))
      (push (cadr (assoc (mod (+ (cadr e) i) (* i j)) (let (r) (dolist (e (mat-pairlst i j) r) (push (reverse e) r))))) r))))

(defun cy (cribl i-j)
  (let (i j)
    (setq i (car i-j))
    (setq j (cadr i-j))
    (when (and (= 1 (gcd i j)) (not (= 1 i)) (not (= 1 j)))
      (let ((r (list (mapcar 'car (place-crible-mat cribl i j)))))
	(loop until (member (car r) (cdr r) :test #'equalp)
	   do
	     (push (decall-crible-mat (car r) i j) r))
	(reverse r)))))

(defun cy-x (cribl i-j)
  (when (and (not (null i-j)) (> (* (car i-j) (cadr i-j)) (apply #'max cribl)))
    (if (eq (cy cribl i-j) nil) nil
      (list (cy cribl i-j) (cy cribl (reverse i-j))))))

(defun cycle-x1 (crible field-x i j option-x)
  (when (listp crible)
    (cond ((and (not (eq field-x nil)) (not (bc-test field-x (apply #'max crible)))) nil)
          ((and (not (eq i nil)) (not (bc-test i 2))) nil)
          ((and (not (eq j nil)) (not (bc-test j 2))) nil)
          (t
           (when (bc-test crible 0)
	     (let (a k)
	       (setq a (1+ (apply #'max crible)))
	       (setq k (when (or (null i) (null j)) (if (null i) j i)))
	       (cond ((and (null field-x) (null i) (null j))
		      (case option-x
			(:no (cy-x crible (i&j a :key :plus-no)))
			(:yes (cy-x crible (i&j a :key :plus-yes)))
			(:in-field-x nil)))
		     ((and (not (null field-x)) (null i) (null j))
		      (case option-x
			(:no (cy-x crible (i&j field-x :key :moins-no)))
			(:yes (cy-x crible (i&j field-x :key :moins-yes)))
			(:in-field-x (cy-x crible (in-f-x a field-x k)))))
		     ((and (null field-x) (xor (null i) (null j)))
		      (case option-x
			(:in-field-x nil)
			(otherwise (cy-x crible (iorj-plus k a)))))
		     ((and (not (null field-x)) (xor (null i) (null j)))
		      (case option-x
			(:in-field-x (cy-x crible (in-f-x a field-x k)))
			(otherwise (when (>= (apply #'* (iorj-moins k field-x)) a)
				     (cy-x crible (iorj-moins k field-x))))))
		     ((and (null field-x) (not (null i)) (not (null j)))
		      (case option-x
			(:in-field-x nil)
			(otherwise (when (and (= 1 (gcd i j)) (>= (* i j) a)) (cy-x crible (list i j))))))
		     ((not (and (null field-x) (null i) (null j)))
		      (case option-x
			(otherwise (when (and (= 1 (gcd i j)) (>= (* i j) a) (>= field-x (* i j)))
				     (cy-x crible (list i j)))))))))))))

;;;;-------------;;;;
;;;; pea-pattern ;;;;
;;;;-------------;;;;

(defun count-1 (lst)
  (let (r)
    (dolist (e (remove-duplicates lst) r)
      (push (list (count e lst) e) r))
    (sort r '< :key 'cadr)))

(defun commentaire-cyclique1 (lst)
  (when (bc-test lst 0)
    (let ((r (list lst)))
      (loop until (member (car r) (cdr r) :test #'equalp)
	 do (push (flatten (count-1 (car r))) r))
      (result-l r))))

;;;;------------------------;;;;
;;;; kreuzspiel-permutation ;;;;
;;;;------------------------;;;;

(defun krpop (lst &optional ind)
  (unless ind (setf ind (floor (/ (length lst) 2))))
  (if (and (< 0 ind) (> (length lst) ind))
      (let ((p1 (subseq lst 0 ind))
	    (p2 (subseq lst ind)))
	(append
	 (cdr p1)
	 (last p2)
	 (list (car p1))
	 (butlast p2)))
      lst))

(defun a-kreuzspiel (lst &optional ind res)
  (if (member lst res :test #'equalp)
      (reverse res)
      (a-kreuzspiel (krpop lst ind) ind (cons lst res))))

;;;;----------------------;;;;
;;;; circular-permutation ;;;;
;;;;----------------------;;;;

(defun pop-circ (lst)
  (flatten (list (cdr lst) (car lst))))

(defun perm-circ (lst)
  (let ((r (list lst)))
    (loop repeat (length lst)
          do
          (push (pop-circ (car r)) r))
    (reverse r)))

(defun perm-circ-base1 (lst n-lst n-circ)
  (cond ((not (bc-test n-lst 2)) nil)
        ((not (bc-test n-circ 2)) nil)
        ((not (bc-test lst 0)) nil)
        (t
         (let ((r (perm-circ (a-10->n (a-n->10 lst n-lst) n-circ))) (s))
           (dolist (e r (reverse s))
             (push (a-10->n (a-n->10 e n-circ) n-lst) s))))))

(defun p-c-b (lst n-lst n-circ)
  (perm-circ-base1 lst n-lst n-circ))

;;;;-----------------;;;;
;;;; symmetric-group ;;;;
;;;;-----------------;;;;

(defun list-mod (a)
  (let ((r nil))
    (dotimes (i a (reverse r)) (push (1+ i) r))))

(defun 2list (l)
  (mapcar #'list (list-mod (length l)) l))

(defun boucle (l) ;l = (2list l)
  (if (null l) nil
    (let ((r (list (car l))))
      (loop until (assoc (cadr (car r)) r)
            do
            (push (assoc (cadr (car r)) l) r)) (reverse r))))

(defun rem-assoc (l1 l2)
  (let (r)
    (loop for i in (complementary (mapcar #'cadr l1) (mapcar #'cadr l2))
          do
          (push (assoc i l2) r)) r))

(defun c-f-p (l &optional r)
  (let ((b (boucle l)))
    (push (mapcar #'car b) r)
    (if (not (rem-assoc b l)) (reverse r)
	(c-f-p (reverse (rem-assoc b l)) r))))

(defun cfp1 (lst)
  (when (and (bc-test lst nil) (equalp (list-mod (length lst)) (sort (copy-tree lst) #'<)))
    (let ((r) (s (mapcar 'list (list-mod (length lst)) (sort (copy-tree lst) #'<))))
      (loop for i in (c-f-p (2list lst))
            do
            (push
             (let (z)
               (dolist (e i (mapcar #'cadr (reverse z)))
                 (push (assoc e s) z))) r)) (reverse r))))

;;;;-----------------------;;;;
;;;; lorenz-discretisation ;;;;
;;;;-----------------------;;;;

(defun xx (x)
  (let ((a (* 2 (rationalize x))))
    (if (> a 1) (- a 1) a)))

(defun chaos (i &optional r rat)
  (if r
      (if (member i r)
	  (if rat (cons i r) (cons (float i) (mapcar #'float r)))
	  (chaos (xx i) (push i r) rat))
      (chaos (xx i) (list (rationalize i)) (when (rationalp i) t))))

(defun l-d (x)
  (when (and (numberp x) (<= 0 x) (>= 1 x)) (result-l (chaos x))))

;;;;--------------------;;;;
;;;; collatz-conjecture ;;;;
;;;;--------------------;;;;

(defun a-collatz (n &optional (r (list n)))
  (let ((x (if (evenp n) (/ n 2) (+ (* 3 n) 1))))
    (if (member x r) (result-l (cons x r)) (a-collatz x (cons x r)))))

;;;;------------------;;;;
;;;; euclidean-rhythm ;;;;
;;;;------------------;;;;

(defun list! (x) (if (listp x) x (list x)))

(defun rtm-euclidean (len n &optional head tail)
  (when (or (and (bc-test len 0) (bc-test n 0) (>= len n)) (null len))
    (if (and head (> 2 (length tail)))
	(flatten (append head tail))
	(rtm-euclidean nil nil
		       ;; HEAD
		       (if len
			   (make-list n :initial-element 1)
			   (loop for i in head for j in tail collect (append (list! i) (list! j))))
		       ;; TAIL
		       (if len
			   (make-list (- len n) :initial-element 0)
			   (cond ((> (length head) (length tail)) (make-list (- (length head) (length tail)) :initial-element (car head)))
				 ((< (length head) (length tail)) (make-list (- (length tail) (length head)) :initial-element (car tail)))
				 (t nil)))))))
  
;;;;-------;;;;
;;;; utils ;;;;
;;;;-------;;;;

(defun a-n->10 (lst n)
  (when (and (listp lst) (bc-test lst 0) (bc-test n 2))
    (let (r)
      (dolist (e lst r)
        (push (* e (expt n (- (- (length lst) 1) (length r)))) r))
      (apply '+ r))))

(defun a-10->n (x n)
  (when (and (bc-test x 0) (bc-test n 2))
    (let (r)
      (if (zerop x) (push 0 r)
        (loop until (= 0 x)
              do
              (push (rem x n) r)
              (setf x (floor x n))))
      r)))

(defun a-mk-int-lst (lstIn short-lst)
  (loop for i in lstIn
        collect (cadr (assoc i (mapcar #'list short-lst (list-module (length short-lst))) :test #'equalp))))

(defun a-mk-integer (lst)
  (when (listp lst)
    (if (bc-test lst nil)
        (a-mk-int-lst lst (sort (copy-tree (remove-duplicates lst)) '<))
      (a-mk-int-lst lst (remove-duplicates lst :from-end t :test #'equalp)))))

(defun comp-lst (lst n)
  (if (= n (length lst)) lst
    (let ((l lst))
      (loop until (= n (length l))
          do
          (setf l (cons 0 l))) l)))

(defun a-fill-lst (lst &optional m)
  (if (and (listp lst) (loop for e in lst always (listp e)))
    (let* ((w (loop for i in lst maximize (length i)))
           (n (if (and m (integerp m) (>= m w)) m w)))
      (loop for x in lst
	 collect (comp-lst x n)))
    (if (and m (integerp m) (>= m (length lst))) (comp-lst lst m) lst)))

;;;------------------------------ END ------------------------------
