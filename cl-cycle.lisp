(in-package :cl-cycle)

;;;---------------------------- COMMON-LISP ----------------------------

(defmethod kaprekar-constant ((lst list) &optional (n 10))
 "Application of the Kaprekar's algorithm for a list of integers of base n.
The result is the path leading to the cycle -- if it exists -- and the cycle itself."
 (cl-cycle::a-kaprekar-l lst n))

(defmethod kaprekar-constant ((x integer) &optional n)
  (declare (ignore n))
  (kaprekar-constant (cl-cycle:10->n x 10)))

(defmethod kaprekar-constant ((x t) &optional n) (declare (ignore x n)) nil)

(defmethod rhythmic-sieve ((crible list) &key field-x i j (optimize :yes))
 "Realize the matrix i j -- if it exists -- to allow a given sieve to be shifted by one step horizontally and vertically in order to realize two cycles.
The option 'optimize' allows to minimize | i - j | when 'optimize' is set to :yes.
When 'optimize' is set to :in-field-x, the optimisation is done inside the limited field by the maximum value of the sieve and the value of 'field-x'."
 (when (member optimize '(:yes :no :in-field-x)) (mapcar #'butlast (cl-cycle::cycle-x1 crible field-x i j optimize))))

(defmethod rhythmic-sieve ((a t) &key b c d e) (declare (ignore a b c d e)) nil)

(defmethod pea-pattern ((lst list))
 "The pea pattern is a variation of the look-and-say sequence that consists to read off the digits of the previous number, counting the number of digits in groups of the same digit, and read them in ascending order until the sequence becomes the comment of themself.

For example:
1 is read off as 'one 1' or 11
11 is read off as 'two 1' or 21
21 is read off as 'one 1, one 2' or 1112
1112 is read off as 'three 1, one 2' or 3112
and so one until 21322314 which is its own comment.

The result is the path leading to the cycle -- if it exists -- and the cycle itself."
 (cl-cycle::commentaire-cyclique1 lst))

(defmethod pea-pattern ((x integer))
  (pea-pattern (cl-cycle:10->n x 10)))

(defmethod pea-pattern ((x t)) (declare (ignore x)) nil)

(defmethod symmetric-group ((lst list))
  "The symmetric group allows to enumerate all cycles from a list according the position of its elements."
  (let ((tmp (cl-cycle::cfp1 lst)))
    (when tmp (list tmp))))

(defmethod symmetric-group ((x t)) (declare (ignore x)) nil)

(defmethod symmetric-permutation ((lst list) (code-lst list))
 "The symmetric permutation consists to attribute a number -- from the code-lst -- to a given set of chromatic durations (or any kind of symbols) and to read them always in the same order."
 (list (butlast (cl-cycle::perm-sym1 lst code-lst))))

(defmethod symmetric-permutation ((a t) (b t)) (declare (ignore a b)) nil)

(defmethod circular-permutation ((lst list) (init-base integer) (circ-base integer))
  "Returns a converted circular permutation done in circ-base to the init-base."
  (let ((tmp (cl-cycle::a-fill-lst (butlast (cl-cycle::p-c-b lst init-base circ-base)))))
    (when tmp (list tmp))))

(defmethod circular-permutation ((a t) (b t) (c t)) (declare (ignore a b c)) nil)

(defmethod lorenz-discretisation ((x number))
 "Returns the cycle done from the starting point x between 0 and 1 by doubling its value in modulo 1.
The result is the path leading to the cycle -- if it exists -- and the cycle itself."
 (cl-cycle::l-d x))

(defmethod lorenz-discretisation ((x t)) (declare (ignore x)) nil)

(defmethod collatz-conjecture ((x integer))
 "Returns the conjectural cycle (4 2 1) according to the initial value x such as:
	if x is even then divide x by 2,
	if x is odd then multiply x by 3 and add 1.
Note that the interesting part is the path leading to the trivial cycle (4 2 1)."
 (cond ((= 1 x) (list (list 1) (list 4 2 1)))
       ((= 2 x) (list (list 2 1) (list 4 2 1)))
       ((= 4 x) (list (list 4 2 1)))
       (t (cl-cycle::a-collatz x))))

(defmethod collatz-conjecture ((x t)) (declare (ignore x)) nil)

(defun interlace-cycle (&rest motifs)
  "Depending on the length of each list, a cycle is generated according to the least common multiple. So, each list will be repeated
lcm(Union^n_i=1 |l_i|)/|l_i| times to complete the motivational cycle of interlacing.
The first value is the interlace cycle, the second value is the number of repetition of each respective list as arguments."
  (when (loop for i in motifs always (listp i))
    (let* ((lst (remove nil motifs))
	   (len (mapcar #'length lst))
	   (n (apply #'lcm len)))
      (values
       (list (apply #'mapcar #'list (loop for e in lst collect (loop repeat (/ n (length e)) append e))))
       (mapcar #'(lambda (x) (/ n x)) len)))))
  
(defmethod euclidean-rhythm ((n integer) (m integer) &key ratio)
  "Euclidean rhythms allow spacing out <n> events (or onsets) across <m> positions (or pulses or beats) as evenly possible, according to the Bjorklund’s algorithm."
  (let ((tmp (cl-cycle::rtm-euclidean m n)))
    (when tmp
      (if ratio
	  (let (r s) (loop for i in tmp if (= 1 i) do (push (length s) r) (setf s (list i)) else do (push i s)) (list (reverse (butlast (push (length s) r)))))
	  (list tmp)))))

(defmethod euclidean-rhythm ((a t) (b t) &key c) (declare (ignore a b c)) nil)

;;;-----------------------------------------------------------------

(defmethod 10->n ((x integer) (n integer))
 "Convert an integer to a list of digits in base n."
 (cl-cycle::a-10->n x n))

(defmethod n->10 ((lst list) (n integer))
 "Convert a list of digits in base n to an integer in base 10."
 (cl-cycle::a-n->10 lst n))

;;;-----------------------------------------------------------------

(defmethod mk-integer-lst ((lst list))
  "Allows to built a list of integers from any kind of list."
  (cl-cycle::a-mk-integer lst))

(defmethod fill-digit-lst ((lst list) &optional (n nil))
  "Allows to add zero to match a given length of a list of any base.
In the case of a list of lists, the length is the maximum of the list or n."
  (cl-cycle::a-fill-lst lst n))

;;;------------------------------ END ------------------------------
