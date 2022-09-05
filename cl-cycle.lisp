(in-package :cl-cycle)

;;;---------------------------- COMMON-LISP ----------------------------

(defgeneric kaprekar-constant (in &key n r)
  (:documentation "Application of the Kaprekar's algorithm for a list of integers of base n.")
  (:method ((in list) &key (n 10) (r *cycle-default-value*))
    (res-l (a-kaprekar-l in n) r))
  (:method ((in integer) &key n (r *cycle-default-value*))
    (declare (ignore n))
    (kaprekar-constant (10->n in 10) :r r))
  (:method ((in t) &key n r)
    (declare (ignore in n r)) nil))

(defgeneric rhythmic-sieve (crible &key field-x i j optimize)
  (:documentation "Realize the matrix i j -- if it exists -- to allow a given sieve to be shifted by one step horizontally and vertically in order to realize two cycles.
The option 'optimize' allows to minimize | i - j | when 'optimize' is set to :yes.
When 'optimize' is set to :in-field-x, the optimisation is done inside the limited field by the maximum value of the sieve and the value of 'field-x'.")
  (:method ((crible list) &key field-x i j (optimize :yes))
    (when (member optimize '(:yes :no :in-field-x)) (mapcar #'butlast (cycle-x1 crible field-x i j optimize))))
  (:method ((crible t) &key field-x i j optimize)
    (declare (ignore crible field-x i j optimize)) nil))

(defgeneric pea-pattern (in &key r)
    (:documentation "The pea pattern is a variation of the look-and-say sequence that consists to read off the digits of the previous number, counting the number of digits in groups of the same digit, and read them in ascending order until the sequence becomes the comment of themself.
For example:
1 is read off as 'one 1' or 11
11 is read off as 'two 1' or 21
21 is read off as 'one 1, one 2' or 1112
1112 is read off as 'three 1, one 2' or 3112
and so one until 21322314 which is its own comment.
The result is the path leading to the cycle -- if it exists -- and the cycle itself.")
  (:method ((in list) &key (r *cycle-default-value*))
    (res-l (commentaire-cyclique1 in) r))
  (:method ((in integer) &key (r *cycle-default-value*))
    (pea-pattern (10->n in 10) :r r))
  (:method ((in t) &key r)
    (declare (ignore in r)) nil))

(defgeneric symmetric-group (lst)
  (:documentation "The symmetric group allows to enumerate all cycles from a list according the position of its elements.")
  (:method ((lst list))
    (let ((tmp (cfp1 lst))) tmp))
  (:method ((lst t))
    (declare (ignore lst)) nil))

(defgeneric symmetric-permutation (lst code-lst)
  (:documentation "The symmetric permutation consists to attribute a number -- from the code-lst -- to a given set of chromatic durations (or any kind of symbols) and to read them always in the same order.")
  (:method ((lst list) (code-lst list))
    (butlast (perm-sym1 lst code-lst)))
  (:method ((lst t) (code-lst t))
    (declare (ignore lst code-lst)) nil))

(defgeneric circular-permutation (lst init-base circ-base)
  (:documentation "Returns a converted circular permutation done in circ-base to the init-base.")
  (:method ((lst list) (init-base integer) (circ-base integer))
    (let ((tmp (a-fill-lst (butlast (p-c-b lst init-base circ-base))))) tmp))
  (:method ((lst t) (init-base t) (circ-base t))
    (declare (ignore lst init-base circ-base)) nil))

(defgeneric kreuzspiel-permutation (lst &optional ind)
  (:documentation "Crossing technique consisting to take the first and the last values of lst and places them in the middle (or optionally at a given indexed place) by opposite crossing recursively until the effectiveness of a cycle.")
  (:method ((lst list) &optional ind)
    (a-kreuzspiel lst ind))
  (:method ((lst t) &optional ind)
    (declare (ignore lst ind)) nil))

(defgeneric lorenz-discretisation (x &key r)
  (:documentation "Returns the cycle done from the starting point x between 0 and 1 by doubling its value in modulo 1.")
  (:method ((x number) &key (r *cycle-default-value*))
    (res-l (l-d x) r))
  (:method ((x t) &key r)
    (declare (ignore x r)) nil))

(defgeneric collatz-conjecture (x &key r)
  (:documentation "Returns the conjectural cycle (4 2 1) according to the initial value x such as:
if x is even then divide x by 2,
if x is odd then multiply x by 3 and add 1.")
  (:method ((x integer) &key (r *cycle-default-value*))
    (let ((tmp (cond ((= 1 x) (list (list 1) (list 4 2 1)))
		     ((= 2 x) (list (list 2 1) (list 4 2 1)))
		     ((= 4 x) (list (list 4 2 1)))
		     (t (a-collatz x)))))
      (res-l tmp r)))
  (:method ((x t) &key r)
    (declare (ignore x r)) nil))

(defgeneric interlace-cycle (&rest motifs)
  (:documentation "Depending on the length of each list, a cycle is generated according to the least common multiple. So, each list will be repeated
lcm(Union^n_i=1 |l_i|)/|l_i| times to complete the motivational cycle of interlacing.
The function returns two values. The first value is the interlace cycle, the second value is the number of repetition of each respective list as arguments.")
  (:method (&rest motifs)
    (when (loop for i in motifs always (listp i))
      (let* ((lst (remove nil motifs))
	     (len (mapcar #'length lst))
	     (n (apply #'lcm len)))
	(values
	 (apply #'mapcar #'list (loop for e in lst collect (loop repeat (/ n (length e)) append e)))
	 (mapcar #'(lambda (x) (/ n x)) len))))))
  
(defgeneric euclidean-rhythm (n m &key ratio)
  (:documentation "Euclidean rhythms allow spacing out <n> events (or onsets) across <m> positions (or pulses or beats) as evenly possible, according to the Bjorklund’s algorithm.")
  (:method ((n integer) (m integer) &key ratio)
    (let ((tmp (rtm-euclidean m n)))
      (when tmp
	(if ratio
	    (let (r s) (loop for i in tmp if (= 1 i) do (push (length s) r) (setf s (list i)) else do (push i s)) (reverse (butlast (push (length s) r))))
	    tmp))))
  (:method ((n t) (m t) &key ratio)
    (declare (ignore n m ratio)) nil))

(defgeneric campanology-permutation (lst mode)
  (:documentation "Application of the 'hunting group' algorithm according to two mode as 0 for the first position and 1 for the second position.")
  (:method ((lst list) (mode integer)) (append (campanology lst mode) (cdr (butlast (campanology (reverse lst) (case mode (0 (if (oddp (length lst)) 1 0)) (1 (if (evenp (length lst)) 1 0))))))))
  (:method ((in integer) (mode integer)) (mapcar #'(lambda (x) (n->10 x 10)) (campanology-permutation (10->n in 10) mode)))
  (:method ((lst t) (mode t)) (declare (ignore lst mode)) nil))

;;;-----------------------------------------------------------------

(defgeneric 10->n (x n)
  (:documentation "Convert an integer to a list of digits in base n.")
  (:method ((x integer) (n integer))
    (a-10->n x n)))

(defgeneric n->10 (lst n)
  (:documentation "Convert a list of digits in base n to an integer in base 10.")
  (:method ((lst list) (n integer))
    (a-n->10 lst n)))

;;;-----------------------------------------------------------------

(defgeneric mk-integer-lst (lst)
  (:documentation "Allows to built a list of integers from any kind of list.")
  (:method ((lst list)) 
    (a-mk-integer lst)))

(defgeneric fill-digit-lst (lst &optional n)
  (:documentation "Allows to add zero to match a given length of a list of any base.
In the case of a list of lists, the length is the maximum of the list or n.")
  (:method ((lst list) &optional (n nil))
    (a-fill-lst lst n)))

;;;------------------------------ END ------------------------------
