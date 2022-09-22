(defpackage :reazon
  (:use :cl )
  (:shadow equal)
  (:export #:!S #:!U #:== #:adjacento #:appendo #:assqo #:caro #:cdro
           #:circular-query #:circular-query-expr #:conda #:conde #:condu
           #:conj #:conj-2 #:conso #:defrel #:disj #:disj-2 #:fresh
           #:immediately-precedeso #:listo #:membero #:nullo #:pairo
           #:precedeso #:project #:run #:run* #:set-equalo #:subseto))
(defpackage :reazon.reify
  (:use)
  (:export))
(in-package :reazon)

(declaim (inline equal))
(defun equal (a b)
  (if (vectorp a)
      (and (vectorp b)
           (= (length a)
              (length b))
           (every 'cl:equal a b))
      (cl:equal a b)))

(defvar *false*
  '#:false)

(defun make-variable (name)
  (vector name))

(defun variable-p (var)
  (and (not (stringp var))
       (vectorp var)))

(define-condition circular-query (error)
  ((%expr :reader circular-query-expr :initarg :expr)))

(defun walk (var sub)
  (let* ((next var)
         (val (and (variable-p next)
                   (assoc next sub))))
    (loop while (consp val)
          do
             (setq next (cdr val))
             (setq val (and (variable-p next)
                            (assoc next sub))))
    next))

(defun occurs-p (var val sub)
  (let ((walked (walk val sub)))
    (cond
      ((variable-p walked)
       (equal walked var))
      ((consp walked)
       (or (occurs-p var (car walked) sub)
           (occurs-p var (cdr walked) sub)))
      (t nil))))

(defvar *occurs-check* t)

(defun extend (var val sub)
  "Associate VAR and VAL in SUB."
  (if (and *occurs-check* (occurs-p var val sub))
      (error 'circular-query :expr `(,var ,val ,sub))
      (cons `(,var . ,val) sub)))

(defun unify (u v sub)
  (let ((u-walked (walk u sub))
        (v-walked (walk v sub)))
    (cond
      ((equal u-walked v-walked)
       sub)
      ((variable-p u-walked)
       (extend u-walked v-walked sub))
      ((variable-p v-walked)
       (extend v-walked u-walked sub))
      ((and (consp u-walked)
            (consp v-walked))
       (let ((sub (unify (car u-walked)
                         (car v-walked)
                         sub)))
         (if (equal sub *false*)
             *false*
             (unify (cdr u-walked)
                    (cdr v-walked)
                    sub))))
      (t
       *false*))))

(defun == (u v)
  (lambda (sub)
    (let ((unified (unify u v sub)))
      (if (equal unified *false*)
          '()
          `(,unified)))))

(defun r-append (stream-1 stream-2)
  (let (result
        (rest stream-1))
    (loop while (and rest
                     (not (functionp rest)))
          do (setf result (cons (car rest)
                                result)
                   rest (cdr rest)))
    (append (nreverse result)
            (if (null rest)
                stream-2
                (lambda ()
                  (r-append stream-2
                            (funcall rest)))))))

(defun append-map (goal stream)
  (cond
    ((null stream) nil)
    ((functionp stream)
     (lambda ()
       (append-map goal
                   (funcall stream))))
    (t (r-append (funcall goal (car stream))
                 (append-map goal
                             (cdr stream))))))

(defun !U (_sub)
  "Return the empty stream.
This primitive goal always fails."
  (declare (ignore _sub))
  '())

(defun disj-2 (goal-1 goal-2)
  (lambda (stream)
    (r-append (funcall goal-1 stream)
              (funcall goal-2 stream))))

(defmacro disj (&body goals)
  (trivia:ematch goals
    (() `#'!U)
    ((list goal) goal)
    ((list* goal rest)
     `(disj-2 ,goal (disj ,@rest)))))

(defun conj-2 (goal-1 goal-2)
  (lambda (stream)
    (append-map
     goal-2
     (funcall goal-1 stream))))

(defun !S (sub)
  `(,sub))

(defmacro conj (&body goals)
  (trivia:ematch goals
    (() `#'!S)
    ((list goal) goal)
    ((list* goal rest)
     `(conj-2 ,goal (conj ,@rest)))))

(defun call-with-fresh (names function)
  (apply function
         (mapcar #'make-variable names)))

(defmacro fresh (vars &body goals)
  (if (null vars)
      `(conj ,@goals)
      `(call-with-fresh (mapcar (lambda (it)
                                  (gensym (string it)))
                                ',vars)
                        (lambda (,@vars)
                          (conj ,@goals)))))

(defun reify-name (number)
  (intern (format nil "_~d" number)
          :reazon.reify))

(defun reify-sub (var sub)
  (let ((walked (walk var sub)))
    (cond
      ((variable-p walked)
       (let ((name (reify-name (length sub))))
         (extend walked name sub)))
      ((consp walked)
       (let ((sub (reify-sub (car walked)
                             sub)))
         (reify-sub (cdr walked)
                    sub)))
      (t
       sub))))

(defun walk* (var sub)
  (let ((walked (walk var sub)))
    (if (not (consp walked))
        walked
        (cons (walk* (car walked)
                     sub)
              (walk* (cdr walked)
                     sub)))))

(defun reify (var)
  (lambda (sub)
    (let* ((walked-var (walk* var sub))
           (reified-sub (reify-sub walked-var '())))
      (walk* walked-var
             reified-sub))))

(defun pull (stream)
  (let ((result stream))
    (loop while (functionp result)
          do (setf result (funcall result)))
    result))

(defun run-goal (goal)
  (pull (funcall goal nil)))

(defun take (n stream)
  (cond
    ((or (functionp stream)
         (null stream))
     nil)
    ((equal 1 n)
     (list (car stream)))
    (t (let ((count (if n
                        (1- n)
                        -1))
             (result (list (car stream)))
             (rest (pull (cdr stream))))
         (loop while (and rest
                          (not (zerop count))
                          (not (functionp rest)))
               do
                  (setf count (1- count)
                        result (cons (car rest)
                                     result)
                        rest (pull (cdr rest))))
         (nreverse result)))))


(defmacro run (n var/list &body goals)
  (if (listp var/list)
      (let ((vars var/list)
            (q (gensym)))
        `(run ,n ,q
           (fresh ,vars
             (== (list ,@vars) ,q)
             ,@goals)))
      (let ((var var/list))
        `(let ((,var (make-variable ',var)))
           (mapcar (reify ,var)
                   (take ,n
                                 (run-goal (conj ,@goals))))))))

(defmacro run* (query-var &body goals)
  `(run nil ,query-var
     ,@goals))

(defmacro project (vars &body goals)
  (if (null vars)
      `(conj ,@goals)
      (let* ((stream (gensym))
             (walked-vars (mapcar (lambda (var)
                                    `(,var (walk* ,var ,stream)))
                                  vars)))
        `(lambda (,stream)
           (let ,walked-vars
             (funcall (conj ,@goals)
                      ,stream))))))

(defmacro defrel (name varlist &optional docstring &body goals)
  ;; keep this nasty docstring logic
  ;; away from the relation definition
  (cond
    ((stringp docstring) (setq docstring `(,docstring)))
    (docstring (setq goals `(,docstring . ,goals)
                     docstring nil)))

  (let ((stream (gensym)))
    `(defun ,name ,varlist
       ,@docstring
       (declare (ignorable ,@varlist))
       (lambda (,stream)
         (lambda ()
           (funcall (conj ,@goals) ,stream))))))

(defrel conso (a d p)
    "P is a cons of A and D"
  (== p (cons a d)))

(defrel caro (p a)
    "A is the car of P"
  (fresh (d)
    (conso a d p)))

(defrel cdro (p d)
    "D is the cdr of P"
  (fresh (a)
    (conso a d p)))

(defrel nullo (x)
    "X is null."
  (== x '()))

(defrel pairo (p)
    "P is a pair."
  (fresh (a d)
    (conso a d p)))

(defmacro conde (&body clauses)
  "Chain together CLAUSES as a disjunction of conjunctions."
  `(disj ,@(mapcar (lambda (arm) `(conj ,@arm)) clauses)))

(defrel listo (s)
    "S is a proper list."
  (conde
    ((nullo s))
    ((pairo s)
     (fresh (d)
       (cdro s d)
       (listo d)))))

(defrel appendo (l p out)
    "L appended to P produces OUT."
  (conde
    ((nullo l) (== p out))
    ((fresh (a d res)
       (conso a d l)
       (conso a res out)
       (appendo d p res)))))

(defrel membero (x s)
    "X exists in the list S."
  (fresh (a d)
    (== s `(,a . ,d))
    (disj
      (== a x)
      (membero x d))))

(defrel precedeso (x y s)
    "X is the list of elements preceding Y in the list S."
  (fresh (a d)
    (conso a d s)
    (conde
      ((== x a) (membero y d))
      ((precedeso x y d)))))

(defrel immediately-precedeso (x y s)
    "X is the element immediately preceding Y in the list S."
  (fresh (a d)
    (conso a d s)
    (conde
      ((== x a) (caro d y))
      ((immediately-precedeso x y d)))))

(defrel adjacento (x y s)
    "X is an adjacent element to Y in the list S."
  (disj
    (immediately-precedeso x y s)
    (immediately-precedeso y x s)))

(defrel subseto (subset set)
    "SUBSET is a subset of SET."
  (disj
    (== subset '())
    (fresh (a d)
      (== subset `(,a . ,d))
      (membero a set)
      (subseto d set))))

(defrel set-equalo (s1 s2)
    "S1 contains the same set of elements as S2."
  (subseto s1 s2)
  (subseto s2 s1))

(defun ifte-help (stream consq alt)
  "Run ALT with STREAM if it's nil, else CONSQ."
  (cond
    ((null stream) (funcall alt stream))
    ((functionp stream)
     (lambda () (ifte-help (funcall stream) consq alt)))
    (t (append-map consq stream))))

(defun ifte (test consq alt)
  "Run CONSQ if TEST succeeds, else ALT."
  (lambda (s)
    (ifte-help (funcall test s) consq alt)))

(defmacro conda (&rest clauses)
  "Run only the first clause in CLAUSES whose head succeeds.
Also known as committed choice. This operator is impure."
  (trivia:ematch clauses
    (() '#'!U)
    ((list clause) `(conj ,@clause))
    ((list* (list* head body) rest)
     `(ifte ,head
                    (conj ,@body)
                    (conda ,@rest)))))

(defun once-help (stream)
  "Pull at most one value out of STREAM."
  (cond
    ((null stream) '())
    ((functionp stream)
     (lambda () (once-help (funcall stream))))
    (t `(,(car stream)))))

(defun once (goal)
  "Run GOAL for just one value (if there is one)."
  (lambda (s)
    (once-help (funcall goal s))))

(defmacro condu (&rest clauses)
  "Run for just one value the first clause in CLAUSES whose head succeeds.
Also known as committed choice. This operator is impure."
  (trivia:ematch clauses
    ('() '#'!U)
    ((list* (list* head body) rest)
     `(conda
       ((once ,head) ,@body)
       ,@rest))))

(defrel assqo (x s out)
  "Retrieving X from the alist S produces OUT."
  (conde
   ((nullo s) (nullo out))
   ((fresh (key val rest)
      (== s `((,key . ,val) ,@rest))
      (conde
       ((== x key) (== out `(,key . ,val)))
       ((assqo x rest out)))))))
