(defpackage :reazon.test
  (:use :cl)
  (:export ))
(in-package :reazon.test)

(defmacro rt-should-equal (expected &body forms)
  "Assert that each form in FORMS equals EXPECTED."
  `(progn
     ,@(loop for form in forms
             collect `(parachute:is equal ,expected ,form))))

(reazon:defrel rt--alwayso ()
    "Infinite successful goals."
  (reazon:disj
    #'reazon:!S
    (rt--alwayso)))

(reazon:defrel rt--nevero ()
    "Infinite unsuccessful goals."
  (reazon:disj
    #'reazon:!U
    (rt--nevero)))

(parachute:define-test reazon)

(defmacro ert-deftest (name arg &body body)
  `(parachute:define-test ,name
     :parent reazon
     :time-limit 0.5
     ,arg
     ,@body))

(parachute:define-test rt-interface-run
  :parent reazon
  ()
  (rt-should-equal '()
    (reazon:run* q
      #'reazon:!U))
  (rt-should-equal '(t)
    (reazon:run* q
      (reazon:== t q)))
  (rt-should-equal '()
    (reazon:run* q
      #'reazon:!U
      (reazon:== t q)))
  (rt-should-equal '(t)
    (reazon:run* q
      #'reazon:!S
      (reazon:== t q)))
  (rt-should-equal '(corn)
    (reazon:run* r
      #'reazon:!S
      (reazon:== 'corn r)))
  (rt-should-equal '(olive oil)
    (reazon:run* x
      (reazon:disj
        (reazon:== 'olive x)
        (reazon:== 'oil x))))
  (rt-should-equal '(oil olive)
    (reazon:run* x
      (reazon:disj
        (reazon:== 'oil x)
        (reazon:== 'olive x))))
  (rt-should-equal '(oil)
    (reazon:run* x
      (reazon:disj
        (reazon:conj-2
         (reazon:== 'olive x)
         #'reazon:!U)
        (reazon:== 'oil x))))
  (rt-should-equal '(olive reazon.reify::|_0|
                     oil)
    (reazon:run* x
      (reazon:disj
        (reazon:conj
          (reazon:== 'virgin x)
          #'reazon:!U)
        (reazon:disj
          (reazon:== 'olive x)
          (reazon:disj
            #'reazon:!S
            (reazon:== 'oil x))))))
  (rt-should-equal '(done)
    (reazon:run 1 q
      (reazon:disj
        (reazon:== q 'done)
        (rt--nevero)))))

(parachute:define-test rt-interface-circular-query
  :parent reazon
  ()
  (parachute:fail (reazon:run* q
                    (reazon:== q `(,q)))
      'reazon:circular-query)
  (parachute:fail (reazon:run 10 q
                    (reazon:== q `(,q)))
      'reazon:circular-query))

(parachute:define-test rt-interface-fresh
  :parent reazon
  ()
  (rt-should-equal '(t)
    (reazon:run* q
      (reazon:fresh (x)
        (reazon:== t x)
        (reazon:== t q))))
  (rt-should-equal '((reazon.reify::|_0|
                      reazon.reify::|_1|))
    (reazon:run* s
      (reazon:fresh (x)
        (reazon:fresh (y)
          (reazon:== `(,x ,y) s)))))
  (rt-should-equal '((reazon.reify::|_0|
                      reazon.reify::|_1|
                      reazon.reify::|_0|))
    (reazon:run* s
      (reazon:fresh (x y)
        (reazon:== `(,x ,y ,x) s))))
  (rt-should-equal '((split pea))
    (reazon:run* r
      (reazon:fresh (x)
        (reazon:fresh (y)
          (reazon:== 'split x)
          (reazon:== 'pea y)
          (reazon:== `(,x ,y) r))))
    (reazon:run* r
      (reazon:fresh (x)
        (reazon:fresh (y)
          (reazon:== 'split x)
          (reazon:== 'pea y)
          (reazon:== `(,x ,y) r))))
    (reazon:run* r
      (reazon:fresh (x y)
        (reazon:== 'split x)
        (reazon:== 'pea y)
        (reazon:== `(,x ,y) r)))
    (reazon:run* (x y)
      (reazon:== 'split x)
      (reazon:== 'pea y)))
  (rt-should-equal '(((split pea) split pea))
    (reazon:run* (r x y)
      (reazon:== 'split x)
      (reazon:== 'pea y)
      (reazon:== `(,x ,y) r)))
  (rt-should-equal '((reazon.reify::_0
                      reazon.reify::_1)
                     (reazon.reify::_0
                      reazon.reify::_1))
    (reazon:run* (x y)
      (reazon:fresh (z)
        (reazon:conde
          ((reazon:== x z) (reazon:fresh (z) (reazon:== y z)))
          ((reazon:fresh (z) (reazon:== x z)) (reazon:== y z))))))
  (rt-should-equal '((nil reazon.reify::_0)
                     (reazon.reify::_0 nil))
    (reazon:run* (x y)
      (reazon:fresh (z)
        (reazon:conde
          ((reazon:== x z) (reazon:fresh (z) (reazon:== y z)))
          ((reazon:fresh (z) (reazon:== x z)) (reazon:== y z)))
        (reazon:== nil z)))))

(parachute:define-test rt-interface-project
  :parent reazon
  ()
  (rt-should-equal '(25)
    (reazon:run* q
      (reazon:fresh (x)
        (reazon:== x 5)
        (reazon:project (x)
          (reazon:== q (* x x))))))
  (parachute:fail
      (reazon:run* q
        (reazon:fresh (x)
          (reazon:project (x)
            (reazon:== q (* x x)))
          (reazon:== x 5)))))


#+(or)
(ert-deftest rt-interface-timeout ()
  (rt-should-equal '()
    (let ((reazon:timeout 0.01))
      (reazon:run* q (rt--nevero))))
  ;; This test might fail if your computer is REALLY slow.
  (should
   (>
    (length
     (let ((reazon:timeout 0.01))
       (reazon:run* q
         (rt--alwayso))))
    3)))

(reazon:defrel rt-teacupo (x)
    "X is tea or cup."
  (reazon:disj
    (reazon:== x 'tea)
    (reazon:== x 'cup)))

(ert-deftest rt-interface-defrel ()
  (rt-should-equal '(tea cup)
    (reazon:run* x (rt-teacupo x)))
  (rt-should-equal "X is tea or cup."
    (documentation #'rt-teacupo 'function))
  (rt-should-equal '((nil t) (tea t) (cup t))
    (reazon:run* (x y)
      (reazon:conde
        ((rt-teacupo x) (reazon:== y t))
        ((reazon:== x nil) (reazon:== y t)))))
  (rt-should-equal '((tea tea) (tea cup) (cup tea) (cup cup))
    (reazon:run* (x y)
      (rt-teacupo x)
      (rt-teacupo y)))
  (rt-should-equal '(tea cup)
    (reazon:run* x
      (rt-teacupo x)
      (rt-teacupo x)))
  (rt-should-equal '((nil tea) (nil cup)
                     (tea reazon.reify::_0)
                     (cup reazon.reify::_0))
    (reazon:run* (x y)
      (reazon:disj-2
       (reazon:conj-2
        (rt-teacupo x)
        (rt-teacupo x))
       (reazon:conj-2
        (reazon:== nil x)
        (rt-teacupo y)))))
  (rt-should-equal '((t tea) (t cup)
                     (tea reazon.reify::_0)
                     (cup reazon.reify::_0))
    (reazon:run* (x y)
      (reazon:conde
        ((rt-teacupo x) (rt-teacupo x))
        ((reazon:== x t) (rt-teacupo y))))))

(reazon:defrel rt-empty-relo (_x))
(reazon:defrel rt-empty-relo-with-doc (_x) "docstring")

(ert-deftest rt-interface-empty-relation ()
  (rt-should-equal '(reazon.reify::|_0|)
    (reazon:run* x (rt-empty-relo-with-doc x)))
  (rt-should-equal '(reazon.reify::|_0|)
    (reazon:run* x (rt-empty-relo x))))

(ert-deftest rt-interface-empty-conj-disj ()
  (rt-should-equal '(reazon.reify::|_0|)
    (reazon:run* q)
    (reazon:run* q (reazon:conj)))
  (rt-should-equal '()
    (reazon:run* q (reazon:disj))))

(ert-deftest rt-conde ()
  (rt-should-equal '()
    (reazon:run* q (reazon:conde)))
  (rt-should-equal '((split pea) (navy bean) (red lentil))
    (reazon:run* (x y)
      (reazon:conde
        ((reazon:== x 'split) (reazon:== y 'pea))
        ((reazon:== x 'navy) (reazon:== y 'bean))
        ((reazon:== x 'red) (reazon:== y 'lentil)))))
  (rt-should-equal '(oil)
    (reazon:run* x
      (reazon:conde
        ((reazon:== x 'olive) #'reazon:!U)
        ((reazon:== x 'oil))))))

(ert-deftest rt-interface-conda ()
  (rt-should-equal '()
    (reazon:run* q
      (reazon:conda
       (#'reazon:!U #'reazon:!S)
       (#'reazon:!U)))
    (reazon:run* q
      (reazon:conda
       (#'reazon:!S #'reazon:!U)
       (#'reazon:!S)))
    (reazon:run* x
      (reazon:conda
       ((reazon:== 'virgin x) #'reazon:!U)
       ((reazon:== 'olive x) #'reazon:!S)
       ((reazon:== 'oil x))))
    (reazon:run* q
      (reazon:fresh (x y)
        (reazon:== 'split x)
        (reazon:== 'pea y)
        (reazon:conda
         ((reazon:== 'split x) (reazon:== x y))
         (#'reazon:!S)))))
  (rt-should-equal '(reazon.reify::_0)
    (reazon:run* q
      (reazon:conda
       (#'reazon:!U #'reazon:!S)
       (#'reazon:!S)))
    (reazon:run* q
      (reazon:conda
       (#'reazon:!S #'reazon:!S)
       (#'reazon:!U)))
    (reazon:run* q
      (reazon:fresh (x y)
        (reazon:== 'split x)
        (reazon:== 'pea y)
        (reazon:conda
         ((reazon:== x y) (reazon:== 'split x))
         (#'reazon:!S)))))
  (rt-should-equal '(olive)
    (reazon:run* x
      (reazon:conda
       ((reazon:== 'olive x) #'reazon:!S)
       ((reazon:== 'oil x))))))

(reazon:defrel rt-not-pasta (x)
    "X is not pasta."
  (reazon:conda
   ((reazon:== 'pasta x) #'reazon:!U)
   (#'reazon:!S)))

(ert-deftest rt-interface-not-pasta ()
  (rt-should-equal '(spaghetti)
                   (reazon:run* x
                     (reazon:conda
                      ((rt-not-pasta x) #'reazon:!U)
                      ((reazon:== 'spaghetti x) #'reazon:!S)
                      (#'reazon:!U))))
  (rt-should-equal '()
                   (reazon:run* x
                     (reazon:== 'spaghetti x)
                     (reazon:conda
                      ((rt-not-pasta x) #'reazon:!U)
                      ((reazon:== 'spaghetti x) #'reazon:!S)
                      (#'reazon:!U)))))

(reazon:defrel rt-onceo (goal)
    "???"
  (reazon:condu
   (goal #'reazon:!S)
   (#'reazon:!U)))

(ert-deftest rt-interface-condu ()
  (rt-should-equal '(tea)
                   (reazon:run* x
                     (rt-onceo
                      (rt-teacupo x)))))

(ert-deftest rt-interface-condeau ()
  (rt-should-equal '(5 tea cup)
    (reazon:run* q
      (reazon:conde
        ((rt-teacupo q) (reazon:== 0 0))
        ((reazon:== q 5) (reazon:== 0 0)))))
  (rt-should-equal '(tea cup)
    (reazon:run* q
      (reazon:conda
       ((rt-teacupo q) (reazon:== 0 0))
       ((reazon:== q 5)))))
  (rt-should-equal '(tea)
    (reazon:run* q
      (reazon:condu
       ((rt-teacupo q) (reazon:== 0 0))
       ((reazon:== q 5)))))
  (rt-should-equal '()
    (reazon:run* q (reazon:conde))
    (reazon:run* q (reazon:conda))
    (reazon:run* q (reazon:condu))))

#+(or)
(parachute:define-test rt-interface-conda-bug
  :parent reazon
  :time-limit 0.5
  ()
  (rt-should-equal '(a1)
    (reazon:run* x
      (reazon:conde
        ((reazon:== x 'a1))
        ((reazon:== x 'a2)))
      (reazon:conda ((reazon:== x 'a2) #'reazon:!u) (#'reazon:!s)))
    (reazon:run* (x)
      (reazon:conde
        ((reazon:== x 'a1))
        ((reazon:== x 'a2)))
      (reazon:conda ((reazon:== x 'a2) (reazon:== x 'c)) ((reazon:== x x))))))
