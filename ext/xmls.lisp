(cl:in-package :cl-user)
(defpackage :reazon.ext.xmls
  (:use)
  (:import-from :reazon
                #:defrel #:caro #:cdro #:membero #:== #:fresh #:conj
                #:disj)
  (:export
   #:tago
   #:attro
   #:childreno
   #:childo
   #:dom-searcho))
(in-package :reazon.ext.xmls)

(defrel tago (tag dom)
  (caro dom tag))

(defrel attro (attr value dom)
  (fresh (attrs attr-pair dom-cdr)
    (cdro dom dom-cdr)
    (caro dom-cdr attrs)
    (membero attr-pair attrs)
    (== (cl:list attr value)
        attr-pair)))

(defrel childreno (children dom)
  (fresh (dom-cdr)
    (cdro dom dom-cdr)
    (cdro dom-cdr children)))

(defrel childo (child dom)
  (fresh (children)
    (childreno children dom)
    (membero child children)))

(defrel dom-searcho (child dom)
  (disj
    (== child dom)
    (fresh (int)
      (conj
        (childo int dom)
        (dom-searcho child int)))))
