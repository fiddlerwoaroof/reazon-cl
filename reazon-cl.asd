;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :reazon-cl
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on ()
  :serial t
  :in-order-to ((test-op (test-op :reazon-cl/test)))
  :components ((:module "src"
                :components ((:file "reazon")))))

(defsystem :reazon-cl/test
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (:parachute)
  :serial t
  :perform (test-op (o c) (symbol-call :parachute :test :reazon.test))
  :components ((:module "src"
                :components ((:file "test")))))
