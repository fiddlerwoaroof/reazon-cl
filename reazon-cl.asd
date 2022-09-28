;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :reazon-cl
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (:trivia)
  :serial t
  :in-order-to ((test-op (test-op :reazon-cl/test)))
  :components ((:module "src"
                :components ((:file "reazon")))))

(defsystem :reazon-cl/test
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (:reazon-cl
               :parachute)
  :serial t
  :perform (test-op (o c)
                    (when (eql :failed
                               (symbol-call :parachute :status
                                            (symbol-call :parachute :test :reazon.test)))
                      (error "tests failed")))
  :components ((:module "src"
                :components ((:file "test")))))
