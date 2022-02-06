(defsystem "cl-ctm"
  :version "0.1.0"
  :author "doyougnu"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "1"))))
  :description ""
  :in-order-to ((test-op (test-op "./tests"))))

(defsystem "./tests"
  :author ""
  :license ""
  :depends-on ("."
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for ."
  :perform (test-op (op c) (symbol-call :rove :run c)))
