(defsystem "cl-ctm"
  :version "0.1.0"
  :author "doyougnu"
  :license ""
  :depends-on ("arrows" "closer-mop")
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "1")
                 (:file "math")
                 (:file "type-system"))))
  :description ""
  :in-order-to ((test-op (test-op "./tests"))))
