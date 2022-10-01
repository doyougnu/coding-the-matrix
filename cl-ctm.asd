(defsystem "cl-ctm"
  :version "0.1.0"
  :author "doyougnu"
  :license ""
  :depends-on ("alexandria" "arrows" "closer-mop" "vgplot" "eazy-gnuplot")
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "1")
                 (:file "4")
                 (:file "5")
                 (:file "math")
                 (:file "type-system"))))
  :description ""
  :in-order-to ((test-op (test-op "./tests"))))
