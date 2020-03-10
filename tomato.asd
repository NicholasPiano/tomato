
(asdf:defsystem :tomato
  :description ""
  :author ""
  :license ""
  :version ""
  :depends-on (:filter)
  :serial t
  :components
    ((:file "packages")
     (:module "src"
      :serial t
      :components
        ((:file "suite")
         (:file "test")
         (:file "before-each")
         (:file "mock")
         (:file "expect")
         (:file "it")))))
