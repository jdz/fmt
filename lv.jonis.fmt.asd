(defsystem "lv.jonis.fmt"
  :description "Formatting utilities"
  :author "Jānis Džeriņš <lisp@jonis.lv>"
  :version "0.1"
  :license "zlib"
  :long-description "A package with utility functions to be used with tilde slash FORMAT operation."
  :pathname "src/"
  :components ((:file "package")
               (:file "fmt" :depends-on ("package")))
  :in-order-to ((test-op (test-op "lv.jonis.fmt/tests"))))

(defsystem "lv.jonis.fmt/tests"
  :depends-on ("fmt" "parachute")
  :pathname "tests/"
  :components ((:file "package")
               (:file "fmt" :depends-on ("package")))
  :perform (test-op (operation component)
                    (symbol-call '#:parachute '#:test "LV.JONIS.FMT.TESTS")))
