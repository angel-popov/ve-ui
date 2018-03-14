;;;; ve-ui.asd

(asdf:defsystem #:ve-ui
  :description "Describe ve-ui here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:mcst
               #:cl-selenium
               #:alexandria
               #:prove)
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "ve-ui")))

