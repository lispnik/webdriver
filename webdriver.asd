(defsystem #:webdriver
  :components ((:file "package")
               (:file "webdriver"))
  :depends-on (#:dexador
               #:json-mop
               #:yason
               #:quri
               #:alexandria))
