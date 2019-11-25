(in-package #:webdriver)

;;; TODO https://www.w3.org/TR/webdriver/#dfn-proxy-configuration

(defclass proxy ()
  ((proxy-type
    :initarg :proxy-type
    :json-type :string
    :json-key "proxyType"))
  (:metaclass json-mop:json-serializable-class))

(defclass timeouts ()
  ((script
    :initarg :script
    :json-type :number
    :json-key "script")
   (page-load
    :initarg :page-load
    :json-type :number
    :json-key "pageLoad")
   (implicit
    :initarg :implicit
    :json-type :number
    :json-key "implicit"))
  (:metaclass json-mop:json-serializable-class))

(defclass capabilities ()
  ((browser-name
    :initarg :browser-name
    :json-type :string
    :json-key "browserName")
   (browser-version
    :initarg :browser-version
    :json-type :string
    :json-key "browserVersion")
   (platform-name
    :initarg :platform-name
    :json-type :string
    :json-key "platformName")
   (accept-insecure-certs-p
    :initarg :accept-insecure-certs-p
    :json-type :bool
    :json-key "acceptInsecureCerts")
   (page-load-strategy
    :initarg :page-load-strategy
    :json-type :string
    :json-key "pageLoadStrategy")
   (proxy
    :initarg :proxy
    :json-type proxy
    :json-key "proxy")
   (window-dimension/position-p
    :initarg :window-dimension/position-p
    :json-type :bool
    :json-key "setWindowRect")
   (timeouts
    :initarg :timeouts
    :json-type timeouts
    :json-key "timeouts")
   (strict-file-interactability-p
    :initarg :strict-file-interactability-p
    :json-type :bool
    :json-key "strictFileInteractability")
   (undefined-prompt-behavior-p
    :initarg :undefined-prompt-behavior-p
    :json-type :string
    :json-key "unhandledPromptBehavior"))
  (:metaclass json-mop:json-serializable-class))

(defclass session ()
  ((id :initarg :id
       :reader id
       :json-type :string
       :json-key "sessionId")
   (capabilities :initarg :capabilities
                 :json-type capabilities
                 :json-key "capabilities")
   (base-uri :reader base-uri))
  (:metaclass json-mop:json-serializable-class))

(defclass success-response ()
  ((value :initarg :value
          :json-key "value"
          :json-type :hash-table))
  (:metaclass json-mop:json-serializable-class))

(defclass error-response ()
  ((error :initarg :error
          :json-key "error"
          :json-type :hash-table)
   (message :initarg :message
            :json-key "message"
            :json-type :string)
   (stacktrace :initarg :stacktrace
               :json-key "stacktrace"
               :json-type :string)
   (data :initarg :data
         :json-key "data"
         :json-type :hash-table))
  (:metaclass json-mop:json-serializable-class))

(defun make-endpoint-uri (base-uri path-template &rest path-bindings)
  (quri:make-uri
   :defaults base-uri
   :path (format nil
                 "~A~A" (quri:uri-path base-uri)
                 (apply #'format nil path-template (mapcar #'quri:url-encode path-bindings)))))

(defun normalize-uri (uri)
  (let ((uri-path (quri:uri-path uri)))
    (if (ends-with #\/ uri-path)
        uri
        (quri:make-uri :defaults uri
                       :path (concatenate 'string uri-path "/")))))

(defun json-encode (serializable)
  (with-output-to-string (*standard-output*)
    (json-mop:encode serializable)))

;;; FIXME not much to this any more
(defun request (function &optional (deserializer #'identity))
  (multiple-value-bind
        (body)
      (funcall function)
    (funcall deserializer body)))

(defun make-json-mop-deserializer (class)
  (lambda (response)
    (let ((success-response (json-mop:json-to-clos response 'success-response)))
      (json-mop:json-to-clos (slot-value success-response 'value) class))))

(defun new-session (base-uri)
  (let* ((normalized-base-uri
           (normalize-uri base-uri))
         (session 
           (request
            (lambda ()
              (dex:post (make-endpoint-uri normalized-base-uri "session")
                        :content
                        (json-encode (make-instance 'session :capabilities (make-instance 'capabilities)))))
            (make-json-mop-deserializer 'session))))
    (setf (slot-value session 'base-uri)
          normalized-base-uri)
    session))

(defun delete-session (session)
  (request
   (lambda ()
     (dex:delete (make-endpoint-uri (base-uri session) "session/~A" (id session))))))

(defun status (base-uri))

(defun current-timeouts (session)
  (request
   (lambda ()
     (dex:get (make-endpoint-uri (base-uri session) "session/~A/timeouts" (id session))))
   'timeouts))

(defun (setf current-timeouts) (new-current-timeouts session)
  (request
   (lambda ()
     (dex:post (make-endpoint-uri (base-uri session) "session/~A/timeouts" (id session))
               :content (json-encode new-current-timeouts))))
  new-current-timeouts)

(defun url (session)
;;; FIXME url in the raw body response
  (request
   (lambda ()
     (dex:get (make-endpoint-uri (base-uri session) "session/~A/url" (id session))))))

(defun (setf url) (new-url session)
  (request
   (lambda ()
     (dex:post (make-endpoint-uri (base-uri session) "session/~A/url" (id session))
               :content
               (json-encode (plist-hash-table (list "url" new-url))))))
  new-url)

(defun back (session)
  (request
   (lambda ()
     (dex:post (make-endpoint-uri (base-uri session) "session/~A/back" (id session))
               :content "{}")))
  session)

(defun forward (session)
  (request
   (lambda ()
     (dex:post (make-endpoint-uri (base-uri session) "session/~A/forward" (id session))
               :content "{}")))
  session)

(defun refresh (session)
  (request
   (lambda ()
     (dex:post (make-endpoint-uri (base-uri session) "session/~A/refresh" (id session))
               :content "{}")))
  session)

;;; TODO

(deftype locator-strategy ()
  '(member
    :css
    :link-text
    :partial-link-text
    :tag-name
    :xpath))

(defvar *locator-strategy-name*
  (plist-hash-table
   (list :css "css selector"
         :link-text "link text"
         :paritial-link-text "paritial-link-text"
         :tag-name "tag name"
         :xpath "xpath")))

(defun deserialize-elements (response)
  (yason:parse (slot-value (json-mop:json-to-clos response 'success-response) 'value)))

(defun find-element (session locator-strategy using)
  (check-type locator-strategy locator-strategy)
  (let ((response 
          (dex:post (make-endpoint-uri (base-uri session) "session/~A/element" (id session))
                    :content (json-encode
                              (plist-hash-table
                               (list "using" (gethash locator-strategy *locator-strategy-name*)
                                     "value" using))))))
    (first (hash-table-values (gethash "value" (yason:parse response))))))

(defun find-elements (session locator-strategy using)
  (check-type locator-strategy locator-strategy)
  (let ((response 
          (dex:post (make-endpoint-uri (base-uri session) "session/~A/elements" (id session))
                    :content (json-encode
                              (plist-hash-table
                               (list "using" (gethash locator-strategy *locator-strategy-name*)
                                     "value" using))))))
    (loop for i in (gethash "value" (yason:parse response))
          collect (first (hash-table-values i)))))

;;; TODO

(defun element-text (session element)
  (let ((response (dex:get (make-endpoint-uri (base-uri session) "session/~A/element/~A/text" (id session) element))))
    response))

(defun element-tag-name (session element)
  (let ((response (dex:get (make-endpoint-uri (base-uri session) "session/~A/element/~A/name" (id session) element))))
    response))

(defclass rect ()
  ((x :initarg :x
      :json-type :number
      :json-key "x")
   (y :initarg :y
      :json-type :number
      :json-key "y")
   (width :initarg :width
          :json-type :number
          :json-key "width")
   (height :initarg :height
           :json-type :number
           :json-key "height"))
  (:metaclass json-mop:json-serializable-class))

(defmethod print-object ((instance rect) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~A,~A ~Ax~A"
            (slot-value instance 'x)
            (slot-value instance 'y)
            (slot-value instance 'width)
            (slot-value instance 'height))))

(defun element-rect (session element)
  (let* ((response (dex:get (make-endpoint-uri (base-uri session) "session/~A/element/~A/rect" (id session) element)))
         (success-response (json-mop:json-to-clos response 'success-response)))
    (json-mop:json-to-clos (slot-value success-response 'value) 'rect)))

;;; TODO

(defun element-click (session element)
  (dex:post (make-endpoint-uri (base-uri session) "session/~A/element/~A/click" (id session) element)
            :content "{}")
  session)

(defun send-keys (session element text)
  (dex:post (make-endpoint-uri (base-uri session) "session/~A/element/~A/value" (id session) element)
            :content (json-encode (plist-hash-table (list "text" text))))
  session)

#+nil
(let ((session (new-session (quri:uri "http://localhost:9515"))))
  (setf (url session) "http://google.com/")
  (refresh session)
  (back session)
  (forward session)
  (let ((element1 (find-element session :css "input[name=q]")))
    (send-keys session element1 "foobar
"))
  (mapcar (lambda (element)
            (list (element-text session element)
                  (element-rect session element)))
          (find-elements session :css "div.g"))
  (let ((element (find-element session :css "div.g")))
    (print (element-text session element))
    (element-click session element))
  )

