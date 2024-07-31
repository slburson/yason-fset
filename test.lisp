(defpackage :yason-test
  (:use :cl :unit-test))

(in-package :yason-test)

(defparameter *basic-test-json-string* "[{\"foo\":1,\"bar\":[7,8,9]},2,3,4,[5,6,7],true,null]")
(defparameter *basic-test-json-string-indented* "
[
  {\"foo\":1,
   \"bar\":[7,8,9]
  },
  2, 3, 4, [5, 6, 7], true, null
]")
(defparameter *basic-test-json-dom* (fset:seq (fset:map (("foo" 1) ("bar" (fset:seq 7 8 9))))
                                              2 3 4
                                              (fset:seq 5 6 7)
                                              t nil))


(deftest :yason "parser.basic"
  (let ((result (yason-fset:parse *basic-test-json-string*)))
    (test-equal *basic-test-json-dom* result :test #'fset:equal?)))

(deftest :yason "parser.basic-with-whitespace"
  (let ((result (yason-fset:parse *basic-test-json-string-indented*)))
    (test-equal *basic-test-json-dom* result :test #'fset:equal?)))

(deftest :yason "dom-encoder.basic"
  (let ((result (yason-fset:parse
                 (with-output-to-string (s)
                   (yason-fset:encode *basic-test-json-dom* s)))))
    (test-equal *basic-test-json-dom* result :test #'fset:equal?)))

(deftest :yason "dom-encoder.w-o-t-s*"
      (let* ((stg (yason-fset:with-output-to-string* (:indent 2)
                    (yason-fset:encode *basic-test-json-dom*)))
             (result (yason-fset:parse stg)))
    (test-equal (subseq stg 2 5) "  {")
    (test-equal *basic-test-json-dom* result :test #'fset:equal?)))

(deftest :yason "dom-encoder.w-o"
  (let* ((stg (with-output-to-string (s)
                    (yason-fset:with-output (s :indent 3)
                      (yason-fset:encode *basic-test-json-dom*))))
         (result (yason-fset:parse stg)))
    (test-equal (subseq stg 2 6) "   {")
    (test-equal *basic-test-json-dom* result :test #'fset:equal?)))

(defun whitespace-char-p (char)
  (member char '(#\space #\tab #\return #\newline #\linefeed)))

(deftest :yason "dom-encoder.indentation"
  (test-equal "[
          1,
          2,
          3
]"
              (with-output-to-string (s)
                (yason-fset:encode '(1 2 3) (yason-fset:make-json-output-stream s :indent 10))))
  (dolist (indentation-arg '(nil t 2 20))
    (test-equal "[1,2,3]" (remove-if #'whitespace-char-p
                                     (with-output-to-string (s)
                                       (yason-fset:encode '(1 2 3)
                                                    (yason-fset:make-json-output-stream s :indent indentation-arg)))))))

(deftest :yason "dom-encoder.object-indentation"
  (test-equal "{
  \"foo\": [
    1
  ],
  \"bar\": [
    2,
    3
  ]
}"
              (with-output-to-string (s)
                (yason-fset:encode-alist
                 '(("foo" 1) ("bar" 2 3))
                 (yason-fset:make-json-output-stream s :indent 2)))))

(deftest :yason "dom-encoder.empty-array-and-object-indentation"
  (test-equal "[
  [],
  {}
]"
              (with-output-to-string (s)
                (yason-fset:encode
                 (list (vector) (make-hash-table))
                 (yason-fset:make-json-output-stream s :indent 2)))))

(deftest :yason "stream-encoder.basic-array"
  (test-equal "[0,1,2]"
              (with-output-to-string (s)
                (yason-fset:with-output (s)
                  (yason-fset:with-array ()
                    (dotimes (i 3)
                      (yason-fset:encode-array-element i)))))))

(deftest :yason "stream-encoder.basic-object"
  (test-equal "{\"hello\":\"hu hu\",\"harr\":[0,1,2]}"
              (with-output-to-string (s)
                (yason-fset:with-output (s)
                  (yason-fset:with-object ()
                    (yason-fset:encode-object-element "hello" "hu hu")
                    (yason-fset:with-object-element ("harr")
                      (yason-fset:with-array ()
                        (dotimes (i 3)
                          (yason-fset:encode-array-element i)))))))))

;; See "surrogate" test below for CMUCL.
(deftest :yason "stream-encode.unicode-string"
  (test-equal "\"ab\\u0002 cde \\uD834\\uDD1E\""
              (with-output-to-string (s)
		(yason-fset:encode
		 #-cmucl
		 (format nil "ab~C cde ~C" (code-char #x02) (code-char #x1d11e))
		 #+cmucl
		 ;; Cmucl strings are utf-16 so we need to use
		 ;; surrogate pairs to represent codepoints outside the
		 ;; BMP.
                 (format nil "ab~C cde ~{~C~}"
			 (code-char #x02)
			 (multiple-value-list (lisp:surrogates #x1d11e)))
		 s))))

(defstruct user name age password)

(defmethod yason-fset:encode ((user user) &optional (stream *standard-output*))
           (yason-fset:with-output (stream)
             (yason-fset:with-object ()
               (yason-fset:encode-object-element "name" (user-name user))
               (yason-fset:encode-object-element "age" (user-age user)))))

(deftest :yason "stream-encoder.application-struct"
  (test-equal "[{\"name\":\"horst\",\"age\":27},{\"name\":\"uschi\",\"age\":28}]"
              (with-output-to-string (s)
                (yason-fset:encode (list (make-user :name "horst" :age 27 :password "puppy")
                                   (make-user :name "uschi" :age 28 :password "kitten"))
                             s))))

(deftest :yason "recursive-alist-encode"
  (test-equal "{\"a\":3,\"b\":[1,2,{\"c\":4,\"d\":[6]}]}"
              (yason-fset:with-output-to-string* (:stream-symbol s)
                (let ((yason-fset:*list-encoder* #'yason-fset:encode-alist))
                  (yason-fset:encode
                    `(("a" . 3) ("b" . #(1 2 (("c" . 4) ("d" . #(6))))))
                    s)))))

(deftest :yason "symbols-as-keys"
  (test-condition
    (yason-fset:with-output-to-string* (:stream-symbol s)
      (let ((yason-fset:*symbol-key-encoder* #'yason-fset:encode-symbol-as-lowercase))
        (yason-fset:encode-alist
          `((:|abC| . 3))
          s)))
    'error)
  (test-equal "{\"a\":3}"
              (yason-fset:with-output-to-string* (:stream-symbol s)
                (let ((yason-fset:*symbol-key-encoder* #'yason-fset:encode-symbol-as-lowercase))
                  (yason-fset:encode-alist
                    `((:a . 3))
                    s)))))

(deftest :yason "symbols-as-ht-keys"
  (test-equal "{\"bar\":2}"
              (let* ((yason-fset:*symbol-key-encoder* #'yason-fset:encode-symbol-as-lowercase))
                (with-output-to-string (*standard-output*)
                  (yason-fset:with-output (*standard-output*) 
                    (yason-fset:encode (alexandria:plist-hash-table `(:bar 2))))))))

(deftest :yason "ENCODE-as-obj-value"
  (test-equal "{\"foo\":1}"
              (with-output-to-string (*standard-output*)
                (yason-fset:with-output (*standard-output*) 
                  (yason-fset:with-object ()
                    (yason-fset:with-object-element ("foo")
                      (yason-fset:encode 1)))))))

(deftest :yason "ENCODE-as-obj-value-2"
  (test-equal "{\"baz\":12,\"foo\":{\"bar\":1}}"
              (with-output-to-string (*standard-output*)
                (yason-fset:with-output (*standard-output*) 
                  (yason-fset:with-object ()
                    (yason-fset:with-object-element ("baz")
                      (yason-fset:encode 12))
                    (yason-fset:with-object-element ("foo")
                      (yason-fset:with-object ()
                        (yason-fset:with-object-element ("bar")
                          (yason-fset:encode 1)))))))))

(deftest :yason "object-in-array"
  (test-equal "[5,[6,{\"far\":8,\"near\":9,\"there\":1},7],{\"foo\":\"bar\",\"bar\":\"baz\"},7]"
              (with-output-to-string (*standard-output*)
                (yason-fset:with-output (*standard-output* :indent nil)
                  (yason-fset:with-array ()
                    (yason-fset:encode-array-element 5)
                    (yason-fset:with-array ()
                      (yason-fset:encode-array-element 6)
                      (yason-fset:with-object ()
                        (yason-fset:with-object-element ("far")
                          (yason-fset:encode 8))
                        (yason-fset:encode-object-elements 
                          "near" 9
                          "there" 1))
                      (yason-fset:encode-array-element 7))
                    (yason-fset:with-object ()
                      (yason-fset:encode-object-element "foo" "bar")
                      (yason-fset:encode-object-element "bar" "baz"))
                    (yason-fset:encode-array-element 7))))))

(deftest :yason "output-to-string-with-indentation"
  (test-equal "[
  1,
  2,
  3
]"
              (yason-fset:with-output-to-string* (:indent 2 :stream-symbol s)
                (yason-fset:encode #(1 2 3)))))

(deftest :yason "parse-double-float"
  (test-equal 1579806040.0d0
              (yason-fset:parse "1579806040.0")))

(deftest :yason "parse-ordering-hash"
  (let ((parsed-map (yason-fset:parse "{\"foo\":0,\"bar\":1,\"foO\":2}")))
    (test-equal 0
                (lookup parsed-map "foo"))
    (test-equal 2
                (lookup parsed-map "foO"))
    (test-equal 1
                (lookup parsed-map "bar"))))


(deftest :yason "duplicate-key"
  (test-condition (yason-fset:parse "{\"a\":1,\"a\":2}")
                  'yason-fset::duplicate-key)
  (test-condition (yason-fset:parse "{\"a\":1,\"a\\ud800\":2}")
                  'error))

(deftest :yason "surrogate"
  ;; Cmucl uses utf-16 strings, so the result has the surrogate pair
  ;; in the parsed string.
  (test-equal #-cmucl
	      (list (char-code #\a) #x1d11e (char-code #\b))
	      #+cmucl
	      (list (char-code #\a) #xd834 #xdd1e (char-code #\b))
	      (map 'list #'char-code (yason-fset:parse "\"a\\ud834\\udd1eb\""))))

(deftest :yason "booleans-as-symbols"
  (let ((yason-fset:*parse-json-booleans-as-symbols* t))
    (test-equal (yason-fset:parse "true") 'yason-fset:true :test #'eq)
    (test-equal (yason-fset:parse "false") 'yason-fset:false :test #'eq)
    (test-equal (yason-fset:parse "true") yason-fset:true :test #'eq)
    (test-equal (yason-fset:parse "false") yason-fset:false :test #'eq)
    (let ((yason-fset:true :true)
	  (yason-fset:false :false))
      (test-equal (yason-fset:parse "true") :true :test #'eq)
      (test-equal (yason-fset:parse "false") :false :test #'eq)
      (test-equal (yason-fset:parse "true") yason-fset:true :test #'eq)
      (test-equal (yason-fset:parse "false") yason-fset:false :test #'eq)))
  (labels ((encode-to-string (data)
	     (with-output-to-string (stream)
	       (yason-fset:encode data stream))))
    (test-equal (encode-to-string 'yason-fset:true) "true")
    (test-equal (encode-to-string 'yason-fset:false) "false")
    (test-equal (encode-to-string yason-fset:true) "true")
    (test-equal (encode-to-string yason-fset:false) "false")
    (let ((yason-fset:true :true)
	  (yason-fset:false :false))
      (test-equal (encode-to-string :true) "true")
      (test-equal (encode-to-string :false) "false")
      (test-equal (encode-to-string yason-fset:true) "true")
      (test-equal (encode-to-string yason-fset:false) "false")
      ;; The symbols ‘true’ and ‘false’ always encode to true and
      ;; false, no matter how they are bound.
      (test-equal (encode-to-string 'yason-fset:true) "true")
      (test-equal (encode-to-string 'yason-fset:false) "false"))))
