(require 'ert)

(require 'popup)

(defmacro popup-test-with-common-setup (&rest body)
  (declare (indent 0) (debug t))
  `(save-excursion
     (with-temp-buffer
       (erase-buffer)
       (let ((popup (popup-create (point) 10 10)))
         ,@body
         (popup-delete popup)))))

(ert-deftest popup-test-simple ()
  (popup-test-with-common-setup
    (popup-set-list popup '("foo" "bar" "baz"))
    (popup-draw popup)
    (should (equal (popup-list popup) '("foo" "bar" "baz")))))
