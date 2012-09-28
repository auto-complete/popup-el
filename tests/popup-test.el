(require 'ert)

(require 'popup)
;; for "every" function
(require 'cl)

(when (< (frame-width) (length "long long long long line"))
  (set-frame-size (selected-frame) 80 35))

(defmacro popup-test-with-create-popup (&rest body)
  (declare (indent 0) (debug t))
  `(let ((popup (popup-create (point) 10 10)))
     (unwind-protect
         (progn ,@body)
       (when popup
         (popup-delete popup)))
     ))

(defmacro popup-test-with-temp-buffer (&rest body)
  (declare (indent 0) (debug t))
  `(save-excursion
     (with-temp-buffer
       (switch-to-buffer (current-buffer))
       (delete-other-windows)
       (erase-buffer)
       ,@body
       )))

(defmacro popup-test-with-common-setup (&rest body)
  (declare (indent 0) (debug t))
  `(popup-test-with-temp-buffer
     (popup-test-with-create-popup ,@body)))

(defun popup-test-helper-get-overlays-buffer ()
  "Create a new buffer called *text* containing the visible text
of the current buffer, ie. it converts overlays containing text
into real text. Return *text* buffer"
  (interactive)
  (let ((tb (get-buffer-create "*text*"))
        (s (point-min))
        (os (overlays-in (point-min) (point-max))))
    (with-current-buffer tb
      (erase-buffer))
    (setq os (sort os (lambda (o1 o2)
                        (< (overlay-start o1)
                           (overlay-start o2)))))
    (mapc (lambda (o)
            (let ((bt (buffer-substring-no-properties s (overlay-start o)))
                  (b (overlay-get o 'before-string))
                  (text (or (overlay-get o 'display)
                            (buffer-substring-no-properties (overlay-start o) (overlay-end o))))
                  (a (overlay-get o 'after-string))
                  (inv (overlay-get o 'invisible)))
              (with-current-buffer tb
                (insert bt)
                (unless inv
                  (when b (insert b))
                  (insert text)
                  (when a (insert a))))
              (setq s (overlay-end o))))
          os)
    (let ((x (buffer-substring-no-properties s (point-max))))
      (with-current-buffer tb
        (insert x)
        tb))))

(defun popup-test-helper-match-points (strings)
  "Return list of start of first match"
  (when (listp strings)
    (let ((text (buffer-string)))
      (mapcar
       (lambda (content)
         (let ((pos (string-match (regexp-quote content) text)))
           (if (null pos) pos (1+ pos))))
       strings))))

(defun popup-test-helper-points-to-columns (points)
  "Return list of colum"
  (mapcar
   (lambda (point)
     (if point
         (save-excursion (goto-char point) (current-column))
       nil))
   points))

(defun popup-test-helper-same-all-p (seq)
  "Return first element if `eq' every element of SEQ.If not, return nil."
  (reduce #'(lambda (x y) (if (eq x y) x nil)) seq))

(defun popup-test-helper-input (key)
  (push key unread-command-events))

(defun popup-test-helper-in-popup-p ()
  (let* ((face (get-text-property (point) 'face))
         (face (if (listp face) face (list face))))
    (some (lambda (face) (if (memq face '(popup-tip-face popup-face)) t nil))
          face)))

(defun popup-test-helper-last-popup-line ()
  "Return (START END) list of last popup line"
  (save-excursion
    (end-of-buffer)
    (let ((end (point)))
      (while (and (not (popup-test-helper-in-popup-p))
                  (let ((pos (previous-single-property-change (point) 'face)))
                    (when pos
                      (setq end (point))
                      (goto-char pos))
                    pos)))
      (if (popup-test-helper-in-popup-p) `(,(point) ,end) nil)
      )))

;; Test for helper method
(ert-deftest popup-test-test-helper ()
  (should (eq (popup-test-helper-same-all-p '(0 0 0)) 0))
  (should (eq (popup-test-helper-same-all-p '(1 1 1)) 1))
  (should (eq (popup-test-helper-same-all-p '(0 1 1)) nil))
  )

;; Test for popup-el
(ert-deftest popup-test-simple ()
  (popup-test-with-common-setup
    (popup-set-list popup '("foo" "bar" "baz"))
    (popup-draw popup)
    (should (equal (popup-list popup) '("foo" "bar" "baz")))
    (with-current-buffer (popup-test-helper-get-overlays-buffer)
      (let ((points (popup-test-helper-match-points '("foo" "bar" "baz"))))
        (should (every #'identity points))
        (should (equal (popup-test-helper-points-to-columns points) '(0 0 0)))
        (should (eq (popup-test-helper-same-all-p
                     (popup-test-helper-points-to-columns points)) 0))))))

(ert-deftest popup-test-delete ()
  (popup-test-with-common-setup
    (popup-delete popup)
    (should-not (popup-live-p popup))))

(ert-deftest popup-test-hide ()
  (popup-test-with-common-setup
    (popup-set-list popup '("foo" "bar" "baz"))
    (popup-draw popup)
    (popup-hide popup)
    (should (equal (popup-list popup) '("foo" "bar" "baz")))
    (with-current-buffer (popup-test-helper-get-overlays-buffer)
      (should-not (every #'identity
                         (popup-test-helper-match-points '("foo" "bar" "baz")))))
    ))

(ert-deftest popup-test-tip ()
  (popup-test-with-temp-buffer
    (popup-tip
     "Start isearch on POPUP. This function is synchronized, meaning
event loop waits for quiting of isearch.

CURSOR-COLOR is a cursor color during isearch. The default value
is `popup-isearch-cursor-color'.

KEYMAP is a keymap which is used when processing events during
event loop. The default value is `popup-isearch-keymap'.

CALLBACK is a function taking one argument. `popup-isearch' calls
CALLBACK, if specified, after isearch finished or isearch
canceled. The arguments is whole filtered list of items.

HELP-DELAY is a delay of displaying helps."
     :nowait t)
    (with-current-buffer (popup-test-helper-get-overlays-buffer)
      (let ((points (popup-test-helper-match-points
                     '("CURSOR-COLOR is a cursor color during isearch"
                       "KEYMAP is a keymap"))))
        (should (every #'identity points))
        (should (eq (popup-test-helper-same-all-p
                     (popup-test-helper-points-to-columns points)) 0)))
      )))

(ert-deftest popup-test-culumn ()
  (popup-test-with-temp-buffer
    (insert " ")
    (popup-test-with-create-popup
      (popup-set-list popup '("foo" "bar" "baz"))
      (popup-draw popup)
      (should (equal (popup-list popup) '("foo" "bar" "baz")))
      (with-current-buffer (popup-test-helper-get-overlays-buffer)
        (let ((points (popup-test-helper-match-points '("foo" "bar" "baz"))))
          (should (every #'identity points))
          (should (equal (popup-test-helper-points-to-columns points)
                         '(1 1 1)))
          )
        ))))

(ert-deftest popup-test-folding-long-line-right-top ()
  (popup-test-with-temp-buffer
    ;; To use window-width because Emacs 23 does not have window-body-width
    (insert (make-string (- (window-width) 3) ? ))
    (popup-tip "long long long long line" :nowait t)
    (with-current-buffer (popup-test-helper-get-overlays-buffer)
      (let ((points (popup-test-helper-match-points
                     '("long long long long line"))))
        (should (every #'identity points))
        (should (eq (line-number-at-pos (car points)) 2))
        ))))

(ert-deftest popup-test-folding-long-line-left-bottom ()
  (popup-test-with-temp-buffer
    (insert (make-string (- (window-body-height) 1) ?\n))
    (popup-tip "long long long long line" :nowait t)
    (with-current-buffer (popup-test-helper-get-overlays-buffer)
      (let ((points (popup-test-helper-match-points
                     '("long long long long line"))))
        (should (every #'identity points))
        (should (eq (line-number-at-pos (car points))
                    (- (window-body-height) 1)))
        ))))

(ert-deftest popup-test-folding-long-line-right-bottom ()
  (popup-test-with-temp-buffer
    (insert (make-string (- (window-body-height) 1) ?\n))
    (insert (make-string (- (window-width) 3) ? ))
    (popup-tip "long long long long line" :nowait t)
    (with-current-buffer (popup-test-helper-get-overlays-buffer)
      (let ((points (popup-test-helper-match-points
                     '("long long long long line"))))
        (should (every #'identity points))
        (should (eq (line-number-at-pos (car points))
                    (- (window-body-height) 1))))
      )))

(ert-deftest popup-test-folding-short-line-right-top ()
  (popup-test-with-temp-buffer
    (insert (make-string (- (window-width) 3) ? ))
    (popup-tip "bla\nbla\nbla\nbla\nbla" :nowait t)
    (with-current-buffer (popup-test-helper-get-overlays-buffer)
      (let ((points (popup-test-helper-match-points
                     '("bla"))))
        (should (every #'identity points))
        (should (eq (line-number-at-pos (car points)) 2))
        ))))

(ert-deftest popup-test-folding-short-line-left-bottom ()
  (popup-test-with-temp-buffer
    (insert (make-string (- (window-body-height) 1) ?\n))
    (popup-tip "bla\nbla\nbla\nbla\nbla" :nowait t)
    (with-current-buffer (popup-test-helper-get-overlays-buffer)
      (let ((points (popup-test-helper-match-points
                     '("bla"))))
        (should (every #'identity points))
        (should (eq (popup-test-helper-same-all-p
                     (popup-test-helper-points-to-columns points)) 0))
        (should (eq (line-number-at-pos (car points))
                    (- (window-body-height) 5)))
        ))))

(ert-deftest popup-test-folding-short-line-right-bottom ()
  (popup-test-with-temp-buffer
    (insert (make-string (- (window-body-height) 1) ?\n))
    (insert (make-string (- (window-width) 3) ? ))
    (popup-tip "bla\nbla\nbla\nbla\nbla" :nowait t)
    (with-current-buffer (popup-test-helper-get-overlays-buffer)
      (let ((points (popup-test-helper-match-points
                     '("bla"))))
        (should (every #'identity points))
        (should (eq (line-number-at-pos (car points))
                    (- (window-body-height) 5))))
      )))

(ert-deftest popup-test-margin-at-middle ()
  (popup-test-with-temp-buffer
    (insert " ")
    (let ((popup (popup-tip "Margin?" :nowait t :margin t)))
      (with-current-buffer (popup-test-helper-get-overlays-buffer)
        (let ((points (popup-test-helper-match-points '(" Margin? "))))
          (should (every #'identity points))
          (should (equal (car (popup-test-helper-points-to-columns points))
                         0))
          )))))

(ert-deftest popup-test-two-lines ()
  (popup-test-with-temp-buffer
    (let ((popup (popup-tip "Foo\nBar\nBaz" :nowait t :height 2)))
      (with-current-buffer (popup-test-helper-get-overlays-buffer)
        (let ((points (popup-test-helper-match-points '("Foo" "Bar" "Baz"))))
          (should (equal points '(2 6 nil)))
          (should (equal (popup-test-helper-points-to-columns points)
                         '(0 0 nil)))
          (should (eq (line-number-at-pos (car points)) 2))
          )))))

(ert-deftest popup-test-two-lines-bottom ()
  (popup-test-with-temp-buffer
    (insert (make-string (- (window-body-height) 1) ?\n))
    (let ((popup (popup-tip "Foo\nBar\nBaz" :nowait t :height 2)))
      (with-current-buffer (popup-test-helper-get-overlays-buffer)
        (let ((points (popup-test-helper-match-points '("Foo" "Bar" "Baz"))))
          (should (equal (popup-test-helper-points-to-columns points)
                         '(0 0 nil)))
          (should (eq (line-number-at-pos (car points))
                      (- (window-body-height) 2)))
          )))))

(ert-deftest popup-test-scroll-bar ()
  (popup-test-with-temp-buffer
    (let* ((popup-scroll-bar-foreground-char
            (propertize "f" 'face 'popup-scroll-bar-foreground-face))
           (popup-scroll-bar-background-char
            (propertize "b" 'face 'popup-scroll-bar-background-face))
           (popup (popup-tip "Foo\nBar\nBaz\nFez\nOz"
                             :nowait t :height 3 :scroll-bar t :margin t)))
      (with-current-buffer (popup-test-helper-get-overlays-buffer)
        (let ((points (popup-test-helper-match-points
                       '("Foo f" "Bar b" "Baz b" "Fez"))))
          (should (equal (popup-test-helper-points-to-columns points)
                         '(0 0 0 nil)))
          (should (eq (line-number-at-pos (car points)) 2))
          )))))

(ert-deftest popup-test-scroll-bar-right-no-margin ()
  (popup-test-with-temp-buffer
    (insert (make-string (- (window-width) 1) ? ))
    (let* ((popup-scroll-bar-foreground-char
            (propertize "f" 'face 'popup-scroll-bar-foreground-face))
           (popup-scroll-bar-background-char
            (propertize "b" 'face 'popup-scroll-bar-background-face))
           (popup (popup-tip "Foo\nBar\nBaz\nFez\nOz"
                             :nowait t :height 3 :scroll-bar t)))
      (with-current-buffer (popup-test-helper-get-overlays-buffer)
        (let ((points (popup-test-helper-match-points
                       '("Foof" "Barb" "Bazb"))))
          (should (equal (popup-test-helper-same-all-p
                          (popup-test-helper-points-to-columns points))
                         (- (window-width) 5)))
          (should (eq (line-number-at-pos (car points)) 2))
          )))))

(ert-deftest popup-test-min-height ()
  (popup-test-with-temp-buffer
    (insert (make-string (- (window-width) 1) ? ))
    (let ((popup (popup-tip "Hello" :nowait t :min-height 10)))
      (with-current-buffer (popup-test-helper-get-overlays-buffer)
        (let ((points (popup-test-helper-match-points
                       '("Hello")))
              (last-bounds (popup-test-helper-last-popup-line)))
          (should (eq (line-number-at-pos (car points)) 2))
          (should (eq
                   (car (popup-test-helper-points-to-columns points))
                   (car (popup-test-helper-points-to-columns last-bounds))))
          (should (eq (line-number-at-pos (car last-bounds)) 11))
          (should (eq (nth 1 (popup-test-helper-points-to-columns last-bounds))
                      (1- (window-width))))
          )))))

(ert-deftest popup-test-menu ()
  (popup-test-with-temp-buffer
    (let ((popup (popup-menu* '("Foo" "Bar" "Baz") :nowait t)))
      (should (equal (popup-list popup) '("Foo" "Bar" "Baz")))
      (with-current-buffer (popup-test-helper-get-overlays-buffer)
        (let ((points (popup-test-helper-match-points
                       '("Foo" "Bar" "Baz"))))
          (should (eq (line-number-at-pos (car points)) 2))
          (should (equal (popup-test-helper-points-to-columns points)
                         '(0 0 0)))
          )))))

(ert-deftest popup-test-cascade-menu ()
  (popup-test-with-temp-buffer
    (let ((popup (popup-cascade-menu
                  '(("Foo" "Foo1" "Foo2") "Bar" "Baz") :nowait t :margin t)))
      (should (string= (car (popup-list popup)) "Foo"))
      (should (equal (popup-item-sublist (car (popup-list popup)))
                     '("Foo1" "Foo2")))
      (should (equal (popup-item-symbol (car (popup-list popup))) ">"))
      (should (equal (cdr (popup-list popup)) '("Bar" "Baz")))
      (with-current-buffer (popup-test-helper-get-overlays-buffer)
        (let ((points (popup-test-helper-match-points
                       '("Foo" "Bar" "Baz" "Foo1"))))
          (should (eq (line-number-at-pos (car points)) 2))
          (should (equal (popup-test-helper-points-to-columns points)
                         '(0 0 0 nil)))
          )))))

(ert-deftest popup-test-next ()
  (popup-test-with-temp-buffer
    (let ((popup (popup-cascade-menu
                  '("Foo" "Bar" "Baz") :nowait t :margin t)))
      (should (equal (popup-list popup) '("Foo" "Bar" "Baz")))
      (should (equal (popup-selected-item popup) "Foo"))
      (popup-next popup)
      (should (equal (popup-selected-item popup) "Bar"))
      (popup-next popup)
      (should (equal (popup-selected-item popup) "Baz"))
      (popup-next popup)
      (should (equal (popup-selected-item popup) "Foo"))
      (with-current-buffer (popup-test-helper-get-overlays-buffer)
        (let ((points (popup-test-helper-match-points
                       '("Foo" "Bar" "Baz"))))
          (should (eq (line-number-at-pos (car points)) 2))
          (should (equal (popup-test-helper-points-to-columns points)
                         '(0 0 0)))
          )))))

(ert-deftest popup-test-previous ()
  (popup-test-with-temp-buffer
    (let ((popup (popup-cascade-menu
                  '("Foo" "Bar" "Baz") :nowait t :margin t)))
      (should (equal (popup-list popup) '("Foo" "Bar" "Baz")))
      (should (equal (popup-selected-item popup) "Foo"))
      (popup-previous popup)
      (should (equal (popup-selected-item popup) "Baz"))
      (popup-previous popup)
      (should (equal (popup-selected-item popup) "Bar"))
      (popup-previous popup)
      (should (equal (popup-selected-item popup) "Foo"))
      (with-current-buffer (popup-test-helper-get-overlays-buffer)
        (let ((points (popup-test-helper-match-points
                       '("Foo" "Bar" "Baz"))))
          (should (eq (line-number-at-pos (car points)) 2))
          (should (equal (popup-test-helper-points-to-columns points)
                         '(0 0 0)))
          )))))

(ert-deftest popup-test-select ()
  (popup-test-with-temp-buffer
    (let ((popup (popup-cascade-menu
                  '("Foo" "Bar" "Baz") :nowait t :margin t)))
      (should (equal (popup-list popup) '("Foo" "Bar" "Baz")))
      (should (equal (popup-selected-item popup) "Foo"))
      (popup-select popup 1)
      (should (equal (popup-selected-item popup) "Bar"))
      (popup-select popup 0)
      (should (equal (popup-selected-item popup) "Foo"))
      (popup-select popup 2)
      (should (equal (popup-selected-item popup) "Baz"))
      (with-current-buffer (popup-test-helper-get-overlays-buffer)
        (let ((points (popup-test-helper-match-points
                       '("Foo" "Bar" "Baz"))))
          (should (eq (line-number-at-pos (car points)) 2))
          (should (equal (popup-test-helper-points-to-columns points)
                         '(0 0 0)))
          )))))
