;;; -*- lexical-binding: t -*-

;;; Copyright (C) 2018 Paul Pogonyshev

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses.


(require 'extmap)
(require 'ert)


(defconst extmap--test-directory (file-name-directory (or load-file-name (buffer-file-name))))

(defvar extmap--test-filename nil)


(defun extmap--test-alist (data &rest options)
  (let ((filename (concat extmap--test-directory (or extmap--test-filename "test.extmap"))))
    (apply #'extmap-from-alist filename data :overwrite t options)
    (let ((extmap (extmap-init filename)))
      (should (equal (extmap--test-sort-keys (mapcar #'car data)) (extmap--test-sort-keys (extmap-keys extmap))))
      (dolist (entry data)
        (should (extmap-contains-key extmap (car entry)))
        (should (extmap--equal-including-properties (extmap-get extmap (car entry)) (cdr entry)))
        (should (extmap-value-loaded extmap (car entry))))
      extmap)))

(defun extmap--test-sort-keys (keys)
  (sort keys (lambda (a b) (string< (symbol-name a) (symbol-name b)))))


(ert-deftest extmap-1 ()
  (extmap--test-alist `((foo  . 1)
                        (bar  . "string")
                        (baz  . ,(number-sequence 0 100))
                        (spam . "lalala lalala lalala lalala lalala lalala lalala lalala lalala lalala lalala"))))

(ert-deftest extmap-nonascii-1 ()
  (extmap--test-alist `((раз    . 1)
                        (два    . "два")
                        (три    . ,(cons "ноль" (number-sequence 1 100)))
                        (четыре . "В траве сидел кузнечик, // В траве сидел кузнечик, // Совсем как огуречик, // Зелененький он был."))))

(ert-deftest extmap-with-text-properties-1 ()
  (extmap--test-alist `((foo  . 1)
                        (bar  . ,(propertize "string" 'face 'bold))
                        (baz  . ,(number-sequence 0 100))
                        (spam . ,(propertize "lalala lalala lalala lalala lalala lalala lalala lalala lalala lalala lalala" 'face '(bold italic)))
                        (ham  . ,(list (propertize "string" 'face '(bold italic)))))))

(ert-deftest extmap-shared-values-1 ()
  (let ((extmap (extmap--test-alist `((foo . (this value is supposed to be shared))
                                      (bar . (this value is supposed to be shared)))
                                    :share-values t :max-inline-bytes 0)))
    (should (eq (extmap-get extmap 'foo) (extmap-get extmap 'bar))))
  (let ((extmap (extmap--test-alist `((foo . (this value will not be shared even if equal))
                                      (bar . (this value will not be shared even if equal))))))
    (should-not (eq (extmap-get extmap 'foo) (extmap-get extmap 'bar)))))

(ert-deftest extmap-shared-values-2 ()
  (extmap--test-alist `((foo . (value with different ,(propertize "string properties" 'face 'bold)   must not be shared))
                        (bar . (value with different ,(propertize "string properties" 'face 'italic) must not be shared)))
                      :share-values t :max-inline-bytes 0))


(ert-deftest extmap-plain-string-p ()
  (should (extmap--plain-string-p "foo"))
  (should (extmap--plain-string-p "проверка"))
  (should-not (extmap--plain-string-p nil))
  (should-not (extmap--plain-string-p (propertize "foo" 'face 'bold)))
  (should-not (extmap--plain-string-p (concat (propertize "foo" 'face 'bold) "bar")))
  (should-not (extmap--plain-string-p (concat "foo" (propertize "bar" 'face 'bold)))))

(ert-deftest extmap-internal-equal ()
  (should-not (extmap--equal-including-properties 1 2))
  (should-not (extmap--equal-including-properties "foo" "bar"))
  (should-not (extmap--equal-including-properties [1 2 3 4] [1 2 4 5]))
  (should-not (extmap--equal-including-properties [1 2 3] [1 2 3 4]))
  (should-not (extmap--equal-including-properties '(1 2 3) '(1 2 "3")))
  (should-not (extmap--equal-including-properties '(1 2 3) '(1 2 3 4)))
  (should-not (extmap--equal-including-properties (propertize "foo" 'face 'bold) "foo"))
  (should (extmap--equal-including-properties nil nil))
  (should (extmap--equal-including-properties 1 1))
  (should (extmap--equal-including-properties (cons 'a 'b) (cons 'a 'b)))
  (should (extmap--equal-including-properties (list 1 2 3) (list 1 2 3)))
  (should (extmap--equal-including-properties (vector 1 2 3) (vector 1 2 3)))
  (should (extmap--equal-including-properties "foo" "foo"))
  (should (extmap--equal-including-properties (propertize "foo" 'face (list 'bold 'italic)) (propertize "foo" 'face (list 'bold 'italic)))))
