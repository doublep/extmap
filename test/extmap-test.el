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
      (should (equal (sort (mapcar #'car data) #'string<) (sort (extmap-keys extmap) #'string<)))
      (dolist (entry data)
        (should (extmap-contains-key extmap (car entry)))
        (should (extmap--equal-including-properties (extmap-get extmap (car entry)) (cdr entry)))
        (should (extmap-value-loaded extmap (car entry))))
      extmap)))

(defun extmap--test-compare (data1 data2 &optional keys-to-ignore &rest options)
  (let* ((filename1 (concat extmap--test-directory (or extmap--test-filename "test1.extmap")))
         (filename2 (concat extmap--test-directory (or extmap--test-filename "test2.extmap"))))
    (apply #'extmap-from-alist filename1 data1 :overwrite t options)
    (apply #'extmap-from-alist filename2 data2 :overwrite t options)
    (extmap-equal-p filename1 filename2 keys-to-ignore)))

(defun extmap--test-compress-value (value)
  (let ((compressed (extmap--compress-value value (make-hash-table :test #'extmap--equal-including-properties))))
    (should (equal compressed value))
    compressed))


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

(ert-deftest extmap-compressed-values-1 ()
  (let* ((extmap (extmap--test-alist `((foo . (compress-this: (1 2 3) (1 2 3) (0 1 2 3)))
                                       (bar . (compress-this: (1 2 3) (1 2 3) (0 1 2 3))))
                                     :compress-values t :max-inline-bytes 0))
         (foo    (extmap-get extmap 'foo))
         (bar    (extmap-get extmap 'bar)))
    (should     (eq (nth 1 foo) (nth 2 foo)))
    (should     (eq (nth 2 foo) (cdr (nth 3 foo))))
    (should     (eq (nth 1 bar) (nth 2 bar)))
    (should     (eq (nth 2 bar) (cdr (nth 3 bar))))
    (should-not (eq foo bar))))

(ert-deftest extmap-compressed-values-2 ()
  ;; Targeted at a specific bug in Emacs.  Extmap adds a workaround for it.
  (let* ((extmap (extmap--test-alist `((foo . ("some long string" "some long string")))
                                     :compress-values t :max-inline-bytes 0))
         (foo    (extmap-get extmap 'foo)))
    (should     (eq (nth 0 foo) (nth 1 foo)))))


(ert-deftest extmap-equal-p-1 ()
  (should (extmap--test-compare `((foo  . 1))
                                `((foo  . 1))))
  (should (extmap--test-compare `((foo  . 1)
                                  (bar  . "string")
                                  (baz  . ,(number-sequence 0 100))
                                  (spam . "lalala lalala lalala lalala lalala lalala lalala lalala lalala lalala lalala"))
                                `((foo  . 1)
                                  (bar  . "string")
                                  (baz  . ,(number-sequence 0 100))
                                  (spam . "lalala lalala lalala lalala lalala lalala lalala lalala lalala lalala lalala")))))

(ert-deftest extmap-equal-p-2 ()
  (should-not (extmap--test-compare `((foo . 1))
                                    `((foo . 2))))
  (should-not (extmap--test-compare `((foo . 1))
                                    `((foo . 2))
                                    '(what?)))
  (should-not (extmap--test-compare `((foo . 1))
                                    `((bar . 2))))
  (should-not (extmap--test-compare `((foo . 1))
                                    `((foo . 1)
                                      (bar . 2))))
  (should-not (extmap--test-compare `((foo . 1)
                                      (bar . 2))
                                    `((foo . 1)))))

(ert-deftest extmap-equal-p-3 ()
  (should (extmap--test-compare `((foo . 1))
                                `((foo . 2))
                                '(foo)))
  (should (extmap--test-compare `((foo . 1))
                                `((bar . 2))
                                '(foo bar)))
  (should (extmap--test-compare `((foo . 1))
                                `((foo . 1)
                                  (bar . 2))
                                '(bar)))
  (should (extmap--test-compare `((foo . 1)
                                  (bar . 2))
                                `((foo . 1))
                                '(bar))))

(ert-deftest extmap-equal-p-nonascii-1 ()
  (should (extmap--test-compare `((раз    . 1)
                                  (два    . "два")
                                  (три    . ,(cons "ноль" (number-sequence 1 100)))
                                  (четыре . "В траве сидел кузнечик, // В траве сидел кузнечик, // Совсем как огуречик, // Зелененький он был."))
                                `((раз    . 1)
                                  (два    . "два")
                                  (три    . ,(cons "ноль" (number-sequence 1 100)))
                                  (четыре . "В траве сидел кузнечик, // В траве сидел кузнечик, // Совсем как огуречик, // Зелененький он был.")))))


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

(ert-deftest extmap-internal-compress-value ()
  (extmap--test-compress-value '(nothing to compress here))
  (let ((compressed (extmap--test-compress-value '((1 2 3) (4 5 6) (1 2 3)))))
    (should (eq (nth 0 compressed) (nth 2 compressed))))
  (let ((compressed (extmap--test-compress-value '((1 2 3) (4 5 6) . (1 2 3)))))
    (should (eq (car compressed) (cddr compressed))))
  (let ((compressed (extmap--test-compress-value '[[1 2 3] [4 5 6] [1 2 3]])))
    (should (eq (aref compressed 0) (aref compressed 2)))))
