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
        (should (equal (extmap-get extmap (car entry)) (cdr entry)))
        (should (extmap-value-loaded extmap (car entry)))))))

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
