;;; extmap.el --- Externally-stored constant mapping for Elisp  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Paul Pogonyshev

;; Author:     Paul Pogonyshev <pogonyshev@gmail.com>
;; Maintainer: Paul Pogonyshev <pogonyshev@gmail.com>
;; Version:    0.9
;; Keywords:   lisp
;; Homepage:   https://github.com/doublep/extmap
;; Package-Requires: ((emacs "25.1"))

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


;;; Commentary:

;; Mapping of symbols to constants that is stored externally and
;; loaded on-demand.  Should be used for huge databases to avoid
;; loading everything to memory.  This package doesn't use any
;; external programs, making it a suitable dependency for smaller
;; libraries.


;;; Code:

(require 'bindat)
(require 'generator)


(defconst extmap--header-bindat-spec '((magic     u16)
                                       (version   u16)
                                       (num-items u32)
                                       (offset    u32)))

;; TYPE can be:
;;   0 -- inlined string (short specification is used);
;;   1 -- inlined Lisp object (short specification is used);
;;   2 -- string;
;;   3 -- Lisp object.
(defconst extmap--item-short-bindat-spec '((type      u8)
                                           (length    u24)))
(defconst extmap--item-bindat-spec       '((type      u8)
                                           (length    u24)
                                           (offset    u32)))


(defun extmap-init (filename &optional weak-data)
  "Load metadata of a previously created map from FILENAME.

Loaded metadata can be further passed to `extmap-get' and other
functions.  It must be treated as an opaque object: you must not
alter it or make any assumptions about its contents.

If WEAK-DATA is non-nil, loaded values are stored in a weak
hashmap and can be garbage-collected by Emacs if no longer used.
This allows to potentially reduce memory usage at the cost of
more disk operations.

The file must remain accessible in case `extmap-get' needs to
load a value later.  There is no need to somehow close a map:
just stop using it."
  (with-temp-buffer
    (let* ((header-length            (bindat-length extmap--header-bindat-spec     nil))
           (item-short-header-length (bindat-length extmap--item-short-bindat-spec nil))
           (item-header-length       (bindat-length extmap--item-bindat-spec       nil))
           (read-as                  (insert-file-contents-literally filename nil 0 header-length))
           (filename                 (car read-as))
           (header                   (when (equal (cadr read-as) header-length) (bindat-unpack extmap--header-bindat-spec (encode-coding-string (buffer-string) 'no-conversion))))
           items)
      (erase-buffer)
      (unless (= (bindat-get-field header 'magic) #x91f7)
        (error "Wrong or corrupted extmap in file `%s'" filename))
      (unless (= (bindat-get-field header 'version) 0)
        (error "Future version of extmap in file `%s', upgrade your `extmap' package" filename))
      (setq items (make-hash-table :test #'eq :size (bindat-get-field header 'num-items)))
      (insert-file-contents-literally filename nil (bindat-get-field header 'offset))
      (dotimes (_ (bindat-get-field header 'num-items))
        (let* ((key         (intern (decode-coding-string (buffer-substring-no-properties (point) (1- (re-search-forward (rx 0)))) 'utf-8 t)))
               (item-header (bindat-unpack extmap--item-short-bindat-spec (encode-coding-string (buffer-substring-no-properties (point) (+ (point) item-short-header-length)) 'no-conversion)))
               (type        (bindat-get-field item-header 'type))
               (length      (bindat-get-field item-header 'length)))
          (if (<= type 1)
              ;; Inlined item.
              (let ((value (decode-coding-string (buffer-substring-no-properties (+ (point) item-short-header-length) (+ (point) item-short-header-length length)) 'utf-8 t)))
                (goto-char (+ (point) item-short-header-length length))
                (when (= type 1)
                  (setq value (car (read-from-string value))))
                (puthash key (cons t value) items))
            ;; Non-inlined item.
            (let ((item-header (bindat-unpack extmap--item-bindat-spec (encode-coding-string (buffer-substring-no-properties (point) (+ (point) item-header-length)) 'no-conversion))))
              (goto-char (+ (point) item-header-length))
              (puthash key (cons nil (cons type (cons (bindat-get-field item-header 'offset) length))) items)))))
      (list filename items (when weak-data (make-hash-table :test #'eq :weakness 'value))))))

(defun extmap-get (extmap key &optional no-error)
  "Get value associated with KEY from the map.

EXTMAP must be a result of a previous call to `extmap-init'.  KEY
should be a symbol present in the map.  If it is not, function
signals an error, unless NO-ERROR is specified, in which case it
returns nil."
  (let* ((items     (nth 1 extmap))
         (weak-data (nth 2 extmap))
         ;; A key cannot be mapped to `items' table itself, we use
         ;; that as a marker for "no mapping" situation.
         (value     items))
    (when weak-data
      (setq value (gethash key weak-data items)))
    (if (eq value items)
        (progn (setq value (gethash key items))
               (if value
                   (if (car value)
                       ;; Already loaded.
                       (cdr value)
                     ;; Load now.
                     (let ((coding-system-for-read 'utf-8)
                           (offset                 (caddr value)))
                       (with-temp-buffer
                         (insert-file-contents (nth 0 extmap) nil offset (+ offset (cdddr value)))
                         (let ((new-value (if (= (cadr value) 2) (buffer-string) (read (current-buffer)))))
                           (if weak-data
                               (puthash key new-value weak-data)
                             (prog1 (setcdr value new-value)
                               (setcar value t)))))))
                 (unless no-error
                   (error "No value for key `%s'" key))))
      value)))

(defun extmap-contains-key (extmap key)
  "Determine if there is a mapping for given KEY in EXTMAP."
  (consp (gethash key (nth 1 extmap))))

(defun extmap-value-loaded (extmap key)
  "Determine if value for given KEY is loaded in EXTMAP.
If there is no mapping for KEY, this function always returns
nil.

In case the map has been initialized with WEEK-DATA argument, it
may happen that this function returns t, but value for the KEY
has to be loaded again in the future."
  (or (car-safe (gethash key (nth 1 extmap)))
      (let ((weak-data (nth 2 extmap)))
        (when weak-data
          (not (eq (gethash key weak-data weak-data) weak-data))))))

(defun extmap-keys (extmap)
  "Return a list of all the keys in the map.
The list is in no particular order.

EXTMAP must be a result of a previous call to `extmap-init'."
  (let (keys)
    (maphash (lambda (key _value) (push key keys)) (nth 1 extmap))
    keys))

(defun extmap-mapc (extmap callback)
  "Invoke CALLBACK on each key-value pairing in the map.

EXTMAP must be a result of a previous call to `extmap-init'.

CALLBACK is called with two arguments: KEY and VALUE.  Its return
value is ignored.  Values in the map are enumerated in no
particular order.

Note that unless CALLBACK exits non-locally (with `throw' or by
signalling an error), this will result in loading all values into
memory.  If you just need to enumerate the keys, use
`extmap-keys' instead."
  (maphash (lambda (key _value) (funcall callback key (extmap-get extmap key))) (nth 1 extmap)))

(defun extmap-mapcar (extmap callback)
  "Invoke CALLBACK on each key-value pairing in the map.
Return its results as a list.

Returned list corresponds to the order in which keys have been
passed to CALLBACK.  However, that order can be arbitrary.

See `extmap-mapc' for more information."
  (let (result)
    (maphash (lambda (key _value) (push (funcall callback key (extmap-get extmap key)) result)) (nth 1 extmap))
    (nreverse result)))

(defun extmap-statistics (extmap)
  "Collect and return some statistics about EXTMAP.

Returned value is an alist (in no particular order) with at least
the following items:

    `filename': absolute path of the file that contains the map;
    `num-items': number of key-value mappings in the map;
    `num-loaded': number of loaded values.

In some cases maps can report loaded values right after
initialization.  This is because of value inlining and typically
happens for small values.  In case the map has been initialized
with WEEK-DATA argument, `num-loaded' should be seen as an upper
limit only, as (some) loaded values can be garbage-collected at
any time."
  (let ((items      (nth 1 extmap))
        (weak-data  (nth 2 extmap))
        (num-loaded 0))
    (maphash (lambda (_key value) (when (car value) (setq num-loaded (1+ num-loaded)))) items)
    `((filename   . ,(nth 0 extmap))
      (num-items  . ,(hash-table-count items))
      (num-loaded . ,(+ num-loaded (if weak-data (hash-table-count weak-data) 0))))))


(defun extmap-from-alist (filename data &rest options)
  "Create an externally-stored map from given DATA.

Created map is stored to file specified by FILENAME.  Later this
filename should be passed to `extmap-init' to read the map.

DATA must be an alist with symbols used as keys.  All symbols
must be interned in the default obarray.  Values must be
serializable with `print'/`read' (e.g. no buffers or non-interned
symbols), but are otherwise not restricted.

OPTIONS can be a list of the following keyword arguments:

  :overwrite

    By default, this function will signal an error if the output
    file already exists.  However, you can order it to
    overwrite (not merge!) the file.

  :max-inline-bytes

    Inline values for which `print' results in this many bytes.
    Inlined values are loaded by `extmap-init' immediately and
    don't require additional disk access later.  Default value is
    currently 16, but can be changed in a future package version.
    If this setting is important for you for some reason, always
    specify it explicitly."
  (apply #'extmap-from-iterator filename (funcall (iter-lambda () (while data (iter-yield (pop data))))) options))

(defun extmap-from-iterator (filename iterator &rest options)
  "Create an externally-stored map from data provided by ITERATOR.
Iterator must return cons cells with car being a symbol (key),
cdr -- the value.  See standard Emacs form `iter-defun' for how
to declare iterator functions.

See `extmap-from-alist' for more information.  This function is
basically the same, but is provided for the cases your values are
so huge you'd rather provide them one-by-one with an iterator
rather than keep them all in memory."
  (with-temp-buffer
    (let ((print-level                nil)
          (print-length               nil)
          (max-inline-bytes           (or (plist-get options :max-inline-bytes) 16))
          (offset                     (bindat-length extmap--header-bindat-spec nil))
          (buffer                     (current-buffer))
          (coding-system-for-write    'no-conversion)
          (write-region-inhibit-fsync t)
          (used-keys                   (make-hash-table :test #'eq)))
      ;; Will be replaced at the end.
      (insert (bindat-pack extmap--header-bindat-spec nil))
      (write-region (point-min) (point-max) filename nil nil nil (if (plist-get options :overwrite) nil 'excl))
      (erase-buffer)
      (iter-do (item iterator)
        (let ((key   (car item))
              (value (cdr item)))
          (unless (and (symbolp key) (not (string-match (rx 0) (symbol-name key))) (eq (intern (symbol-name key)) key))
            (error "Wrong key `%S': expected an interned symbol without null character" key))
          (when (gethash key used-keys)
            (error "Duplicate key `%s'" key))
          (puthash key t used-keys)
          (insert (encode-coding-string (symbol-name key) 'utf-8 t))
          (insert 0)
          (with-temp-buffer
            (let ((serialized (if (stringp value) value (prin1-to-string value))))
              (unless (or (stringp value) (condition-case _ (equal (read serialized) value) (error nil)))
                (error "Value for key `%s' cannot be saved in database: it cannot be read back or is different after reading" key))
              (insert (encode-coding-string serialized 'utf-8 t))
              (let ((num-bytes (buffer-size)))
                (if (<= num-bytes max-inline-bytes)
                    (let ((serialized-in (current-buffer)))
                      (with-current-buffer buffer
                        (insert (bindat-pack extmap--item-short-bindat-spec `((type . ,(if (stringp value) 0 1)) (length . ,num-bytes))))
                        (insert-buffer-substring serialized-in)))
                  (write-region (point-min) (point-max) filename t)
                  (with-current-buffer buffer
                    (insert (bindat-pack extmap--item-bindat-spec `((type . ,(if (stringp value) 2 3)) (length . ,num-bytes) (offset . ,offset))))
                    (setq offset (+ offset num-bytes)))))))))
      (write-region (point-min) (point-max) filename t)
      ;; Update the header.
      (erase-buffer)
      (insert (bindat-pack extmap--header-bindat-spec `((magic     . #x91f7)
                                                        (version   . 0)
                                                        (num-items . ,(hash-table-count used-keys))
                                                        (offset    . ,offset))))
      (write-region (point-min) (point-max) filename 0))))


(provide 'extmap)

;;; extmap.el ends here
