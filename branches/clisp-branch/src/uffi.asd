;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-
(defpackage :asdf-uffi (:use #:asdf #:cl))
 (in-package :asdf-uffi)
 (defsystem uffi :depends-on (:cffi-uffi-compat))
;;; End file uffi.asd
