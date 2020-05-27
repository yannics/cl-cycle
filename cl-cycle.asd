;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :ASDF)

(defsystem :cl-cycle
  :name "cl-cycle"
  :author "Yann Ics"
  :maintainer "Yann Ics"
  :description "Cycle Generator"
  :version "1.20.6"
  :serial t
  :components
  (
   (:FILE "package")
   (:FILE "src")
   (:FILE "cl-cycle")
  )
 )
