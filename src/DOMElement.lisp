
(in-package :karen.dom)

;;; DOM Element
;;; a parsed HTML tag becomes dom-element.

(defstruct dom-element
  (ident :any :type keyword)
  (name "" :type string)
  (attributes '() :type list)
  (parent nil :type (or dom-element null))
  (children '()  :type list)
  (previous nil :type (or dom-element null))
  (next nil :type (or dom-element null)))


