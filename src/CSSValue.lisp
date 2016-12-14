
(in-package :karen.css)

;;; CSS Value
;;; a value of parsed CSS property becomes css-value.

(defstruct css-value
  (data nil)
  (unit nil :type (or keyword null)))


;;; CSS Animation
;;; keyframes of CSS animation.

(defstruct css-animation
  (name "" :type string)
  (anchor '() :type list))
