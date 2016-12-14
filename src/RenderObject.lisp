
(in-package :karen.renderer)

;;; Render Object
;;; a dom-element becomes render-object.

(defstruct render-object
  (dom :type dom-element)
  (style :type render-style)
  (clip :type clip-layer)
  (animation '() :type list))


;;; Render Style
;;; rendering style of a render-object, such as position, size, color, and so on.

(defstruct render-style
  (background nil :type (or renderer-box-fill null))
  (margin nil :type (or renderer-box-fill null))
  (padding nil :type (or renderer-box-fill null))
  (border nil :type (or renderer-box-stroke null))
  (text nil :type (or renderer-text null)))


;;; Clip Layer
;;; region settings for clipping.

(defstruct clip-layer
  (x-mode :auto :type keyword)
  (y-mode :auto :type keyword)
  (x 0.0 :type float)
  (y 0.0 :type float)
  (width 0.0 :type float)
  (height 0.0 :type float))
