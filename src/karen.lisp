(in-package :cl-user)
(defpackage karen
  (:use :cl
        :gtk
        :gdk
        :gdk-pixbuf
        :gobject
        :glib
        :gio
        :pango
        :cairo))
(in-package :karen)

(cl-annot:enable-annot-syntax)

(defclass has-parent ()
  ((parent :accessor get-parent
           :initarg :parent
           :initform nil)))

(defclass has-sibling ()
  ((previous :accessor get-previous
             :initarg :previous
             :initform nil)
   (next :accessor get-next
         :initarg :next
         :initform nil)))

(defclass has-children ()
 ((children :accessor get-children
            :initarg :children
            :initform nil)))

(defclass tagged-element ()
  ((tag :accessor get-tag
        :initarg :tag
        :initform "")
   (attribute :accessor get-attr
              :initarg :attr
              :initform (make-hash-table :test #'eq))))

(defclass drawable ()
  ((style :accessor get-style
          :initarg :style
          :initform (make-hash-table :test #'eq))
   (draw-x :accessor get-x
           :initarg :x
           :initform 0)
   (draw-y :accessor get-y
           :initarg :y
           :initform 0)
   (draw-width :accessor get-w
               :initarg :w
               :initform 0)
   (draw-height :accessor get-h
                :initarg :h
                :initform 0)
   (below-x :accessor get-below-x
            :initarg :below-x
            :initform 0)
   (below-y :accessor get-below-y
            :initarg :below-y
            :initform 0)))

; head
(defclass head-element
  (has-sibling
   has-parent
   has-children
   tagged-element)
  ())

; (html body div span article section table h* list other)
(defclass container-element
  (has-sibling
   has-parent
   has-children
   tagged-element
   drawable)
  ())

; plane text
(defclass text-element
  (has-sibling
   has-parent
   drawable)
  ((text :accessor get-text
         :initarg :text
         :initform "")))

; meta
(defclass meta-element
  (has-sibling
   has-parent
   has-children
   tagged-element)
  ())

; title script style
(defclass data-element
  (has-sibling
   has-parent
   has-children
   tagged-element)
  ())

; img
(defclass image-element
  (has-sibling
   has-parent
   has-children
   tagged-element
   drawable)
  ())

; svg
(defclass xml-element
  (has-sibling
   has-parent
   has-children
   tagged-element
   container
   drawable)
  ())

;;;
;;; 
;;;
@inline
(defun %displace (e)
  (let ((parent (get-parent e))
        (previous (get-previous e)))
    (when (and (not (null parent)) (typep parent 'drawable)) ;; if display:static or relative
      (setf (get-x e) (get-x parent)
            (get-y e) (get-y parent)))
    (when (and (not (null previous)) (typep previous 'drawable))
      (setf (get-x e) ;; if position:inline
            (get-below-x previous)
            (get-y e)
            (get-below-y previous)
            ;(+ (get-x previous) (get-w previous))
            ))))

;;;
;;; Container draw method
;;; container-element
;;;
(defmethod draw (cr (e container-element))
  (%displace e)
  (setf (get-below-x e)
        (+ (get-x e) (get-w e))))

;;;
;;; Set container bound size method
;;; container-element
;;;
(defmethod calc-bound (cr (e container-element)))

;;;
;;; Text draw method
;;; text-element
;;;
(defmethod draw (cr (e text-element))
  (%displace e)
  (draw-text cr (get-text e) 16 (get-x e) (get-y e))
  (setf (get-below-x e)
        (+ (get-x e) (get-w e))))

;;;
;;; Set text bound size method
;;; text-element
;;;
;;; display:block, display:inline, word-wrap: wrap, width: auto, height: auto
(defmethod calc-bound (cr (e text-element))
  (let ((bound (cairo-text-extents cr (get-text e))))
    (setf (get-w e) (* 1.44 (cairo-text-extents-t-width bound))
          (get-h e) (* 1.44 (cairo-text-extents-t-height bound)))))

;;;
;;; Draw text to surface function
;;;
;;; font-size, letter-spacing, color, bold
(defun draw-text (cr text size x y)
  (princ "draw text")
  (cairo-set-source-rgb cr 0.0 0.0 0.0)
  (cairo-select-font-face cr "TakaoP Gothic" :normal :normal)
  (cairo-set-font-size cr size)
  (let ((te (cairo-text-extents cr text)))
    (cairo-move-to cr x 
      (+ y (cairo-text-extents-t-height te)))
    (cairo-show-text cr text)))

(defun first-draw (cr e)
  (print (type-of e))
  (when (typep e 'drawable)
    (calc-bound cr e)
    (draw cr e)
    (when (typep e 'has-children)
      (dolist (child (get-children e))
        (let ((bound (first-draw cr child)))
          (unless (null bound)
            (incf (get-w e) (first bound))  ;; inline 
          )))
      (incf (get-below-x e) (get-w e)))
    (cons (get-w e) (get-h e))))

(defmacro get-style (e rule)
  `(gethash ,rule (style-of ,e)))


;;;;
;;;; Make HTML element class function
;;;;
@export
(defun make-element (expr &key parent previous)
  (cond
    ((null expr) (print "call null") nil)
    ((stringp expr) ;; string
      (make-instance 'text-element :text expr :parent parent :previous previous))
    ((listp expr) ;; s-expr
      (let ((e nil)
            (tag-name (first expr))
            (alist (second expr))
            (clist (third expr))
            (element-type 'container-element))
        (case tag-name
          ((:meta :link)
            (setf element-type 'meta-element))
          (:img
            (setf element-type 'image-element))
          (:head
            (setf element-type 'head-element))
          (:svg
            (setf element-type 'xml-element))
          ((:title :script :style)
            (setf element-type 'data-element))
          (t
            (setf element-type 'container-element)))
        (setf e (make-instance element-type :tag tag-name :parent parent :previous previous))
        ;; Make HTML elements of children recursively
        (when (and (listp clist) (not (null clist)))
          (let ((children (list)))
            (dotimes (i (length clist))
              (push (make-element (nth i clist) :parent e :previous (first children)) children))
            (setf (get-children e) (reverse children))))
        ;; Attatch a-list to attribute of HTML element
        (when (and (listp alist) (not (null alist)))
          (dolist (atr alist)
            (setf (gethash (car atr) (get-attr e)) (cdr atr))))
        e))))

(defun create-main-window ()
  (make-instance 'gtk-window
    :type :toplevel
    :title "Testing Karen Renderer"
    :default-width 640
    :default-height 480))

@export
(defun main (e)
  (let ((surface nil))
    (within-main-loop
      (let (;; Create a toplevel window
          (window (create-main-window)))
      ;; Signal handler for the window to handle the signal "destroy"
        (g-signal-connect window "destroy"
          (lambda (widget)
            (declare (ignore widget))
            (leave-gtk-main)))
        (g-signal-connect window "draw"
          (lambda (widget cr)
            (let ((cr (pointer cr))
              (window (gtk-widget-window widget)))
              (cairo-set-source-rgb cr 1.0 1.0 1.0)
              (cairo-paint cr)
              ;; Set DPI
              (cairo-scale cr
                1.44
                1.44)
              (cairo-set-line-width cr 0.1)
              (first-draw cr e)
              (cairo-destroy cr)
              t)))
        ;; Show the window
        (gtk-widget-show-all window)))))
