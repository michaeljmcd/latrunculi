(defpackage #:glu
    (:use #:common-lisp)
    (:export #:new-quadric)
    (:export #:quadric-normals)
    (:export #:quadric-texture)
    (:export #:quadric-orientation))

(in-package #:glu)

(cffi:define-foreign-library GL
   (:darwin (:framework "OpenGL"))
   (:windows "OPENGL32.dll")
   (:unix (:or "libGL" "libGL.so.2" "libGL.so.1")))

(cffi:use-foreign-library GL)

#| Working from the following definition, yanked from the mesa source base:
struct GLUquadric {
GLint       normals;
GLboolean   textureCoords;
GLint       orientation;
GLint       drawStyle;
void        (GLAPIENTRY *errorCallback)( GLint );
};|#

(cffi:defcstruct GLUquadric 
    (normals :int)
    (textureCoords :unsigned-char)
    (orientation :int)
    (drawStyle :int)
    (errorCallback :pointer))

(defconstant GLU_NONE 100002)
(defconstant GLU_FLAT 100001)
(defconstant GLU_SMOOTH 100000)
(defconstant GLU_OUTSIDE 100020)
(defconstant GLU_INSIDE 100021)

(cffi:defcfun ("gluNewQuadric" new-quadric) :pointer)
(cffi:defcfun ("gluQuadricNormals" quadric-normals) :void
    (quadObject :pointer) (normals :unsigned-int))
(cffi:defcfun ("gluQuadricTexture" quadric-normals) :void
    (quadObject :pointer) (textureCoords :unsigned-char))
(cffi:defcfun ("gluQuadricOrientation" quadric-orientation) :void
    (quadObject :pointer) (orientation :unsigned-int))
