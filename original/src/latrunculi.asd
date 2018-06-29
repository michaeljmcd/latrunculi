(defsystem latrunculi
    :name "latrunculi"
    :version "0.1"
    :maintainer "Michael McDermott"
    :author "Michael McDermott"
    :licence "GPL-2"
    :description "The game latrunculi"
    :depends-on (cffi lispbuilder-sdl cl-opengl cl-glu)
    :components ((:file "main")
                 (:file "ai" :depends-on ("main" "move"))
                 (:file "copy_obj" :depends-on ("main"))
                 (:file "display-lists" :depends-on ("main"))
                 (:file "gfx" :depends-on ("display-lists" "main" "ai"))
                 (:file "savegame" :depends-on ("main"))
                 (:file "move" :depends-on ("main" "copy_obj"))
                 ))
