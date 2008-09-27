(defsystem latrunculi
    :name "latrunculi"
    :version "0.1"
    :maintainer "Michael McDermott"
    :author "Michael McDermott"
    :licence "GPL-2"
    :description "The game latrunculi"
    :components ((:file "main")
                 (:file "ai"
                        :depends-on ("main" "move"))
                 (:file "copy_obj")
                 (:file "display-lists")
                 (:file "gfx"
                        :depends-on ("display-lists" "main"))
                 (:file "move"
                        :depends-on ("main"))
                 (:file "savegame")
                 )
             )
