(ns latrunculi.resources
 (:import (java.nio.channels FileChannel FileChannel$MapMode)
        (java.io File FileInputStream)
          (org.lwjgl.system MemoryStack)
          (org.lwjgl.stb STBImage)
          (org.lwjgl.opengl GL11)
 ))

(def ^:const +CUBE-WIDTH+ 0.075) 
(def ^:const +PYRAMID-HEIGHT+ 0.15) 
(def ^:const +PYRAMID-WIDTH+ 0.075)
(def ^:const +SPHERE-RADIUS+ 0.0375)

(defn- load-image [image-path]
 (let [file (File. image-path)]
  (assert (.isFile file) "Texture does not exist.")
  (let [fis (FileInputStream. file)
        channel (.getChannel fis)
        buffer (.map channel FileChannel$MapMode/READ_ONLY 0 (.size channel))
        memoryStack (MemoryStack/stackPush)
        width (.mallocInt memoryStack 1)
        height (.mallocInt memoryStack 1)
        comp1 (.mallocInt memoryStack 1)
  ]
   (.close channel)
   (.close fis)

   (STBImage/stbi_info_from_memory buffer width height comp1)

   (let [result
   { 
     :image (STBImage/stbi_load_from_memory buffer width height comp1 0)
     :width (.get width 0)
     :height (.get height 0)
     :comp (.get comp1 0)
   }]
   (MemoryStack/stackPop)
   result)
 )
))

(defn- create-texture [image-path]
 (let [texture-id (GL11/glGenTextures)
       image-data (load-image image-path)]
  (GL11/glBindTexture GL11/GL_TEXTURE_2D texture-id)
  (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_LINEAR)
  (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_LINEAR)
  ; TODO: comp?

  (GL11/glTexImage2D GL11/GL_TEXTURE_2D
                     0
                     GL11/GL_RGB ; TODO: fixme
                     (:width image-data)
                     (:height image-data)
                     0
                     GL11/GL_RGB ; TODO: fixme
                     GL11/GL_UNSIGNED_BYTE
                     (:image image-data))
  texture-id
 ))

(defn- load-textures []
 {:menu-background (create-texture "img/exekias.bmp")
  :pine (create-texture "img/pine.bmp")
  :white-marble (create-texture "img/white_marble.bmp")
  :black-marble (create-texture "img/granite.bmp")}
)

(defn- create-empty-space-display-list [texture-id list-id]
  (GL11/glNewList list-id GL11/GL_COMPILE)
   (GL11/glBindTexture GL11/GL_TEXTURE_2D texture-id)
   (GL11/glBegin GL11/GL_QUADS)

   ; Face 1:

   (GL11/glTexCoord2f 1.0 0.0)
   (GL11/glVertex3f 0 +CUBE-WIDTH+ +CUBE-WIDTH+)
   ; P_1

   (GL11/glTexCoord2f 0.0 0.0)
   (GL11/glVertex3f 0 +CUBE-WIDTH+ 0)
   ; P_2

   (GL11/glTexCoord2f 1.0 0.0)
   (GL11/glVertex3f 0 0 0)
   ; P_6

   (GL11/glTexCoord2f 1.0 1.0)
   (GL11/glVertex3f 0 0 +CUBE-WIDTH+)
   ; P_5

   ; Face 2:

   (GL11/glTexCoord2f 1.0 0.0)
   (GL11/glVertex3f 0 +CUBE-WIDTH+ 0)
   ; P_2

   (GL11/glTexCoord2f 0.0 0.0)
   (GL11/glVertex3f +CUBE-WIDTH+ +CUBE-WIDTH+ 0)
   ; P_3

   (GL11/glTexCoord2f 0.0 1.0)
   (GL11/glVertex3f +CUBE-WIDTH+ 0 0)
   ; P_7

   (GL11/glTexCoord2f 1.0 1.0)
   (GL11/glVertex3f 0 0 0)
   ; P_6

   ; Face 3:
   (GL11/glTexCoord2f 0.0 0.0)
   (GL11/glVertex3f +CUBE-WIDTH+ +CUBE-WIDTH+ +CUBE-WIDTH+)
   ; P_4

   (GL11/glTexCoord2f 1.0 0.0)
   (GL11/glVertex3f +CUBE-WIDTH+ +CUBE-WIDTH+ 0)
   ; P_3

   (GL11/glTexCoord2f 1.0 1.0)
   (GL11/glVertex3f +CUBE-WIDTH+ 0 0)
   ; P_7

   (GL11/glTexCoord2f 0.0 1.0)
   (GL11/glVertex3f +CUBE-WIDTH+ 0 +CUBE-WIDTH+)
   ; P_8 

   ; Face 4:

   (GL11/glTexCoord2f 0.0 0.0)
   (GL11/glVertex3f 0 +CUBE-WIDTH+ +CUBE-WIDTH+)
   ; P_1

   (GL11/glTexCoord2f 1.0 0.0)
   (GL11/glVertex3f +CUBE-WIDTH+ +CUBE-WIDTH+ +CUBE-WIDTH+)
   ; P_4

   (GL11/glTexCoord2f 1.0 1.0)
   (GL11/glVertex3f +CUBE-WIDTH+ 0 +CUBE-WIDTH+)
   ; P_8 

   (GL11/glTexCoord2f 0.0 1.0)
   (GL11/glVertex3f 0 0 +CUBE-WIDTH+)
   ; P_5

   ; Face 5:

   (GL11/glTexCoord2f 0.0 0.0)
   (GL11/glVertex3f 0 +CUBE-WIDTH+ +CUBE-WIDTH+)
   ; P_1
   
   (GL11/glTexCoord2f 1.0 0.0)
   (GL11/glVertex3f 0 +CUBE-WIDTH+ 0)
   ; P_2

   (GL11/glTexCoord2f 1.0 1.0)
   (GL11/glVertex3f +CUBE-WIDTH+ +CUBE-WIDTH+ 0)
   ; P_3

   (GL11/glTexCoord2f 0.0 1.0)
   (GL11/glVertex3f +CUBE-WIDTH+ +CUBE-WIDTH+ +CUBE-WIDTH+)
   ; P_4
   ; Face 6:

   (GL11/glTexCoord2f 0.0 1.0)
   (GL11/glVertex3f +CUBE-WIDTH+ 0 +CUBE-WIDTH+)
   ; P_8 

   (GL11/glTexCoord2f 1.0 1.0)
   (GL11/glVertex3f +CUBE-WIDTH+ 0 0)
   ; P_7

   (GL11/glTexCoord2f 1.0 0.0)
   (GL11/glVertex3f 0 0 0)
   ; P_6

   (GL11/glTexCoord2f 0.0 0.0)
   (GL11/glVertex3f 0 0 +CUBE-WIDTH+)
   ; P_5
   (GL11/glEnd) 
  (GL11/glEndList)
    list-id
 )

(defn- create-king-display-list [texture-id list-id]
   (GL11/glNewList list-id GL11/GL_COMPILE)
   (GL11/glBindTexture GL11/GL_TEXTURE_2D texture-id)

   (GL11/glBegin GL11/GL_TRIANGLES)
    (GL11/glTexCoord2f -0.5 0.5)
    (GL11/glVertex3f (* -0.5 +PYRAMID-WIDTH+)
         0
         (* 0.5 +PYRAMID-WIDTH+))

    (GL11/glTexCoord2f 0.5 1.0)
    (GL11/glVertex3f 0
         +PYRAMID-HEIGHT+
         0)

    (GL11/glTexCoord2f 1.0 1.0)
    (GL11/glVertex3f (* 0.5 +PYRAMID-WIDTH+)
         0
         (* 0.5 +PYRAMID-WIDTH+))
    ; first face.

    (GL11/glTexCoord2f 0.0 0.0)
    (GL11/glVertex3f (* 0.5 +PYRAMID-WIDTH+)
         0
         (* 0.5 +PYRAMID-WIDTH+))

    (GL11/glTexCoord2f 1.0 0.0)
    (GL11/glVertex3f 0.0
         +PYRAMID-HEIGHT+
         0.0)

    (GL11/glTexCoord2f 1.0 1.0)
    (GL11/glVertex3f (* 0.5 +PYRAMID-WIDTH+)
         0
         (* -0.5 +PYRAMID-WIDTH+))
    ; second.

    (GL11/glTexCoord2f 0.0 0.0)
    (GL11/glVertex3f (* 0.5 +PYRAMID-WIDTH+)
         0
         (* -0.5 +PYRAMID-WIDTH+)) ; pt. 4

    (GL11/glTexCoord2f 1.0 0.0)
    (GL11/glVertex3f 0.0
         +PYRAMID-HEIGHT+
         0.0) ; pt. 2

    (GL11/glTexCoord2f 1.0 1.0)
    (GL11/glVertex3f (* -0.5 +PYRAMID-WIDTH+)
         0
         (* -0.5 +PYRAMID-WIDTH+)) ; pt. 5
    ; third.

    (GL11/glTexCoord2f 0.0 1.0)
    (GL11/glVertex3f 0.0
         +PYRAMID-HEIGHT+
         0.0) ; pt. 2

    (GL11/glTexCoord2f 1.0 0.0)
    (GL11/glVertex3f (* -0.5 +PYRAMID-WIDTH+)
         0
         (* -0.5 +PYRAMID-WIDTH+)) ; pt. 5

    (GL11/glTexCoord2f 0.0 0.0)
    (GL11/glVertex3f (* -0.5 +PYRAMID-WIDTH+)
         0
         (* 0.5 +PYRAMID-WIDTH+))
        ; fourth.
    (GL11/glEnd)
(GL11/glEndList)
    list-id
)

; TODO: reimplement this.
(defn- create-pawn-display-list [texture-id list-id]
 ; White pawn
 (GL11/glNewList list-id GL11/GL_COMPILE)
  (GL11/glBindTexture GL11/GL_TEXTURE_2D texture-id)
       ;(glu:Sphere quad +SPHERE-RADIUS+ 10 10)
 (GL11/glEndList)
 list-id
)

; We are going to create display lists for each of the following possibilities:
; 1. Empty space
; 2. White king
; 3. Black king
; 4. White pawn
; 5. Black pawn
(defn- create-display-lists [textures]
 (let [list-start (GL11/glGenLists 5)]
  {:empty-space (create-empty-space-display-list (-> textures :pine) list-start)
   :white-king (create-king-display-list (-> textures :white-marble) (+ list-start 1))
   :black-king (create-king-display-list (-> textures :black-marble) (+ list-start 2))
   :white-pawn (create-empty-space-display-list (-> textures :white-marble) (+ list-start 3))
   :black-pawn (create-empty-space-display-list (-> textures :black-marble) (+ list-start 4))}
 ))

(defn load-resources [current-resources]
 (let [textures (load-textures)]
 {:textures textures :display-lists (create-display-lists textures)}
 ))
