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
   { 
     :image (STBImage/stbi_load_from_memory buffer width height comp1 0)
     :width (.get width 0)
     :height (.get height 0)
     :comp (.get comp1 0)
   }
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

(defn load-textures [current-textures]
 { :menu-background (create-texture "img/exekias.bmp") }
)

