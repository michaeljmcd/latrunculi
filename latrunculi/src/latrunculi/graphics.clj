(ns latrunculi.graphics
 (:require [taoensso.timbre :as timbre :refer [trace info]])
 (:import (org.lwjgl.glfw GLFW GLFWKeyCallback Callbacks)
          (org.lwjgl.opengl GL GL11)
          (org.lwjgl BufferUtils)
          (org.lwjgl.system MemoryStack)
          (org.lwjgl.stb STBImage)
          (java.nio.channels FileChannel FileChannel$MapMode)
          (java.io File FileInputStream)))

; Valid scenes are: :main-menu

(def global-state (atom 
                   { :current-scene :main-menu }
                   ))

(def textures (atom 0))

(defn- render-menu [current-state]
 (GL11/glBindTexture GL11/GL_TEXTURE_2D (:menu-background @textures))
 (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
 (GL11/glClear GL11/GL_DEPTH_BUFFER_BIT)
 (GL11/glLoadIdentity)

 (GL11/glBegin GL11/GL_QUADS)
    (GL11/glVertex3i -1 1 0) ; top left
    (GL11/glTexCoord2i 1 0)

    (GL11/glVertex3i 1 1 0) ; top right
    (GL11/glTexCoord2i 1 1)

    (GL11/glVertex3i 1 -1 0) ; bottom right
    (GL11/glTexCoord2i 0 1)

    (GL11/glVertex3i -1 -1 0) ; bottom left
    (GL11/glTexCoord2i 0 0)
 (GL11/glEnd)
)

(defn- render-state [current-state]
 (case (:current-scene current-state)
  :main-menu (render-menu current-state)
 ))

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

(defn- load-textures [current-textures]
 { :menu-background (create-texture "img/exekias.bmp") }
)

(defn start []
 (assert (GLFW/glfwInit) "Failed to initialize GLFW.")
 (GLFW/glfwDefaultWindowHints)
 (GLFW/glfwWindowHint GLFW/GLFW_VISIBLE GLFW/GLFW_FALSE)
 (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE GLFW/GLFW_FALSE)
 (GLFW/glfwWindowHint GLFW/GLFW_SAMPLES 4)

 (let [window (GLFW/glfwCreateWindow (int 1024) (int 768) "Latrunculi" (long 0) (long 0))]
  (GLFW/glfwSetCursor window  (GLFW/glfwCreateStandardCursor GLFW/GLFW_ARROW_CURSOR))
  (GLFW/glfwMakeContextCurrent window)
  (GLFW/glfwSwapInterval 0)

  (GLFW/glfwSetKeyCallback window 
   (proxy [GLFWKeyCallback] []
    (invoke [window key scanCode, action, mods]
     (info "Hi")
    )
   ))

  (GLFW/glfwShowWindow window)

  (let [intBuffer (BufferUtils/createIntBuffer 2)]
   (GL/createCapabilities)

   ; Initialize OpenGL
   (GL11/glEnableClientState GL11/GL_VERTEX_ARRAY)

   (GL11/glEnable GL11/GL_TEXTURE_2D)
   (GL11/glClearDepth 1.0)
   (GL11/glEnable GL11/GL_DEPTH_TEST)
   (GL11/glShadeModel GL11/GL_SMOOTH)
   (GL11/glEnable GL11/GL_BLEND)
   (GL11/glHint GL11/GL_PERSPECTIVE_CORRECTION_HINT GL11/GL_NICEST)
   (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA)

   (swap! textures load-textures)

   (while (not (GLFW/glfwWindowShouldClose window))
    (render-state @global-state)

    (GLFW/glfwSwapBuffers window)
    (GLFW/glfwPollEvents)
   )

   (Callbacks/glfwFreeCallbacks window)
   (GLFW/glfwDestroyWindow window)
   (GLFW/glfwTerminate)
  )
 )
)
