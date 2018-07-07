(ns latrunculi.graphics
 (:require [taoensso.timbre :as timbre :refer [trace info]]
           [latrunculi.model :as model]
           [latrunculi.resources :as resources])
 (:import (org.lwjgl.glfw GLFW GLFWKeyCallback Callbacks GLFWMouseButtonCallback)
          (org.lwjgl.opengl GL GL11)
          (org.lwjgl BufferUtils)
          ))

; Valid scenes are: :main-menu, :active-game

(def global-state (atom 
                   {:current-scene :main-menu 
                    :game-state nil
                    :camera-settings {:zoom 1.6875
                                      :angle 320
                                      :coordinates [ -0.4 0.0 0.0 ]
                                      :angle-delta 1}}))

(def resources (atom 0))

(defn- third [array] (first (next (next array))))

(defn- render-menu [current-state]
 (GL11/glBindTexture GL11/GL_TEXTURE_2D (-> @resources :textures :menu-background))
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

(defn- render-active-game [current-state]
 (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
 (GL11/glClear GL11/GL_DEPTH_BUFFER_BIT)

 (GL11/glMatrixMode GL11/GL_PROJECTION)
 (GL11/glPushMatrix)
 (GL11/glLoadIdentity)
 (GL11/glOrtho 0 0 0 0 -1 1)
 (GL11/glMatrixMode GL11/GL_MODELVIEW)
 (GL11/glPushMatrix)
 (GL11/glLoadIdentity)

 (GL11/glDisable GL11/GL_TEXTURE_2D)
 (GL11/glColor3i 1 1 0)
 (GL11/glLoadIdentity)

 (GL11/glTranslatef -0.85 0.9 0.0)
 (GL11/glScalef 0.1 0.1 0.1)
 (GL11/glColor3i 0 0 0)

 ; Print text
(GL11/glTranslatef 0.0 -18.0 0.0)
(GL11/glColor3i 0 0 0)
(GL11/glEnable GL11/GL_TEXTURE_2D)

    ; TODO: are these 4 lines useful? do they do anything?
(GL11/glMatrixMode GL11/GL_PROJECTION)
(GL11/glPopMatrix)
(GL11/glMatrixMode GL11/GL_MODELVIEW)
(GL11/glPopMatrix)

(GL11/glMatrixMode GL11/GL_MODELVIEW)
(GL11/glLoadIdentity)

(let [camera-coords (-> current-state :camera-settings :coordinates)]
(GL11/glTranslatef (first camera-coords) (second camera-coords) (third camera-coords)))
; 160
)

(defn- render-state [current-state]
 (case (:current-scene current-state)
  :main-menu (render-menu current-state)
  :active-game (render-active-game current-state)
 ))

(defn- menu-mouse-handler [button]
 (if (= button (GLFW/GLFW_MOUSE_BUTTON_LEFT))
  (swap! global-state assoc :current-scene :active-game)
  )
)

(defn- game-mouse-handler [button]
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

  (GLFW/glfwSetMouseButtonCallback window
   (proxy [GLFWMouseButtonCallback] []
    (invoke [window button action mods]
     (case (:current-scene @global-state)
      :main-menu (menu-mouse-handler button)
      :active-game (game-mouse-handler button)
     )
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

   (swap! resources resources/load-resources)

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
