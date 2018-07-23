(ns latrunculi.graphics
 (:require [taoensso.timbre :as timbre :refer [trace info]]
           [latrunculi.model :as m]
           [latrunculi.resources :as r])
 (:import (org.lwjgl.glfw GLFW GLFWKeyCallback Callbacks GLFWMouseButtonCallback)
          (org.lwjgl.opengl GL GL11)
          (org.lwjgl BufferUtils)
          ))

; Valid scenes are: :main-menu, :active-game

(defn- create-default-global-state [s]
{:current-scene :main-menu 
                    :game-state nil
                    :camera-settings {:zoom 1.6875
                                      :angle 320
                                      :coordinates [ -0.4 0.0 0.0 ]
                                      :angle-delta 1}})

(def global-state (atom 0))
(def resources (atom 0))

(defn- third [array] (first (next (next array))))

(defmacro at-offset [coordinates & operations]
 (let [x (first coordinates)
       y (second coordinates)
       z (third coordinates)]
`(do
        (GL11/glTranslatef ~x ~y ~z)
        ~@operations
        (GL11/glTranslatef (* -1 ~x) (* -1 ~y) (* -1 ~z))
    )
))

(defn- render-menu [current-state]
 (GL11/glBindTexture GL11/GL_TEXTURE_2D (-> @resources :textures :menu-background))
 (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
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

(defn- render-piece [cell resources]
    (if (not (= cell m/+EMPTY+))
        (at-offset (0.0 (* 0.5 r/+CUBE-WIDTH+) 0.0)
          (cond
           (= cell m/+WHITE_KING+) (GL11/glCallList (-> resources :display-lists :white-king))
           (= cell m/+BLACK_KING+) (GL11/glCallList (-> resources :display-lists :black-king))
           (= cell m/+WHITE_PAWN+) (do 
                            (GL11/glTranslatef 0.0 (* 0.25 r/+CUBE-WIDTH+) 0.0)
                            (GL11/glCallList (-> resources :display-lists :white-pawn))
                            (GL11/glTranslatef 0.0 (* -0.25 r/+CUBE-WIDTH+) 0.0))
           (= cell m/+BLACK_PAWN+) (do
             (GL11/glTranslatef 0.0 (* 0.25 r/+CUBE-WIDTH+) 0.0)
             (GL11/glCallList (-> resources :display-lists :black-pawn))
             (GL11/glTranslatef 0.0 (* -0.25 r/+CUBE-WIDTH+) 0.0))))
))

(defn- render-active-game [window current-state resources]
 (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))

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
 (doseq [row (-> current-state :game-state :board)]
   (doseq [cell row]
    (at-offset [(* -0.5 r/+CUBE-WIDTH+)
                (* -0.5 r/+CUBE-WIDTH+)
                (* -0.5 r/+CUBE-WIDTH+)]
               (GL11/glCallList (-> resources :display-lists :empty-space)))
    ; 168
    (render-piece cell resources)

    (GL11/glTranslatef r/+CUBE-WIDTH+ 0.0 0.0)
    (GL11/glColor4f 1.0 1.0 1.0 1.0)
   )
   (GL11/glTranslatef (* -1 m/+COLUMNS+ r/+CUBE-WIDTH+) 0.0 (* -1.0 r/+CUBE-WIDTH+))
 )
)

(defn- render-state [window current-state resources]
 (case (:current-scene current-state)
  :main-menu (render-menu current-state)
  :active-game (render-active-game window current-state resources)
 ))

(defn- initialize-game [s]
 (GL11/glDepthFunc GL11/GL_LESS)
 (GL11/glMatrixMode GL11/GL_PROJECTION)

 (let [zoom (-> s :camera-settings :zoom)]
 (GL11/glScalef zoom zoom zoom))

 (let [angle (-> s :camera-settings :angle)]
  (GL11/glRotatef angle 1.0 0.0 0.0))

 (GL11/glClearColor 0.80 0.68 0.38 0)
)

(defn- menu-mouse-handler [button]
 (if (= button (GLFW/GLFW_MOUSE_BUTTON_LEFT))
  (swap! global-state 
         (fn [s]
           (-> s
               (assoc :current-scene :active-game)
               (assoc :game-state (m/create-default-game-state))
           )))
   (initialize-game @global-state)
  )
)

(defn- game-mouse-handler [button]
)

(defn- create-camera-pan-fn [delta]
    (fn [s] 
              (let [coords (-> s :camera-settings :coordinates)]
                (assoc-in s [:camera-settings :coordinates 0]
                            (+ delta (first coords)))
             )))

(defn- game-keyboard-handler [key]
 (swap! global-state
     (cond 
      (= key GLFW/GLFW_KEY_RIGHT) (create-camera-pan-fn 0.01)
      (= key GLFW/GLFW_KEY_LEFT) (create-camera-pan-fn -0.01)
        )
 ))

(defn start []
 (swap! global-state create-default-global-state)
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
    (invoke [window key scanCode action mods]
     (case (:current-scene @global-state)
        :active-game (game-keyboard-handler key)
     )
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

   (swap! resources r/load-resources)
   (info "resources: " @resources)

   (while (not (GLFW/glfwWindowShouldClose window))
    (render-state window @global-state @resources)

    (GLFW/glfwSwapBuffers window)
    (GLFW/glfwPollEvents)
   )

   (Callbacks/glfwFreeCallbacks window)
   (GLFW/glfwDestroyWindow window)
   (GLFW/glfwTerminate)
  )
 )
)
