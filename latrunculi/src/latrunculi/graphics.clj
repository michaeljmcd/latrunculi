(ns latrunculi.graphics
 (:require [taoensso.timbre :as timbre :refer [trace info]]
           [latrunculi.model :as m]
           [latrunculi.resources :as r])
 (:import (org.lwjgl.glfw GLFW GLFWKeyCallback Callbacks GLFWMouseButtonCallback)
          (org.lwjgl.opengl GL GL11)
          (org.lwjgl BufferUtils)
          (org.lwjgl.system MemoryStack)
          ))

; Valid scenes are: :main-menu, :active-game

(defn- create-default-global-state [s]
{:current-scene :main-menu 
                    :game-state nil
                    :move-origin nil
                    :camera-settings {:zoom 1.6875
                                      :angle 320
                                      :coordinates [ -0.4 0.0 0.0 ]
                                      :angle-delta 1}
                    :default-background-color [0.80 0.68 0.38 0]})

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

(defn- get-mouse-position [window]
 (let [stack (MemoryStack/stackPush)
       xBuff (.mallocDouble stack 1)
       yBuff (.mallocDouble stack 1)]
  (GLFW/glfwGetCursorPos window xBuff yBuff)
   (let [result [(.get xBuff 0) (.get yBuff 0)]]
    (MemoryStack/stackPop)
    result
   )
 )
)

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

(defn- clear-with-default-color [current-state]
 (let [color (:default-background-color current-state)]
   (GL11/glClearColor (get color 0) (get color 1) (get color 2) (get color 3))))

(defn- render-active-game [window current-state resources]
 (clear-with-default-color current-state)

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

(defn- initialize-game []
  (swap! global-state 
         (fn [s]
           (-> s
               (assoc :current-scene :active-game)
               (assoc :game-state (m/create-default-game-state))
           )))

 (GL11/glDepthFunc GL11/GL_LESS)
 (GL11/glMatrixMode GL11/GL_PROJECTION)

 (let [zoom (-> @global-state :camera-settings :zoom)]
 (GL11/glScalef zoom zoom zoom))

 (let [angle (-> @global-state :camera-settings :angle)]
  (GL11/glRotatef angle 1.0 0.0 0.0))
)

(defn- menu-mouse-handler [button]
 (if (= button (GLFW/GLFW_MOUSE_BUTTON_LEFT))
   (initialize-game)
  )
)

(defn- color-render [window state resources]
 (GL11/glDisable GL11/GL_TEXTURE_2D)
 (GL11/glDisable GL11/GL_DITHER)
 (GL11/glDisable GL11/GL_LIGHTING)
 (GL11/glDisable GL11/GL_BLEND)

 ;(GL11/glClearColor 255 255 255 0)
 (GL11/glClearColor 1.0 1.0 1.0 0)
 (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))

 (GL11/glMatrixMode GL11/GL_MODELVIEW)
 (GL11/glLoadIdentity)

 (GL11/glTranslatef (get-in state [:camera-settings :coordinates 0])
                    (get-in state [:camera-settings :coordinates 1])
                    (get-in state [:camera-settings :coordinates 2]))

  (doseq [x (range m/+ROWS+)]
    (doseq [y (range m/+COLUMNS+)]
    (GL11/glColor4f (float (/ x 255))
                    (float (/ y 255))
                    0
                    0)

      (at-offset [(* -0.5 r/+CUBE-WIDTH+)
                  (* -0.5 r/+CUBE-WIDTH+)
                  (* -0.5 r/+CUBE-WIDTH+)]
         (GL11/glCallList (-> resources :display-lists :empty-space)))
 
     (let [piece (get-in state [:game-state :board x y])]
      (render-piece piece resources)
     )
     (GL11/glTranslatef r/+CUBE-WIDTH+ 0.0 0.0)
   )
    (GL11/glTranslatef (* -1 m/+COLUMNS+ r/+CUBE-WIDTH+) 0.0 (* -1.0 r/+CUBE-WIDTH+))
   )

 (GL11/glFlush)
 (clear-with-default-color state)
)

(defn- mouse-coordinates-empty? [coordinates]
 (= (get coordinates 0) -1))

(defn- process-move-input [state move]
 (info "Going to do stuff about move " move)
 (if (or (m/move-valid? (get-in state [:game-state :board]) move m/+BLACK+)
         (m/move-valid? (get-in state [:game-state :board]) move m/+WHITE+)) ; TODO: fix this; for testing.
  true
  false
 ))

(defn- game-mouse-handler [window button action position state]
 (if (= action GLFW/GLFW_RELEASE)
  (do
 (info "Click at " position)
  (color-render window state @resources)

  (let [stack (MemoryStack/stackPush)
        viewport-buffer (.mallocInt stack 4)
        pixel-buffer (.malloc stack 4)]
    (GL11/glGetIntegerv GL11/GL_VIEWPORT viewport-buffer)

    (info "Reading pixels at " [(int (first position))
                   (int (- (.get viewport-buffer 3) (second position)))
    ])

    (GL11/glReadPixels (int (first position))
                   (int (- (.get viewport-buffer 3) (second position)))
                   (int 1)
                   (int 1 )
                   GL11/GL_RGBA 
                   GL11/GL_UNSIGNED_BYTE
                   pixel-buffer)

    (let [pixel [(.get pixel-buffer 0) (.get pixel-buffer 1) (.get pixel-buffer 2) (.get pixel-buffer 3)]
          coordinates [(second pixel) (first pixel)]
          viewport [(.get viewport-buffer 0) (.get viewport-buffer 1) (.get viewport-buffer 2) (.get viewport-buffer 3)]]
          ; TODO: make these trace later
   (info "viewport: " viewport)
   (info "pixel: " pixel)
   (info "coordinates:  " coordinates)
     (if (not (mouse-coordinates-empty? coordinates))
      (if (nil? (:move-origin state))
       (swap! global-state (fn [s] (assoc-in s [:move-origin] coordinates)))
       (process-move-input state [(:move-origin state) coordinates])
      )
      (info "Clicked empty space")
     )
   )
    (MemoryStack/stackPop)
  )
    (GL11/glEnable GL11/GL_TEXTURE_2D)
    (GL11/glEnable GL11/GL_DITHER)
    (GL11/glEnable GL11/GL_BLEND)
 )
 nil
 ))

(defn- create-camera-pan-fn [idx delta]
    (fn [s] 
              (let [coords (-> s :camera-settings :coordinates)]
                (assoc-in s [:camera-settings :coordinates idx]
                            (+ delta (first coords)))
             )))

(defn- create-camera-zoom-update-fn [delta]
 (fn [s]
  (GL11/glMatrixMode GL11/GL_PROJECTION)
  (GL11/glScalef delta delta delta)
  (let [zoom (-> s :camera-settings :zoom)]
   (assoc-in s [:camera-settings :zoom]
               (* zoom delta))
  )))

(defn- game-keyboard-handler [key]
 (swap! global-state
     (cond 
      (= key GLFW/GLFW_KEY_RIGHT) (create-camera-pan-fn 0 0.01)
      (= key GLFW/GLFW_KEY_LEFT) (create-camera-pan-fn 0 -0.01)
      (= key GLFW/GLFW_KEY_UP) (create-camera-pan-fn 1 0.01)
      (= key GLFW/GLFW_KEY_DOWN) (create-camera-pan-fn 1 -0.01)
      (= key GLFW/GLFW_KEY_PAGE_UP) (create-camera-zoom-update-fn 1.05)
      (= key GLFW/GLFW_KEY_PAGE_DOWN) (create-camera-zoom-update-fn 0.95)
      (= key GLFW/GLFW_KEY_HOME) (fn [s]
                                    (let [delta (-> s :camera-settings :angle-delta)
                                          angle (-> s :camera-settings :angle)]
                                    (GL11/glMatrixMode GL11/GL_PROJECTION)
                                    (GL11/glRotatef delta 0.0 1.0 0.0)
                                    (assoc-in s [:camera-settings :angle]
                                       (mod (+ angle delta) 360))))
      (= key GLFW/GLFW_KEY_END) (fn [s]
                                    (let [delta (-> s :camera-settings :angle-delta)
                                          angle (-> s :camera-settings :angle)]
                                    (GL11/glMatrixMode GL11/GL_PROJECTION)
                                    (GL11/glRotatef (* -1 delta) 0.0 1.0 0.0)
                                    (assoc-in s [:camera-settings :angle]
                                       (mod (- angle delta) 360))))
      (= key GLFW/GLFW_KEY_INSERT) (fn [s]
                                    (let [delta (-> s :camera-settings :angle-delta)
                                          angle (-> s :camera-settings :angle)]
                                    (GL11/glMatrixMode GL11/GL_PROJECTION)
                                    (GL11/glRotatef delta 1.0 0.0 0.0)
                                    (assoc-in s [:camera-settings :angle]
                                       (mod (+ angle delta) 360))))

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
      :active-game (game-mouse-handler window button action (get-mouse-position window) @global-state)
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
   (initialize-game)

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
