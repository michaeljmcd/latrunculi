(ns latrunculi.graphics
 (:import (org.lwjgl.glfw GLFW)))

(defn start []
 (GLFW/glfwInit)
 (GLFW/glfwDefaultWindowHints)
 (GLFW/glfwWindowHint GLFW/GLFW_VISIBLE GLFW/GLFW_FALSE)
 (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE GLFW/GLFW_FALSE)
 (GLFW/glfwWindowHint GLFW/GLFW_SAMPLES 4)

 (let [window (GLFW/glfwCreateWindow (int 1024) (int 768) "ASDF" (long 0) (long 0))]
  (GLFW/glfwShowWindow window)
 )
)
