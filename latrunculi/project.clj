(defproject latrunculi "0.1.0-SNAPSHOT"
  :description "The game of Latrunculi"
  :url "https://github.com/michaeljmcd/latrunculi"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies 
      [[org.clojure/clojure "1.8.0"]
       [com.taoensso/timbre "4.10.0"]
       [org.lwjgl/lwjgl "3.1.6"]
       [org.lwjgl/lwjgl-stb "3.1.6"]
       [org.lwjgl/lwjgl-glfw "3.1.6"]
       [org.lwjgl/lwjgl-opengl "3.1.6"]]
  :jvm-opts ["-Djava.library.path=lib/winx64"]
  :main ^:skip-aot latrunculi.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
