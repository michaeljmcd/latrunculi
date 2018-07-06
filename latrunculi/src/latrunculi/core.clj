(ns latrunculi.core
  (:require [latrunculi.graphics :as gfx]
            [taoensso.timbre :as timbre :refer [trace info with-level]])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (with-level :trace
  (gfx/start)
  )
  )
