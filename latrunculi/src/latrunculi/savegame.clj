(ns latrunculi.savegame
  (:require [clojure.edn :as e]))

(defn save-game [filename board turn player0 player1]
 (spit filename [board turn player0 player1]))

(defn load-game [filename]
 (e/read-string (slurp filename)))
