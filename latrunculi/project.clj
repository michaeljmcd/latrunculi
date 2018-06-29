(defproject latrunculi "0.1.0-SNAPSHOT"
  :description "The game of Latrunculi"
  :url "https://github.com/michaeljmcd/latrunculi"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot latrunculi.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
