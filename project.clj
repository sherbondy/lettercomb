(defproject lettercomb "0.0.1"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2138"]]
  :plugins [[lein-cljsbuild "1.0.1"]]
  :cljsbuild {
    :builds [{
              :source-paths ["src"]
              :compiler {
                         :output-to "index.js"
                         :optimizations :whitespace
                         :source-map "index.map.js"}}]})