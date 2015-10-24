(defproject clj-kdtree "1.2.0"
  :description "kd-trees for Clojure"
  :url "https://github.com/abscondment/clj-kdtree"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"
            :distribution :repo}
  :dependencies [[org.clojure/clojure "1.7.0"]]
  :profiles {:dev {:dependencies [[criterium "0.4.3"]
                                  [net.sf/javaml "0.1.7"]]
                   :repositories [["javaml" {:url "http://corp.array.ca/nest-web/mavenrepo/"
                                             :checksum :warn}]]}}
  :plugins [[lein-swank "1.4.4"]])
