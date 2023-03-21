(defproject org.soulspace.clj/math.core "0.8.2"
  :description "The math.core library contains core mathematical functions and algorithms."
  :url "https://github.com/soulspace-org/math.core"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  ; use deps.edn dependencies
  :plugins [[lein-tools-deps "0.4.5"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]}

;  :dependencies [[org.clojure/clojure "1.11.1"]]

  :test-paths ["test"]
  :scm {:name "git" :url "https://github.com/soulspace-org/math.core"}
  :deploy-repositories [["clojars" {:sign-releases false :url "https://clojars.org/repo"}]])
