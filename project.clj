(defproject org.soulspace.clj/math.core "0.9.0"
  :description "The math.core library contains core mathematical functions and algorithms."
  :url "https://github.com/soulspace-org/math.core"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  ; use deps.edn dependencies
  :plugins [[lein-tools-deps "0.4.5"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]}

  :test-paths ["test"]
  :scm {:name "git" :url "https://github.com/soulspace-org/math.core"}
  :deploy-repositories [["clojars" {:sign-releases false :url "https://clojars.org/repo"}]])
