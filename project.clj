(defproject ednsl "0.1.0"
  :description "A library for writing DSLs in edn"
  :url "https://github.com/jjl/ednsl/"
  :license {:name "MIT License"
            :url "https://en.wikipedia.org/wiki/MIT_License"
            :distribution :repo}
  :plugins [[lein-midje "3.1.3"]
            [codox "0.8.11"]]
  :source-paths ["src"]
  :test-paths ["t"]
  :clean-targets ^{:protect false} ["target"]
  :deploy-repositories [["releases" :clojars]]
  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version"
                   "leiningen.release/bump-version" "release"]
                  ["doc"]
                  ["vcs" "commit"]
                  ["vcs" "tag"]
                  ["deploy"]]
  :dependencies [[org.clojure/clojure "1.7.0-alpha5"]
                 [org.clojure/tools.reader "0.8.16"]
                 [collectible "0.1.0"]
                 [cats "0.4.0"]]
  :profiles {:dev {:dependencies [[midje "1.6.3"]]}})
