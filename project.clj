(defproject evok "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [speclj "2.4.0"]]
  :aliases {"spec!" ["spec" "-a"]}
  :plugins [[speclj "2.4.0"]]
  :test-paths ["spec/"]
  
  ;; Fix problem with stack traces disappearing. See
  ;; https://github.com/technomancy/leiningen/issues/1025
  :jvm-opts ["-XX:TieredStopAtLevel=1"]
  
  :main evok.core)
