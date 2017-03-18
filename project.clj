(defproject evolution "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"] [org.clojure/math.numeric-tower "0.0.4"]  [net.mikera/core.matrix.stats "0.7.0"] [incanter "1.5.7"] [org.clojure/tools.trace "0.7.9"] [aysylu/loom "1.0.0"] [org.clojure/core.memoize "0.5.8"] [com.taoensso/tufte "1.1.1"]]
  :main ^:skip-aot evolution.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
