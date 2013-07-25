(defproject prismatic/schema "0.0.1-SNAPSHOT"
  :description "TBD"

  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :jar-exclusions [#"\.cljx|\.swp|\.swo|\.DS_Store"]

  :plugins [[lein-cljsbuild "0.3.2"] [com.keminglabs/cljx "0.3.0"]]

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [prismatic/cljs-test "0.0.6"]
                 [prismatic/plumbing "0.1.0"]
                 [potemkin "0.3.0-SNAPSHOT"]]

  :cljx {:builds [{:source-paths ["schema/src/cljx"]
                   :output-path "target/generated/clj/src"
                   :rules :clj}

                  {:source-paths ["schema/src/cljx"]
                   :output-path "target/generated/cljs/src"
                   :rules :cljs}

                  {:source-paths ["schema/test/cljx"]
                   :output-path "target/generated/clj/test"
                   :rules :clj}

                  {:source-paths ["schema/test/cljx"]
                   :output-path "target/generated/cljs/test"
                   :rules :cljs}]})
