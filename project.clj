(defproject ragavardhini "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [overtone "0.9.1"]
                 [leipzig "0.8.1"]
                 [org.clojure/java.jdbc "0.3.7"]
                 [postgresql/postgresql "9.3-1101.jdbc4"]
                 [medley "0.5.5"]
                 [cheshire "5.4.0"]
                 [incanter "1.5.7"]
                 [hiccup "1.0.5"]
                 [bidi "1.19.0"]
                 [clj-http "1.1.2"]
                 [enlive "1.1.5"]
                 [org.clojure/tools.cli "0.3.1"]
                 [org.clojure/tools.nrepl "0.2.6"]
                 [ring/ring-core "1.3.2"]
                 [ring/ring-jetty-adapter "1.3.2"]
                 [ring-middleware-format "0.3.2" :exclusions [ring]]
                 [rm-hull/markov-chains "0.0.1"]]
  :plugins [[cider/cider-nrepl "0.8.2"]]
  :jvm-opts ["-Xms4g" "-Xmx4g"
             "-Dcom.sun.management.jmxremote=true"
             "-Dcom.sun.management.jmxremote.port=12345"
             "-Dcom.sun.management.jmxremote.authenticate=false"])
