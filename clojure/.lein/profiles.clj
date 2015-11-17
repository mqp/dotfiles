;; ~/.lein/profiles.clj
{:user {:plugins [[lein-pprint "1.1.1"]
                  [cider/cider-nrepl "0.10.0-SNAPSHOT"]
                  [clojurewerkz/elastisch "2.1.0"]]
        :dependencies [[org.clojure/tools.nrepl "0.2.12"]]
       :injections [(use 'clojure.repl)
                    (use 'clojure.java.javadoc)
                    (use 'clojure.pprint)] }}
