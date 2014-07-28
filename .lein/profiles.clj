;; ~/.lein/profiles.clj
{:user {:plugins [[lein-pprint "1.1.1"]
                  [cider/cider-nrepl "0.7.0-SNAPSHOT"]]
       :injections [(use 'clojure.repl)
                    (use 'clojure.java.javadoc)
                    (use 'clojure.pprint)] }}
