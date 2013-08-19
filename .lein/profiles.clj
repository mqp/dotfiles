;; ~/.lein/profiles.clj
{:user {:plugins [[lein-pprint "1.1.1"]]
       :injections [(use 'clojure.repl)
                    (use 'clojure.java.javadoc)
                    (use 'clojure.pprint)] }}
