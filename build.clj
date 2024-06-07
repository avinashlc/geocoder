(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.build.api :as b]))

(def build-folder "target")
(def jar-content (str build-folder "/classes"))

(def main 'geocoder.core)
(def !basis (delay (b/create-basis {:project "deps.edn"})))
(def version "0.0.1")
(def app-name "geocoder")
(def uber-file-name (format "%s/%s-%s-standalone.jar" build-folder app-name version)) ; path for result uber file

(defn test "Run all the tests." [opts]
  (let [basis    (b/create-basis {:aliases [:test]})
        cmds     (b/java-command
                  {:basis     basis
                   :main      'clojure.main
                   :main-args ["-m" "cognitect.test-runner"]})
        {:keys [exit]} (b/process cmds)]
    (when-not (zero? exit) (throw (ex-info "Tests failed" {}))))
  opts)

(defn clean [_]
  (b/delete {:path "target"})
  (println (format "Build folder \"%s\" removed" build-folder)))

(defn uber [_]
  (clean nil)

  (b/copy-dir {:src-dirs   ["resources" "src" "config"]         ; copy resources
               :target-dir jar-content})

  (b/compile-clj {:basis      @!basis               ; compile clojure code
                  :src-dirs   ["src"]
                  :ns-compile [main]
                  :class-dir  jar-content})

  (b/uber {:class-dir jar-content                ; create uber file
           :uber-file uber-file-name
           :basis     @!basis
           :main      main})                ; here we specify the entry point for uberjar

  (println "\n" (format "Uber file created: \"%s\"" uber-file-name)))