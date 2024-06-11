(ns geocoder.db
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [malli.core :as m]
            [xtdb.api :as xt]
            [xtdb.jdbc]
            [xtdb.jdbc.psql]))

(defonce db-spec
  (let [rtc (fn runtime-spec-check [{:keys [engine jdbc-uri document-store tx-log]}]
              (let [ok? #(boolean (re-matches % jdbc-uri))]
                (case engine
                  :rocksdb (and (string? document-store) (string? tx-log))
                  :jdbc/sqlite (or (ok? #"(jdbc.*):(.*).db")
                                   (ok? #"(jdbc.*):\d+[;:/](.*)"))
                  (ok? #"(jdbc.*):\d+[;:/](.*)"))))
        rtce {:error/message
              "- JDBC uri is required if the engine is set to any jdbc db.
               - For any KV db such as rocksdb :document-store and :tx-log is required
               - References:
               - xtdb_docs: https://v1-docs.xtdb.com/administration/configuring/#_storage
               - jdbc_docs: https://en.wikibooks.org/wiki/Java_JDBC_using_SQLite/Connecting"}]
    [:and
     [:map {:closed false}
      [:engine [:enum :rocksdb :jdbc :jdbc/psql :jdbc/sqlite]]
      [:http-port {:optional true} :int]
      [:index-store :string]
      [:lucene-store :string]]
     [:fn rtce rtc]]))

(defn start! [conf]
  (when (m/validate db-spec conf)
    (println "Starting Database")
    (pp/pprint conf)
    (println "Successfully connected to Database\n")
    (let [opts (fn jdbc-opts
                 [dialect]
                 {:xtdb/query-engine         {:query-timeout (or (:query-timeout conf) 300000)}
                  :xtdb.jdbc/connection-pool {:dialect {:xtdb/module dialect}
                                              :pool-opts {}
                                              :db-spec   {:jdbcUrl (:jdbc-uri conf)}}
                  :xtdb/tx-log               {:xtdb/module     'xtdb.jdbc/->tx-log
                                              :connection-pool :xtdb.jdbc/connection-pool}
                  :xtdb/document-store       {:xtdb/module     'xtdb.jdbc/->document-store
                                              :connection-pool :xtdb.jdbc/connection-pool}
                  :xtdb/index-store          {:kv-store {:xtdb/module 'xtdb.rocksdb/->kv-store
                                                         :db-dir      (io/file (:index-store conf))}}
                  ;; :xtdb.http-server/server   {:port (or (:http-port conf) 3000)}
                  :xtdb.lucene/lucene-store  {:db-dir (:lucene-store conf)}})]
      (case (:engine conf)
        :jdbc (xt/start-node (opts 'xtdb.jdbc.psql/->dialect))
        :jdbc/psql (xt/start-node (opts 'xtdb.jdbc.psql/->dialect))
        :jdbc/sqlite (xt/start-node (opts 'xtdb.jdbc.sqlite/->dialect))
        :rocksdb (letfn [(kv-store [dir]
                           {:kv-store {:xtdb/module 'xtdb.rocksdb/->kv-store
                                       :db-dir      (io/file dir)
                                       :sync?       true}})]
                   (xt/start-node
                    {:xtdb/tx-log              (kv-store (:tx-log conf))
                     :xtdb/document-store      (kv-store (:document-store conf))
                     :xtdb/index-store         (kv-store (:index-store conf))
                     :xtdb.http-server/server  {:port (or (:http-port conf) 3000)}
                     :xtdb.lucene/lucene-store {:db-dir (:lucene-store conf)}}))
        nil))))