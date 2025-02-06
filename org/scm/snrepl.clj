(ns org.scm.snrepl)

(require '[nrepl.server :as nrepl]
         '[clojure.java.io :as io])

(import '[java.net ServerSocket])

(def port-file-name ".nrepl-port")

(defn random-available-port []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))

(defn delete-port-file []
  (let [file (io/file port-file-name)]
    (when (.exists file)
      (io/delete-file file))))

(defn init []
  (let [np (random-available-port)]
    (spit port-file-name np)
    (Thread/sleep 1000)
    (defonce server (nrepl/start-server :port np))
    (println "nrepl listen on port" np)))

(defn deinit []
  (delete-port-file)
  (println "nrepl delete port file" port-file-name))
