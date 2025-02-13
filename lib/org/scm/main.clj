(ns org.scm.main)

(require '[org.scm.ssrv :as ssrv])

(defn -main []
  (let [path "D:\\tmp\\gittest\\server\\.git"
        port 1666]
    (ssrv/init port path)))
