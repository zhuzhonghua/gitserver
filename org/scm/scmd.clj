(ns org.scm.scmd)

(require '[org.scm.ssrv :refer :all]
         '[org.scm.sgit :refer :all])

(defcmd "gitdir" [client cmd]
  "gitdir /some/path"
  (let [path (nth cmd 1)
        dirs (sgit-list-head-dir path)]
    (send-cmd client dirs)))
