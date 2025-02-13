(ns org.scm.scmd)

(require '[org.scm.ssrv :refer :all]
         '[org.scm.sgit :refer :all])

(defcmd dir [client & args]
  "dir /some/path or just the command dir with no arguments
git ls-tree HEAD <path>|<empty>"
  (let [dirs (if (nil? args)
               (sgit-list-root)
               (sgit-list-head-dir (first args)))]
    (send-cmd client dirs)))
