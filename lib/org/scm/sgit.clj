(ns org.scm.sgit)

(import '[org.eclipse.jgit.api Git]
        '[org.eclipse.jgit.lib Constants]
        '[org.eclipse.jgit.lib Repository]
        '[org.eclipse.jgit.lib Ref]
        '[org.eclipse.jgit.lib FileMode]
        '[org.eclipse.jgit.util QuotedString]
        '[org.eclipse.jgit.revwalk RevWalk]
        '[org.eclipse.jgit.revwalk RevTree]
        '[org.eclipse.jgit.revwalk RevCommit]
        '[org.eclipse.jgit.treewalk TreeWalk]
        '[java.io File]
        '[java.io FileNotFoundException])

(def git (atom nil))

(defn sgit-init-repo [path]
  "server\\.git"
  (println "init git repo " path)
  (reset! git (Git/open (File. path))))

(defn sgit-list-head-dir [path]
  (let [repo (.getRepository @git)
        head (.exactRef repo Constants/HEAD)
        walk (RevWalk. repo)
        commit (->> head
                    .getObjectId
                    (.parseCommit walk))
        tree (.getTree commit)
        treewalk (TreeWalk/forPath repo path tree)]

    ;; check exist
    (when (nil? treewalk)
      (throw (FileNotFoundException. (str "Did not find expected file '"
                                          path
                                          "' in tree '"
                                          (.getName tree)
                                          "'"))))

    ;; check is tree
    (when (= 0 (bit-and (-> treewalk
                            (.getFileMode 0)
                            .getBits)
                        FileMode/TYPE_TREE))
      (throw (IllegalStateException. (str "Tried to read the elements of a non-tree for commit '"
                                          commit
                                          "' and path '"
                                          path
                                          "', had filemode "
                                          (-> treewalk
                                              (.getFileMode 0)
                                              .getBits)))))

    (let [dirwalk (TreeWalk. repo)]
      (.addTree dirwalk (.getObjectId treewalk 0))
      (.setRecursive dirwalk false)
      (loop [fds []]
        (if-not (.next dirwalk)
          fds
          (let [mode (.getFileMode dirwalk 0)]
            (recur (conj fds
                         {:mode mode ;; 100644 040000
                          :tree (= mode FileMode/TREE) ;; true or false
                          :type (Constants/typeString (.getObjectType mode)) ;; blob or tree
                          :sha1 (-> (.getObjectId dirwalk 0)
                                    .name)
                          ;; unique sha1 value
                          ;; real path under above path path
                          :path (.quote QuotedString/GIT_PATH (.getPathString dirwalk))}))))))))
