;; (ns clojuratica.lib.debug)

;; (defmacro with-debug-message [bool msg & body]
;;  `(if-not ~bool
;;     (do ~@body)
;;     (do
;;       (println "Starting" (str ~msg "..."))
;;       (let [result# (do ~@body)]
;;         (println "Done" (str ~msg "."))
;;         result#))))

;; ;; TODO This function should really be moved to a utils ns
;; (defn third [coll] (nth coll 2))
