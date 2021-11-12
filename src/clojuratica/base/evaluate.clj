(ns clojuratica.base.evaluate
  (:require [clojuratica.jlink]
            [clojuratica.lib.options :as options]
            [clojuratica.base.convert :as convert]))

(declare evaluate)
(defn process-state [pid-expr {:keys [flags] :as opts}]
  (assert (options/flag?' flags :serial))
  (let [state-expr    (evaluate (convert/convert (list 'ProcessState pid-expr) opts) opts)
        state-prefix  (first (.toString state-expr))]
    (cond (= \r state-prefix) [:running nil]
          (= \f state-prefix) [:finished (.part state-expr 1)]
          (= \q state-prefix) [:queued nil]
          :else
          (throw (Exception. (str "Error! State unrecognized: " state-expr))))))

(defn queue-run-or-wait [{:keys [flags config] :as opts}]
  (assert (options/flag?' flags :serial))
  (let [lqr-atom (atom nil)
        lqr-time @lqr-atom
        nano-pi  (* 1000000 (:poll-interval config))
        run-in   (when lqr-time (- (+ lqr-time nano-pi) (System/nanoTime)))]
    (if (or (nil? run-in) (neg? run-in))
      (do
        ;; TODO: add debug logging "QueueRunning at time"
        (evaluate (convert/convert '(QueueRun) opts) opts)
        (swap! lqr-atom (fn [_] (System/nanoTime))))
      ;; TODO: else branch: add debug logging "Sleeping for"
      (Thread/sleep (quot run-in 1000000)))))

(defn evaluate [expr {:keys [kernel/link]
                      :as   opts}]
  (assert (instance? com.wolfram.jlink.Expr       expr))
  (assert (instance? com.wolfram.jlink.KernelLink link))
  ;; FIXME: debug log: "evaluate expr>"
  (if (options/flag?' (:flags opts) :serial)
    (io!
     (locking link
       (doto link (.evaluate expr) (.waitForAnswer))
       (.getExpr link)))
    (let [opts' (update opts :flags conj :serial) ;; FIXME: make sure this is supposed to be `:serial`, it's what I gather from previous version of the code
          pid-expr (evaluate (convert/convert '(Unique Clojuratica/Concurrent/process) opts')
                             opts)]
      ;; FIXME: debug log: "pid-expr:"
      (evaluate (convert/convert (list '= pid-expr (list 'ParallelSubmit expr)) opts') opts)
      (evaluate (convert/convert '(QueueRun) opts') opts)
      (loop []
        (let [[state result] (process-state pid-expr opts)]
          (if (not= :finished state)
            (do
              (queue-run-or-wait opts)
              (recur))
            (do
              (evaluate (convert/convert (list 'Remove pid-expr) opts') opts)
              result)))))))
