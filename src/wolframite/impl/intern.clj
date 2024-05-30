(ns wolframite.impl.intern
  "Interning of Wolfram symbols as Clojure vars, for convenience."
  (:import (clojure.lang IMeta)))

(defn interned-var-val->symbol
  "Turns the value of an [[clj-intern]]-ed Wolfram symbols into said symbol, if possible - otherwise, returns nil.
  Ex.:
  ```clj
  (clj-intern 'Plus {})
  Plus ; => #function[wolframite...]
  (interned-var-val->symbol Plus) ; => 'Plus
  ```"
  [maybe-fn]
  (when (instance? IMeta maybe-fn)
    (-> maybe-fn meta ::wolfram-sym)))

(defn- wolfram-fn
  "Turn the wolfram symbol into a metadata-tagged function, which returns a list with
  the symbol at head, and any arguments as-is. The metadata contains the symbol.
  Used for exposing Wolfram symbols to Clojure code."
  [sym]
  ^{::wolfram-sym sym} (fn wolf-fn [& args]
                         (apply list sym
                                (->> args
                                     (map (some-fn interned-var-val->symbol
                                                   ;; I don't think we should ever get to 👇 but better safe than sorry...
                                                   identity))))))

(defn clj-intern
  "Finds or creates a var named by the symbol `wl-sym` in the current namespace,
  which will resolve into a symbol of the same name.

  You can override the target namespace and add additional metadata to the var by
  setting the appropriate `opts`.

  Ex.:
  ```clj
  (clj-intern 'Plus {})
  (wl/eval (Plus 1 2))
  ; => 3
  ```

  See also [[load-all-symbols]]."
  ([wl-sym]
   (clj-intern wl-sym {}))
  ([wl-sym {:intern/keys [ns-sym extra-meta] :as opts}]
   (let [f (wolfram-fn wl-sym)]
     (intern (create-ns (or ns-sym (.name *ns*)))
             (with-meta wl-sym (merge {:clj-intern true}
                                      extra-meta
                                      (meta f)))
             f))))