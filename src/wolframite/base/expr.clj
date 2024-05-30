(ns wolframite.base.expr
  (:require [wolframite.impl.jlink-instance :as jlink-instance]
            [wolframite.impl.protocols :as proto]))

(defn head-str [expr]
  (assert (proto/expr? (jlink-instance/get) expr))
  (.toString (.head expr)))

(defn parts [expr]
  (assert (proto/expr? (jlink-instance/get) expr))
  (cons (.head expr) (seq (.args expr))))

(defn expr-from-parts [expr-coll]
  (assert (every? #(proto/expr? (jlink-instance/get) %) expr-coll))
  (proto/expr (jlink-instance/get) expr-coll))


