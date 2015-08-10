(ns cps.core
  (:require [clojure.core.match :refer [match]])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

; https://www.wikiwand.com/en/Continuation-passing_style

(defn cps-prim [f]
  (fn [& args]
    ((last args) (apply f (butlast args)))))

(defn fn?? [e]
  (and (e (.getMappings *ns*)) (fn? (eval e))))

(defn gen-cps-prim [sym]
  (let [s (symbol (str sym '&))]
    (eval `(def ~s ~(cps-prim (eval sym))))
    s))

(defn cps [exp callback]
  (match exp
         ([(f-sym :guard fn??) & args] :seq)
         (concat
           (cons (gen-cps-prim f-sym) args) [callback])
         :else exp))

