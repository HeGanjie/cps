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

(defn call? [exp]
  (and (seq? exp)
       (-> exp first fn??)))

(defn cps [exp callback]
  (match exp
         ([(f-sym :guard fn??) & args] :seq)
         (if (not-any? call? args)
           `(~(gen-cps-prim f-sym) ~@args ~callback)
           (let [newSym (gensym)
                 preEval (first (filter call? args))
                 vargs (vec args)
                 newExp (assoc vargs (.indexOf vargs preEval) newSym)]
             (cps preEval `(fn [~newSym] ~(cps (cons f-sym newExp) callback)))))
         ([(if-sym :guard #(= 'if %)) test trueExp & rest] :seq)
         (if (call? test)
           (let [newSym (gensym)]
             (cps test `(fn [~newSym] (if ~newSym ~(cps trueExp callback) ~(if rest (cps (first rest) callback))))))
           `(if ~test ~(cps trueExp callback) ~(if rest (cps (first rest) callback))))
         :else `(~callback ~exp)))

