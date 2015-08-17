(ns cps.core
  (:require [clojure.core.match :refer [match]]
            [clojure.walk :refer [macroexpand-all]])
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
  (if (symbol? e)
    (or (fn? (eval e))
        (and (e (.getMappings *ns*)) (fn? (eval e))))
    (fn? e)))

(defn lambda? [exp]
  (when (seq? exp)
    (let [head (first exp)]
      (or
        (= 'fn* head)
        (= `fn* head)))))

(defn call? [exp]
  (when (seq? exp)
    (let [head (first exp)]
      (or
        (fn?? head)
        (lambda? head)))))

; cps for primitive func
(defn gen-cps-prim [sym]
  (let [s (symbol (str sym '&))]
    (when-not (s (.getMappings *ns*))
      (eval `(def ~s ~(cps-prim (eval sym)))))
    s))

(declare cps-fn)

; TODO implement first class lambda args cps transform: (f (fn [n] ...) ...) -> (f (fn [n k] ...) ...)
(defn cps [exp callback]
  (let [exp (macroexpand-all exp)
        handleCall (fn [handler f args]
                     (if (not-any? call? args)
                       `(~(handler f) ~@args ~callback)
                       (let [newSym (gensym)
                             preEval (first (filter call? args))
                             vargs (vec args)
                             newExp (assoc vargs (.indexOf vargs preEval) newSym)]
                         (cps preEval `(fn [~newSym] ~(cps (cons f newExp) callback))))))]
    (match exp
           ([(f-sym :guard fn??) & args] :seq)              ; call
           (handleCall gen-cps-prim f-sym args)
           ([(fn-exp :guard lambda?) & args] :seq)          ; ((fn [] ...) ...)
           (handleCall cps-fn fn-exp args)
           (['if test trueExp & rest] :seq)                 ; if
           (if (call? test)
             (let [newSym (gensym)]
               (cps test `(fn [~newSym] (if ~newSym ~(cps trueExp callback) ~(if rest (cps (first rest) callback))))))
             `(if ~test ~(cps trueExp callback) ~(if rest (cps (first rest) callback))))
           :else `(~callback ~exp))))

; cps for lambda
(defn cps-fn [exp]
  (let [exp (macroexpand-all exp)]
    (match exp
           (['fn* ([(args :guard vector?) body] :seq)] :seq)
           (let [callback (gensym)]
             `(fn ~(conj args callback) ~(cps body callback)))
           :else (throw (Exception. (println-str "Can't match: " exp))))))

(defn callcc& [f k]
  (f k k))

(defn callcc [f]
  (throw (Exception. "I should not execute!")))

(defmacro shift [k exp]
  `(callcc (fn [~k] ~exp)))

(defmacro reset [exp]
  (cps exp 'identity))
