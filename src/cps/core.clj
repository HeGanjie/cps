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
  (and (seq? exp) (not (lambda? exp))))

; cps for primitive func
(defn gen-cps-prim [sym]
  (if-not (sym (.getMappings *ns*))                         ; symbol maybe in lambda scope, handle lazily
    `(cps-prim ~sym)
    (let [s (symbol (str sym '&))]
      (when-not (s (.getMappings *ns*))
        (eval `(def ~s (cps-prim ~sym))))
      s)))

(declare cps-fn)

(defn cps [exp callback]
  (let [exp (macroexpand-all exp)
        transformFnArgs (fn [coll] (map #(if (lambda? %) (cps-fn %) %) coll))
        handleCall (fn [f args]
                     (if (not-any? call? exp)
                       (let [handler (if (lambda? f)
                                       cps-fn
                                       gen-cps-prim)]
                         `(~(handler f) ~@(transformFnArgs args) ~callback))
                       (let [newSym (gensym)
                             [front [preEval & rest]] (split-with (complement call?) exp)
                             newExp `(~@front ~newSym ~@rest)]
                         (cps preEval `(fn [~newSym] ~(cps newExp callback))))))]
    (match exp
           (exp :guard #(not (seq? %))) `(~callback ~exp)   ; var
           (['quote x] :seq) `(~callback ~exp)              ; quote
           (['if test trueExp & rest] :seq)                 ; if
           (if (seq? test)
             (let [newSym (gensym)]
               (cps test `(fn [~newSym] (if ~newSym
                                          ~(cps trueExp callback)
                                          ~(if rest (cps (first rest) callback))))))
             `(if ~test ~(cps trueExp callback) ~(if rest (cps (first rest) callback))))
           (['fn* ([(args :guard vector?) body] :seq)] :seq) ; (fn [] ...)
           `(~callback ~(cps-fn exp))
           ([f-sym & args] :seq)                            ; call
           (handleCall f-sym args)
           )))

(defn fndo [& rest] (last rest))

; cps for lambda
(defn cps-fn [exp]
  (let [exp (macroexpand-all exp)]
    (match exp
           (['fn* ([(args :guard vector?) body] :seq)] :seq)
           (let [callback (gensym)]
             `(fn ~(conj args callback) ~(cps body callback)))
           :else (throw (Exception. (println-str "Can't match: " exp))))))

(defn callcc& [f& cc]
  (f& cc identity))

(defn callcc [f]
  (throw (Exception. "I should not execute!")))

(defmacro shift [cc exp]
  `(~'callcc (fn [~cc] ~exp)))

(defmacro reset [exp]
  (cps exp 'identity))

; (reset (+ 1 (shift k (k 2)))) => 3

; abnormal for callcc, but seems to ok in Delimited continuation:
; (reset (+ 1 (shift k (fndo (k 3) 2)))) => 2 (should be 4)

(defmacro defgenerator
  [name param & body]
  `(defn ~name ~param
     (reset (fndo ~@body))))

(defmacro yield [x]
  `(shift cc# {:value ~x :next (fn [] (cc# nil))}))

#_(defgenerator gen ()
                (yield 0)
                (yield 1)
                (yield 2))
