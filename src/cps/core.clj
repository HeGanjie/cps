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
  (if-not (sym (.getMappings *ns*))
    (if (namespace sym)
      `(cps-prim ~sym)                                      ; evaled + - * / ...
      sym)                                                  ; (fn [f] (f ..))
    (let [s (symbol (str sym '&))]
      (when-not (s (.getMappings *ns*))
        (eval `(def ~s (cps-prim ~sym))))
      s)))

(declare cps-fn)

(defn cps [exp callback]
  (let [exp (macroexpand-all exp)
        transformFnArgs (fn [coll] (map #(if (lambda? %) (cps-fn %)
                                                         (if (symbol? %) (gen-cps-prim %) %)) coll))
        handleCall (fn [f args]
                     (if (not-any? call? exp)
                       (let [handler (if (lambda? f) cps-fn gen-cps-prim)]
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

(defmacro fnlet [[k v & kvp] body]
  (if-not kvp
    `((fn [~k] ~body) ~v)
    `((fn [~k] (fnlet ~kvp ~body)) ~v)))

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
  (if (next body)
    `(defn ~name ~param
       (reset (fndo ~@body)))
    `(defn ~name ~param (reset ~@body))))

(defmacro yield [x]
  `(shift cc# {:value ~x :next (fn [] (cc# nil))}))

(defmacro yield* [g]
  `(shift cc# (yield-all ~g cc#)))

(defn yield-all [ge next-cc]
  (reset
    (fnlet [Yc (fn [g]
                 (fnlet [h (fn [F]
                             (g (fn [a] ((F F) a))))]
                        (h h)))
            looper (Yc (fn [recu]
                         (fn [g]
                           (if g
                             (fndo
                               (shift cc1# {:value (:value g)
                                            :next  (fn [] (cc1# nil))})
                               (recu (apply (get g :next) [])))))))]
           (fndo
             (looper ge)
             (apply next-cc [nil])))))

#_(defgenerator gen []
                (yield 0)
                (yield 1)
                (yield 2))
#_
(defgenerator gen1 []
              (yield 3)
              (yield* (gen))
              (yield 4))

(defn gen2seq [g]
  (lazy-seq
    (when g
      (cons (:value g) (gen2seq ((:next g)))))))

#_(defn Y [g]
    (let [a (fn [F]
              (g
                (fn [& args] (apply (F F) args))))]
      (a a)))

#_(defn fnloop [from to f looper]
    (if (not= from to)
      (fndo
        (looper from)
        (fnloop (f from) to f looper))))

(defgenerator gen [n]
              (fnlet [Yc (fn [g]
                           (fnlet [h (fn [F]
                                       (g (fn [a b c d] ((F F) a b c d))))]
                                  (h h)))
                      fnloop (Yc (fn [recu]
                                   (fn [from to f looper]
                                     (if (not= from to)
                                       (fndo
                                         (looper from)
                                         (recu (f from) to f looper))))))]
                     (fnloop 0 n inc (fn [i] (yield i)))))

#_(cps-anf '(+ 4 (callcc (fn [cc] (cc 3))) (+ 1 (- 3 1))) 'identity)
#_(let [callback identity
        a (fn [x y] (callback (+ 4 x y)))
        b (fn [z]
            (let [o (fn [j] (a z j))
                  n (fn [i] (o (+ 1 i)))
                  m (n (- 3 1))] m))
        c (b (callcc (fn [callback]
                       (fn [cc] (callback (cc 3))))))
        ]
    c)
