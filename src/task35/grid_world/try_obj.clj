(ns task35.grid-world.try-obj
  (:use arcadia.core
        clojure.repl
        clojure.pprint)
  (:require [clojure.spec :as s]))

(s/def ::try-obj-args
  (s/or
    :empty (s/cat
             :fallback (s/or :seq seq? :sym symbol?)
             :decl (s/and vector? empty?)
             :body (s/* any?))
    :not-empty (s/cat
                 :fallback (s/? (s/or :seq seq? :sym symbol?))
                 :decl (s/spec (s/and
                                 vector?
                                 (s/cat :sym symbol?
                                        :expr any?)))
                 :body (s/* any?))))

(comment
  (let [conformed (s/conform ::try-obj-args
                    (rest
                      '(try-obj porpoise []
                         (println "flum")
                         (with-cmpt p2 [rb Rigidbody]
                           (blooort rb)))))
        {bla :empty} conformed]
    (pprint conformed)))

(comment
  (pprint
    (s/conform ::try-obj-args
      (rest
        '(try-obj porpoise
           [p2 (GameObject. "hello there")]
           (println "flum")
           (with-cmpt p2 [rb Rigidbody]
             (blooort rb))))))
  (pprint
    (s/conform ::try-obj-args
      (rest
        '(try-obj porpoise []
           (println "flum")
           (with-cmpt p2 [rb Rigidbody]
             (blooort rb))))))
  (pprint
    (s/conform ::try-obj-args
      (rest
        '(try-obj  []
           (println "flum")
           (with-cmpt p2 [rb Rigidbody]
             (blooort rb))))))
  (pprint
    (s/conform ::try-obj-args
      (rest
        '(try-obj porpoise
           [p2 (GameObject. "hello there")])))))

(defn try-obj-form-empty [spec]
  (let [{:keys [body]
         [_ fallback] :fallback} (val spec)]
    `(let [fallback# ~fallback]
       (try
         (do ~@body
             fallback#)
         (catch Exception e#
           (UnityEngine.Debug/LogError e#)
           fallback#)))))

(defn try-obj-form-not-empty [spec]
  (let [spec (val spec)
        fallback (some-> spec :fallback val)
        {:keys [sym expr]} (:decl spec)
        body (:body spec)]
    `(let [~(with-meta sym {:tag 'UnityEngine.GameObject}) ~expr]
       (try
         (do ~@body
             ~sym)
         (catch Exception e#
           (UnityEngine.Debug/LogError e#)
           (let [fallback# ~fallback]
             ;; flakey?
             (when-not (= fallback# ~sym)
               (retire ~sym))
             fallback#))))))

(defmacro try-obj [& args]
  (let [spec (s/conform ::try-obj-args args)]
    (if-not (= ::s/invalid spec)
      (case (first spec)
        :empty (try-obj-form-empty spec)
        :not-empty (try-obj-form-not-empty spec))
      (throw (Exception.
               (str
                 "Invalid use of try-obj:\n"
                 (with-out-str
                   (pprint
                     (::problems
                      (s/explain-data ::try-obj-args args))))))))))
