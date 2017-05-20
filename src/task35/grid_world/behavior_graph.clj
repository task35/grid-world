(ns task35.grid-world.behavior-graph
  (:use arcadia.core
        arcadia.linear
        clojure.repl
        clojure.pprint)
  (:require [clojure.spec :as s]))

;; Simple prototype for state machine. Once we get this working we can
;; think about fancier versions.

;; ============================================================
;; spec

;; for now; this grammar is internal, totally subject to change
(s/def ::current-node
  any?)

(s/def ::transition
  map?)

(s/def ::graph
  (s/map-of
    any?
    (s/keys
      :opt [::transition])))

;; maybe this doesn't matter? dunno
(defn transition-closure? [{:keys [::graph]}]
  (let [ks (set (keys graph))
        transitions (into #{}
                      (comp
                        (map ::transition)
                        (mapcat vals))
                      (vals graph))]
    (clojure.set/subset? transitions ks)))

(s/def ::state-machine
  (s/and
    (s/keys
      :req [::current-node ::graph])
    transition-closure?))

;; ============================================================
;; basic ops

(comment
  {::current-node ::advance
   ::graph {::advance {::transition {::retreat ::retreat}}
            ::retreat {::transition {::safe ::advance}}}})

(defn build-state-machine [spec]
  ;; for now let's just say the spec format we're using is the state machine. I mean why not
  spec)

(defn next-node [state-machine signal]
  (let [{:keys [::current-node ::graph]} state-machine
        {{:keys [::transition]} current-node} graph]
    (get transition signal)))

(defn set-current-node [state-machine node]
  (if (not= (::current-node state-machine) node)
    (assoc state-machine ::current-node node)
    state-machine))

(defn merge-graph [g1 g2]
  (merge-with (fn [{t1 ::transition :as v1}
                   {t2 ::transition :as v2}]
                (-> (merge v1 v2)
                    (assoc ::transition (merge t1 t2))))
    g1 g2))

(defn merge-sm [{g1 ::graph :as sm1}
                {g2 ::graph :as sm2}]
  (-> (merge sm1 sm2)
      (assoc ::graph (merge-graph g1 g2))))

;; + build-state-machine, above

(defn transition [state-machine, signal]
  (if-let [nn (next-node state-machine signal)]
    (set-current-node state-machine nn)
    state-machine))

(defn current-node [state-machine]
  (::current-node state-machine))

(defn has-node? [{:keys [::graph]} node]
  (contains? graph node))

(defn nodes [{:keys [::graph]}]
  (keys graph))

(defn- map-vals [m f]
  (persistent!
    (reduce-kv
      (fn [m' k v]
        (assoc! m' k (f v)))
      (transient m)
      m)))

(defn remove-nodes [{:keys [::graph ::current-node] :as sm} nodes]
  (let [node-set (if (set? nodes) nodes (set nodes))]
    (-> (if (node-set current-node)
          (assoc sm ::current-node nil)
          sm)
        (assoc ::graph 
          (-> (reduce dissoc graph node-set)
              (map-vals (fn [x]
                          (update x ::transition
                            (fn [trn]
                              (into {}
                                (remove #(or (node-set (key %)) (node-set (val %))))
                                trn))))))))))

(comment
  {::state-machine sg
   ::update {::advance #'patroller-advance
             ::retreat #'patroller-retreat}})
;; ============================================================
;; some more stuff

(defn update-graph-hook [obj k _]
  (let [{:keys [::state-machine],
         update-map ::update} (state obj k)
        f (get update-map (current-node state-machine))]
    (when-let [signal (f obj k)]
      (update-state! obj k
        update ::state-machine
        transition signal))))

;; for now
(defn graph-role [state-machine behavior-map]
  {:state (assoc behavior-map ::state-machine state-machine)
   :update #'update-graph-hook})

(comment
  (role+ obj ::patrol
    {:state {::state-machine sg
             ::update {::advance #'patroller-advance
                       ::retreat #'patroller-retreat}}
     :update (fn [obj]
               (let [{:keys [::state-machine],
                      update-map ::update} (state obj ::patrol)
                     f (get update-map (current-node state-machine))]
                 (when-let [signal (f obj)]
                   (update-state! obj ::patrol
                     update ::state-machine
                     transition signal))))}))

(comment
  (defn update-graph-hook [obj k]
    (let [{:keys [::state-machine],
           update-map ::update} (state obj k)
          f (get update-map (current-node state-machine))]
      (when-let [signal (f obj k)]
        (update-state! obj k
          update ::state-machine
          transition signal))))

  (role+ obj ::patrol
    {:state {::state-machine sg
             ::update {::advance #'patroller-advance
                       ::retreat #'patroller-retreat}}
     :update #'update-graph-hook}))


(comment
  (defn update-graph-hook [obj]
    (let [{:keys [::state-machine],
           update-map ::update} (state obj k)
          f (get update-map (current-node state-machine))]
      (when-let [signal (f obj k)]
        (update-state! obj k
          update ::state-machine
          transition signal))))

  (role+ obj ::state-machine
    {:state {::patrol {::state-machine sg
                       ::update {::advance #'patroller-advance
                                 ::retreat #'patroller-retreat}}}
     :update #'update-graph-hook}))
