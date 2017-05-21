(ns task35.grid-world
  (:use arcadia.core
        arcadia.linear
        clojure.repl
        clojure.pprint)
  (:require [timsg.scene-var.get :as sv]
            [magic.api :as m]
            [arcadia.internal.map-utils :as mu]
            [arcadia.introspection :as int]
            ;; [timsg.unity-tools.material :as mat]
            [timsg.imperative :as i]
            [task35.grid-world.try-obj :as to]
            [clojure.spec :as s]
            [task35.grid-world.behavior-graph :as bg])
  (:import [UnityEngine
            Vector3
            Mathf
            Quaternion
            Transform GameObject Light
            LightType Rigidbody PhysicMaterial
            Collider Mathf Time ForceMode]))

(m/bind-basic-spells!)

(defn mempr [& args]
  (pprint
    (map #(.Name %)
      (apply int/members args))))

(defn scene-view ^UnityEditor.SceneView []
  UnityEditor.SceneView/lastActiveSceneView)

(defn align-to-scene-view [x]
  (when-let [^UnityEditor.SceneView sv (scene-view)]
    (let [scene-camera (.. sv camera gameObject)]
      (with-cmpt x [tr Transform]
        (with-cmpt scene-camera [sctr Transform]
          (i/sets! tr
            position (.position sctr)
            rotation (.rotation sctr))))))
  x)

(defmacro vfor [& stuff]
  `(vec (for ~@stuff)))

(defn bename [^GameObject obj, name]
  (set! (.name obj) name)
  obj)

(comment
  (sv/defgetter origin-sphere
    ([]
     (-> (create-primitive :sphere) (bename "origin-sphere")))
    ([origin-sphere]
     (to/try-obj origin-sphere []
       (with-cmpt origin-sphere [tr Transform]
         (i/sets! tr
           position (v3 0 0 0)
           localScale (v3 1 5 1)))))))

(sv/defgetter the-floor
  ([]
   (GameObject. "the-floor"))
  ([the-floor]
   (to/try-obj the-floor []
     (comment
       (let [floor-width-x 150
             floor-width-z floor-width-x
             tiles-x 40
             tiles-z tiles-x
             tile-width-x (/ floor-width-x tiles-x)
             tile-width-z (/ floor-width-z tiles-z)
             tile-width-y 100]
         (dorun (map retire (children the-floor)))
         (with-cmpt the-floor [tr Transform]
           (i/sets! tr
             position (v3 0)))
         (let [tiles (vfor [x (range tiles-x)]
                       (vfor [z (range tiles-z)]
                         (let [tile (create-primitive :cube)]
                           (set! (.name tile) (str "tile_" x "_" z))
                           (with-cmpt tile [tr Transform]
                             (i/sets! tr
                               localScale (v3 tile-width-x tile-width-y tile-width-z)
                               localPosition (-> (v3scale (v3 x 1 z)
                                                          (v3 tile-width-x tile-width-y tile-width-z))
                                                 (v3+ (v3div (v3 tile-width-x tile-width-y tile-width-z)
                                                             2))
                                                 (v3- (v3scale (v3 floor-width-x tile-width-y floor-width-z)
                                                               (v3 0.5 2 0.5))))))
                           (child+ the-floor tile)
                           tile)))]
           (set-state! the-floor ::tiles tiles)))))))

(defn tile-foreach [tiles f]
  (dotimes [x (count tiles)]
    (let [xrow (get tiles x)]
      (dotimes [z (count xrow)]
        (f (get xrow z) x z))))
  ;; invalid il!
  ;; (m/faster
  ;;   (dotimes [x (count tiles)]
  ;;     (let [xrow (get tiles x)]
  ;;       (dotimes [z (count xrow)]
  ;;         (f (get xrow z) x z)))))
  )

(def whatsit-log (atom nil))

(defn tile-hook+ [tiles hook kw f]
  (tile-foreach tiles
    (fn [tile x z] ;; gonna box anyway
      (hook+ tile hook kw f
        ;; (fn
        ;;   ([obj _] ;; doesn't work for all hooks :(
        ;;    ;;(reset! whatsit-log (cons obj args))
        ;;    (f obj x z)))
        ))))

(defn tile-hook- [tiles hook kw]
  (tile-foreach tiles
    (fn [tile _ _]
      (hook- tile hook kw))))

(defmacro fast-cmpt [obj bndgs & body]
  (let [objsym (with-meta (gensym "obj_") {:tag 'UnityEngine.GameObject})
        dcls (->> bndgs
                  (partition 2)
                  (mapcat (fn [[sym t]]
                         (let [sym2 (with-meta sym {:tag t})]
                           [sym2 `(.GetComponent ~objsym ~t)]))))]
    `(let [~objsym ~obj
           ~@dcls]
       ~@body)))

;; this is what you've driven me to
(definline plus [x y]
  `(float (+ (double ~x) (double ~y))))

(definline minus [x y]
  `(float (- (double ~x) (double ~y))))

(definline times [x y]
  `(float (* (double ~x) (double ~y))))

(definline div [x y]
  `(float (/ (double ~x) (double ~y))))

(m/defn rtss ^System.Single []
  Time/realtimeSinceStartup)

(defn tile-update [tile _]
  (fast-cmpt tile [tr Transform]
    (m/faster
      (let [t (rtss)
            pos (.position tr)]
        (i/sets! tr
          position (-> (v3 (.x pos)
                           (times (Mathf/Cos (+ (rtss)
                                                (div (.x pos) 10)
                                                (div (.z pos) 10)))
                             5)
                           (.z pos))
                       (v3- (v3 0 (/ (float (.. tr localScale y)) (float 2)) 0))))))))

(comment
  (tile-hook+ (state (the-floor) ::tiles) :update ::tile-update #'tile-update)
  (tile-hook- (state (the-floor) ::tiles) :update ::tile-update)
  
  (tile-hook+ (state (the-floor) ::tiles) :fixed-update ::tile-update #'tile-update)
  (tile-hook- (state (the-floor) ::tiles) :fixed-update ::tile-update)
  )

(defn freeze ^GameObject [^GameObject x]
  (doto x
    (fast-cmpt [rb Rigidbody]
      (i/sets! rb
        velocity (v3 0)
        angularVelocity (v3 0)))))

(sv/defgetter sphere-buddy
  ([]
   (let [x (create-primitive :sphere)]
     (set! (.name x) "sphere-buddy")
     x))
  ([sphere-buddy]
   (to/try-obj sphere-buddy []
     (with-cmpt sphere-buddy [tr Transform
                              rb Rigidbody]
       (freeze sphere-buddy)
       (i/sets! tr
         position (v3 35 10 35)
         localScale (v3 2))))))


;; ============================================================
;; let's do something with behavior-graph

(def patroller-behavior
  {::bg/current-node ::advance
   ::bg/graph {::advance {::bg/transition {::retreat ::retreat}}
               ::retreat {::bg/transition {::advance ::advance}}}})


;; ============================================================
;; patroller-advance

(m/defn position [^Transform tr]
  (.position tr))

(m/defn v-mag [^Vector3 v]
  (.magnitude v))

(m/defn v-normalized [^Vector3 v]
  (.normalized v))

(defn patroller-advance [^GameObject obj _]
  (fast-cmpt obj [tr Transform]
    (m/faster
      (let [targ (v3 20)
            speed 0.1
            pos (position tr)
            diff (v3- targ pos)
            dist (v-mag diff)]
        (i/sets! tr
          position (if (= pos targ)
                     (v3+ pos
                          (v3 speed 0 0))
                     (v3- pos
                          (v3* (v-normalized diff) ;; the .normalized makes it really really slow
                               (Mathf/Min (float speed) dist)))))
        (when (<= (float 20) dist)
          ;; (log "retreating!")
          ::retreat)))))

;; (comment
;;   (defn patroller-advance [^GameObject obj _]
;;     (fast-cmpt obj [tr Transform]
;;       (m/faster
;;         (let [targ (v3 20)
;;               speed 0.1
;;               diff (v3- targ (.position tr))
;;               dist (.magnitude diff)]
;;           (i/sets! tr
;;             position (if (= (.position tr) targ)
;;                        (v3+ (.position tr)
;;                             (v3 speed 0 0))
;;                        (v3- (.position tr)
;;                             (v3* (v-normalized diff)
;;                                  (Mathf/Min (float speed) dist)))))
;;           (when (<= (float 20) dist)
;;             ;; (log "retreating!")
;;             ::retreat)))))



;;   (defn patroller-advance [^GameObject obj _]
;;     (fast-cmpt obj [tr Transform]
;;       (let [targ (v3 20)
;;             speed 0.1
;;             pos (.position tr)
;;             diff (v3- targ pos)
;;             dist (v-mag diff)]
;;         (m/faster
;;           (i/sets! tr
;;             position (if (= pos targ)
;;                        (v3+ pos
;;                             (v3 speed 0 0))
;;                        (v3- pos
;;                             (v3* (.normalized diff)
;;                                  (Mathf/Min (float speed) dist))))))
;;         (when (<= (float 20) dist)
;;           ;; (log "retreating!")
;;           ::retreat))))

;;   (m/faster
;;     (let [speed 0.1
;;           diff (v3 3)
;;           dist (.magnitude diff)]
;;       (if true
;;         ::hi
;;         (v3* (.normalized diff)
;;              (Mathf/Min  ;; (float speed)
;;                dist)))))

;;   )
;; (defn patroller-advance [^GameObject obj _]
;;   (fast-cmpt obj [tr Transform]
;;     (m/faster
;;       (let [targ (v3 20)
;;             speed 0.1
;;             pos (position tr)
;;             diff (v3- targ pos)
;;             dist (.magnitude diff)]
;;         (i/sets! tr
;;           position (if (= pos targ)
;;                      (v3+ pos
;;                           (v3 speed 0 0))
;;                      (v3- pos
;;                           (v3* (.normalized diff)
;;                                (Mathf/Min (float speed) dist)))))
;;         (when (<= (float 20) dist)
;;           ;; (log "retreating!")
;;           ::retreat)))))

;; end of magic perf test section
;; ==================================================

(defn patroller-retreat [^GameObject obj _]
  (fast-cmpt obj [tr Transform]
    (m/faster
      (let [targ (v3 20)
            speed 0.1
            diff (v3- targ (.position tr))
            dist (.magnitude diff)]
        (i/sets! tr
          position (if (= (.position tr) targ)
                     (v3+ (.position tr)
                          (v3 speed 0 0))
                     (v3+ (.position tr)
                          (v3* (v-normalized diff)
                               (Mathf/Min (float speed) dist)))))
        (when (<= dist (float 3))
          ;; (log "advancing!")
          ::advance)))))

(def patroller-role
  (bg/graph-role
    patroller-behavior
    {::bg/update {::advance #'patroller-advance
                  ::retreat #'patroller-retreat}}))

(defn loadup [obj _])

(sv/defgetter patrolsman
  ([]
   (let [x (create-primitive :sphere)]
     (set! (.name x) "patrolsman")
     x))
  ([patrolsman]
   (to/try-obj patrolsman []
     (with-cmpt patrolsman [tr Transform
                            rb Rigidbody]
       (freeze patrolsman)
       (i/sets! rb
         isKinematic true)
       (i/sets! tr
         position (v3 0 20 0)
         localScale (v3 1))
       ;; shift over to roles API
       (role+ patrolsman ::patrol patroller-role)

       
       (hook+ patrolsman :awake ::loadup #'loadup)
       ;;(set-state! patrolsman ::patrol-state {::state-machine patroller-behavior})
       ;;(hook+ patrolsman :update ::on-patrol #'on-patrol)
       ))))

(comment
  (import 'ArcadiaBehaviour)

  (def ensure-hook-type (var-get #'arcadia.core/ensure-hook-type))

  (let [[obj hook k f] [( patrolsman) :update ::on-patrol #'on-patrol]]
    (let [hook-type (ensure-hook-type hook)
          ^ArcadiaBehaviour hook-cmpt (ensure-cmpt obj hook-type)
          ]
      ;;(ensure-cmpt obj hook-type)
      (println (with-out-str (pprint {:hook-cmpt hook-cmpt
                                      :f f
                                      :k k})))
      (.AddFunction hook-cmpt f k)
      ;; obj
      )))

(comment
  @ugh-log
  (let [{:keys [obj k]} @ugh-log]
    (let [{:keys [::state-machine],
           update-map ::update} (state obj k)
          f (get update-map (current-state state-machine))]
      (f obj k)
      ;; (when-let [signal (f obj k)]
      ;;   (update-state! obj k
      ;;     update ::state-machine
      ;;     transition signal)
      ;;   )
      )))

(comment
  (defrecord Smurr [x])
  (require '[arcadia.internal.benchmarking :as b])
  (let [^Smurr s (Smurr. :bla)]
    (m/faster
      (b/n-timing 1e6
        (.x s))))
  ;; 7.523E-06
  (let [s {:x :bla}]
    (m/faster
      (b/n-timing 1e6
        (get s :x))))
  ;; 0.000767049
  (let [^Smurr s (Smurr. :bla)]
    (m/faster
      (b/n-timing 1e6
        (get s :x))))
  ;; 0.00078255
  (let [^Smurr s (Smurr. :bla)
        k :x]
    (m/faster
      (b/n-timing 1e6
        (get s k))))
  ;; 0.000116132
  (/ 0.000116132 7.523E-06)
  ;; 15.4369267579423
  )

(comment
  (let [{:keys [obj k]} @hook-log]
    (let [{:keys [::state-machine],
           update-map ::update} (state obj k)
          f (get update-map (current-state state-machine))]
      (f obj k)
      ;; (f obj k)
      ;; (when-let [signal (f obj k)]
      ;;   (update-state! obj k
      ;;     update ::state-machine
      ;;     transition signal)
      ;;   )
      )))


(comment
  (do (require '[arcadia.internal.benchmarking :as b])
      (def ^:dynamic *blorbo* :blorbo)
      (m/faster
        (b/n-timing 1e6
          *blorbo*))))


(comment
  (do
    (import '[Task35 IdentityTest IdentityTest+Vlumper IdentityTest+OtherVlumper])
    )

  (let [vlumper (IdentityTest+Vlumper.)
        other-vlumper (IdentityTest+OtherVlumper.)]
    (.Vlump other-vlumper))

  (let [vlumper (IdentityTest+Vlumper.)
        other-vlumper (IdentityTest+OtherVlumper.)]
    (.Vlump other-vlumper))

  (IdentityTest/VlumpTest))

