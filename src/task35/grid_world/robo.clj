(ns task35.grid-world.robo
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


;; ============================================================

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

(m/defn position [^Transform tr]
  (.position tr))

(m/defn v-mag [^Vector3 v]
  (.magnitude v))

(m/defn v-normalized [^Vector3 v]
  (.normalized v))

(defn freeze ^GameObject [^GameObject x]
  (doto x
    (fast-cmpt [rb Rigidbody]
      (i/sets! rb
        velocity (v3 0)
        angularVelocity (v3 0)))))

(defn move-towards [obj1, obj2, speed]
  (with-cmpt obj1 [tr1 Transform]
    (with-cmpt obj2 [tr2 Transform]
      (;;identity
       m/faster
       (let [ diff (v3- (.position tr2) (.position tr1))]
          (set! (.position tr1)
            (v3+ (.position tr1)
                 (v3* (.normalized diff)
                      (Mathf/Min speed (.magnitude diff))))))))))

(defn move-away-from [obj1, obj2, speed]
  (with-cmpt obj1 [tr1 Transform]
    (with-cmpt obj2 [tr2 Transform]
      (;;identity
       m/faster
       (let [diff (v3- (.position tr2) (.position tr1))]
         (if (< (.magnitude diff) 0.1)
           (set! (.position tr1)
             (v3+ (.position tr1) (v3 1)))
           (set! (.position tr1)
             (v3- (.position tr1)
                  (v3* (.normalized diff) speed)))))))))

;; ============================================================

(defn robo-advance [^GameObject obj k]
  (let [{:keys [:target :speed]} (state obj k)]
    (if target
      (move-towards obj target speed)
      (m/faster
        (fast-cmpt obj [tr Transform]
          (fast-cmpt target [targ-tr Transform]
            (let [p1 (position tr)
                  p2 (position targ-tr)]
              (if (< (Vector3/Distance p1 p2) (float 5))
                (update-state! obj k assoc :mode ::retreat))
              )))))))

(defn robo-retreat [^GameObject obj ^GameObject target speed k]
  (let [{:keys [:target :speed]} (state obj k)]
    (if target
      (move-away-from obj target speed)
      (let [target ^GameObject target]
        (m/faster
          (let [p1 (position (.. obj transform))
                p2 (position (.. target transform))]
            (if (< (float 50) (Vector3/Distance p1 p2))
              (update-state! obj k assoc :mode ::advance))))))))

(defn robo-1-fixed-update [obj k]
  (let [{:keys [:target :mode]} (state obj k)]
    (if target
      (case mode
        ::advance (robo-advance obj target speed)
        ::retreat (robo-retreat obj target speed)
        ::stand (freeze obj)
        (freeze obj))
      (freeze obj))))

(sv/defgetter robo-1
  ([] (create-primitive :capsule))
  ([robo-1]
   (to/try-obj robo-1 []
     (with-cmpt robo-1 [tr Transform
                        _ Rigidbody]
       (i/sets! tr
         position (v3 0 10 0)
         localScale (v3 4)))
     (role+ robo-1 ::motion
       {:state {:target nil
                :mode :stand
                :speed 1}
        :fixed-update #'robo-1-fixed-update}))))
