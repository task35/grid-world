(ns task35.grid-world.friendship
  (:use arcadia.core
        arcadia.linear
        clojure.repl
        clojure.pprint)
  (:require [timsg.scene-var.get :as sv]
            [magic.api :as m]
            [arcadia.internal.map-utils :as mu]
            [arcadia.introspection :as int]
            [timsg.unity-tools.material :as mat]
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

(def joe-bob
  (let [x (create-primitive :cube)]
    (set! (.name x) "joe-bob")
    x))

(def henry
  (let [x (create-primitive :sphere)]
    (set! (.name x) "henry")
    x))

(def rabia
  (let [x (create-primitive :capsule)]
    (set! (.name x) "rabia")
    x))

;; ============================================================

(defn move-towards [obj1, obj2, speed]
  (with-cmpt obj1 [tr1 Transform]
    (with-cmpt obj2 [tr2 Transform]
      (;;identity
       m/faster
        (let [diff (v3- (.position tr2) (.position tr1))]
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
            (do (log "hiii")
                (set! (.position tr1)
                  (v3+ (.position tr1) (v3 1))))
            (set! (.position tr1)
              (v3- (.position tr1)
                   (v3* (.normalized diff) speed)))))))))

;; ============================================================

(defn friend-update [obj k _]
  (when-let [{:keys [::friend ::speed]} (state obj k)]
    (move-towards obj friend speed)))

(defn friend-awake [_ k _]
  (log (str "Waking up friendship at " k "!")))

(def friendship
  {:state {::friend nil
           ::speed 0.1}
   :update #'friend-update
   :awake #'friend-awake})

(def eu-counter (atom 0))

(defn enemy-update [obj k _]
  (swap! eu-counter inc)
  (when-let [{:keys [::enemy ::speed]} (state obj k)]
    (move-away-from obj enemy speed)))

(defn enemy-awake [_ k _]
  (log (str "Waking up enemyship at " k "!")))

(def enemyship
  {:state {::enemy nil
           ::speed 0.1}
   :update #'enemy-update
   :awake #'enemy-awake})

;; ============================================================

(role+ rabia ::henry-friendship
  (-> friendship
      (assoc-in [:state ::friend] henry)
      (assoc-in [:state ::speed] 0.4)))

(role+ rabia ::joe-bob-friendship
  (assoc-in friendship [:state ::friend] joe-bob))

(role+ henry ::joe-bob-enemyship
  (assoc-in enemyship [:state ::enemy] joe-bob))

(role+ joe-bob ::rabia-friendship
  (assoc-in friendship [:state ::friend] rabia))


;; ============================================================

(defn wobble-update [obj k _ ;;{:keys [::amplitude]}
                     ]
  ;;(let [{:keys [::amplitude]} (state obj k)])
  (with-cmpt obj [tr Transform]
    (;;identity
     m/faster
      (let [amp (float 5) ;;(float amplitude)
            ]
        (set! (.localScale tr)
          (v3
            (* (+ (float 1) (float (Mathf/Sin Time/realtimeSinceStartup)))
               amp))))
      nil))
  )

(defn wobbler-role [amplitude]
  {:state {::amplitude amplitude}
   :update #'wobble-update})

(comment
  (def blorbo-the-wobbler
    (let [x (create-primitive :sphere)]
      (set! (.name x) "blorbo-the-wobbler")
      (with-cmpt x [tr Transform]
        (set! (.position tr) (v3 20 10 20)))
      (role+ x ::wobble (wobbler-role 5))
      x))
  )
