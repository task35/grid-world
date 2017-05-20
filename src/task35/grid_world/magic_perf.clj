(ns task35.grid-world.magic-perf
  (:use arcadia.core
        arcadia.linear
        clojure.repl
        clojure.pprint)
  (:require [clojure.spec :as s]
            [timsg.imperative :as i]
            [magic.api :as m]
            [arcadia.internal.map-utils :as mu])
  (:import [UnityEngine
            Vector3
            Mathf
            Quaternion
            Transform GameObject Light
            LightType Rigidbody PhysicMaterial
            Collider Mathf Time ForceMode]))

(defn move-towards [obj1, obj2, speed]
  (with-cmpt obj1 [tr1 Transform]
    (with-cmpt obj2 [tr2 Transform]
      (identity
       ;;m/faster
        (let [diff (v3- (.position tr2) (.position tr1))]
          (set! (.position tr1)
            (v3+ (.position tr1)
                 (v3* (.normalized diff)
                      (Mathf/Min speed (.magnitude diff))))))))))
