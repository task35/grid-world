(ns task35.grid-world.grid-moves
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

