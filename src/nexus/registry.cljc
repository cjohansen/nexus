(ns nexus.registry
  (:require [nexus.core :as nexus]))

(def ^:no-doc !registry (atom {}))

(defn register-system->state! [f]
  (swap! !registry assoc :nexus/system->state f))

(defn ^{:indent 1} register-action! [action-k f]
  (swap! !registry assoc-in [:nexus/actions action-k] f))

(defn ^{:indent 1} register-effect! [effect-k f]
  (swap! !registry assoc-in [:nexus/effects effect-k] f))

(defn ^{:indent 1} register-placeholder! [placeholder-k f]
  (swap! !registry assoc-in [:nexus/placeholders placeholder-k] f))

(defn ^{:indent 1} register-interceptor!
  ([phase f]
   (register-interceptor! {phase f}))
  ([interceptor]
   (swap! !registry update :nexus/interceptors (fnil conj []) interceptor)))

(defn get-interceptors []
  (:nexus/interceptors @!registry))

(defn get-registry []
  @!registry)

(defn dispatch [store dispatch-data actions]
  (nexus/dispatch (get-registry) store dispatch-data actions))
