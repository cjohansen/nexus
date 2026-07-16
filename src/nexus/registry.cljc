(ns nexus.registry
  (:require [nexus.core :as nexus]))

(def ^:no-doc !registry (atom {}))

(defn register-system->state! [f]
  (swap! !registry assoc :nexus/system->state f))

(defn register-system+dispatch-data->state! [f]
  (swap! !registry assoc :nexus/system+dispatch-data->state f))

(defn ^{:indent 1} register-action! [action-k f]
  (swap! !registry assoc-in [:nexus/expansions action-k] f))

(defn ^{:indent 1} register-expansion! [action-k f]
  (swap! !registry assoc-in [:nexus/expansions action-k] f))

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

(defn on-error [f]
  (swap! !registry assoc :nexus/on-error f))

(defn dispatch [system dispatch-data actions]
  (nexus/dispatch (get-registry) system dispatch-data actions))
