(ns rotten.db)

(defonce state (atom {:world       {}
                      :entities    {}
                      :player      {}
                      :turn        0
                      :game-bounds {:x-min 1
                                    :x-max 20
                                    :y-min 1
                                    :y-max 18}}))

;; TODO: check out https://vvvvalvalval.github.io/supdate/

(defn move-entity [from to entity-id]
  (swap! state (fn [old]
                 (as-> old $
                   (update-in $ [:world from :tile/entities] (fnil disj #{}) entity-id)
                   (update-in $ [:world to   :tile/entities] (fnil conj #{}) entity-id)
                   (update-in $ [:entities entity-id] assoc :entity/position to)))))


(defn place-entity [to entity-id]
  (swap! state (fn [old]
                 (as-> old $
                   (update-in $ [:world to :tile/entities] (fnil conj #{}) entity-id)
                   (update-in $ [:entities entity-id] assoc :entity/position to)))))


(defn create-entity [entity-id entity]
  (swap! state update :entities assoc entity-id entity))


(defn elevate-entity-to-player [entity-id]
  (swap! state assoc-in [:player :player/entity] entity-id))


(defn mark-tile-as-seen [x y]
  (swap! state assoc-in [:world [x y] :tile/seen?] true))


(defn create-and-place-tile [x y tile]
  (swap! state assoc-in [:world [x y]] tile))


(defn get-player-entity []
  (let [{:keys [player entities]} @state
        entity-id                 (:player/entity player)]
    [entity-id (get entities entity-id)]))


(defn get-entity-position [entity-id]
  (:entity/position (get-in @state [:entities entity-id])))


(defn get-tile [x y]
  (get-in @state [:world [x y]]))


(defn get-tile-entity-ids [x y]
  (get (get-tile x y) :tile/entities))


(defn get-entity [entity-id]
  (get-in @state [:entities entity-id]))


(defn get-tile-entities [x y]
  (map get-entity (get-tile-entity-ids x y)))


(defn is-player? [entity-id]
  (= entity-id
     (first (get-player-entity))))

