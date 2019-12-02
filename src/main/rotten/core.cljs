(ns rotten.core
  (:require ["rot-js" :as ROT :refer [Display
                                      FOV
                                      KEYS
                                      Map
                                      Path]]
            [rotten.db :as db]
            [clojure.string :as str]
            [cljs.pprint :as pprint]))


(def display-config
  {:width    70
   :height   20
   :fontSize 24})


(def glyphs
  {:player        "@"
   :game-bounds   "*"
   :ui-horizontal "-"
   :ui-vertical   "|"
   :ui-corner     "+"
   :wall          "▓"
   :moon          "🌓 "
   })


(defonce display
  (Display. (clj->js display-config)))


(defonce body
  (.-body js/document))


(defonce key-code->vk
  (into {} (map (fn [[k v]] [v k]) (js->clj KEYS))))


(defn is-walkable? [x y world]
  (if (false? (:tile/walkable? (get world [x y])))
    false
    true))


(defn get-random-empty-location []
  (let [{:keys [world]} @db/state
        [[x y] _tile] (->> world
                           (shuffle)
                           (filter (fn [[[_x _y] tile]] (get tile :tile/walkable?)))
                           first)]
    [x y]))


(defn can-move? [entity-id direction]
  (let [{:keys [game-bounds
                world]} @db/state
        [x y] (db/get-entity-position entity-id)]
    (case direction
      :left  (and (> x (:x-min game-bounds))
                  (is-walkable? (dec x) y world))
      :right (and (> (:x-max game-bounds) x)
                  (is-walkable? (inc x) y world))
      :up    (and (> y (:y-min game-bounds))
                  (is-walkable? x (dec y) world))
      :down  (and (> (:y-max game-bounds) y)
                  (is-walkable? x (inc y) world))

      false)))


(defn move-player [direction]
  (let [[entity-id entity] (db/get-player-entity)
        [x y]              (:entity/position entity)]
    (when (can-move? entity-id direction)
      (case direction
        :left  (db/move-entity [x y] [(dec x) y] entity-id)
        :right (db/move-entity [x y] [(inc x) y] entity-id)
        :up    (db/move-entity [x y] [x (dec y)] entity-id)
        :down  (db/move-entity [x y] [x (inc y)] entity-id)))))


(defn handle-input [vk]
  (case vk
    "VK_LEFT"  (move-player :left)
    "VK_RIGHT" (move-player :right)
    "VK_UP"    (move-player :up)
    "VK_DOWN"  (move-player :down)

    nil))


(defn add-keyboard-listeners []
  (.addEventListener body "keydown"
                     (fn [e]
                       (let [key-code (.-keyCode e)
                             vk       (get key-code->vk key-code)]
                         (handle-input vk)))))


(defn mount-display []
  (.appendChild body (.getContainer display)))


(defn draw
  ([x y glyph] (.draw display x y glyph))
  ([x y glyph fgcolor] (.draw display x y glyph fgcolor))
  ([x y glyph fgcolor bgcolor] (.draw display x y glyph fgcolor bgcolor)))


(defn draw-text
  ([x y text] (.drawText display x y text))
  ([x y text line-length] (.drawText display x y text line-length)))


(defn draw-player [x y]
  (draw x y (:player glyphs) "goldenrod"))


(defn draw-character [x y {:keys [:entity/name]}]
  (draw x y (str (first name))))


(defonce AStar
  (.. Path -AStar))


(defn PathHandler [target-x target-y]
  (AStar. target-x target-y is-walkable?))


;; (defn pick-a-stroll-route [from-x from-y]
;;   (let [[target-x target-y] (get-random-empty-location)]
;;     (.compute (PathHandler target-x target-y)
;;               from-x from-y
;;               (fn [x y]
;;                 #_(swap! )))))


(defonce RecursiveShadowcasting
  (.. FOV -RecursiveShadowcasting))


(defn light-passes? [x y]
  (let [world (:world @db/state)]
    (if-let [tile (get world [x y])]
      (:tile/walkable? tile)
      true)))


(defonce FOVHandler
  (RecursiveShadowcasting. light-passes?))


(defonce DividedMaze
  (.. Map -DividedMaze))


(defn generate-maze [{:keys [game-bounds]}]
  (let [maze (DividedMaze. (:x-max game-bounds) (:y-max game-bounds))]
    (.create maze
             (fn [x y contents]
               (let [tile (if (= 1 contents)
                            {:tile/kind      :tile.kind/wall
                             :tile/walkable? false}
                            {:tile/kind      :tile.kind/floor
                             :tile/walkable? true})]
                 (db/create-and-place-tile (+ x (:x-min game-bounds))
                                           (+ y (:y-min game-bounds))
                                           tile))))))


(defn draw-wall [x y]
  (draw x y (:wall glyphs)))


(defn draw-empty-space [x y]
  (draw x y "" "#000" "rgba(250,150,60,0.4)"))


(defn draw-tile [x y tile]
  (case (:tile/kind tile)
    :tile.kind/wall (draw-wall x y)

    :tile.kind/floor (draw-empty-space x y)

    (draw-empty-space x y)))


(defn draw-entity [x y entity]
  (case (:entity/kind entity)
    :entity.kind/player (draw-player x y)

    :entity.kind/character (draw-character x y entity)))


(defn with-360-fov [origin-x origin-y range world]
  (.compute FOVHandler origin-x origin-y range
            (fn [x y r _visibility]

              (db/mark-tile-as-seen x y)

              (draw-tile x y (get world [x y]))

              (doseq [entity (db/get-tile-entities x y)]
                (draw-entity x y entity)))))


(defn display-seen [world]
  (doseq [[[x y] tile] world]
    (if (:tile/seen? tile)
      (draw-tile x y tile))))


(defn display-world [{:keys [world]}]
  (let [[_entity-id entity] (db/get-player-entity)
        [x y]               (:entity/position entity)]
    (display-seen world)
    (with-360-fov x y 10 world)))


(defn draw-corner [x y]
  (draw x y (:ui-corner glyphs)))


(defn draw-vertical-line [x y]
  (draw x y (:ui-vertical glyphs)))


(defn draw-horizontal-line [x y]
  (draw x y (:ui-horizontal glyphs)))


(defn is-corner-surrounding-game-bounds? [x y bounds]
  (and (#{(dec (:x-min bounds)) (inc (:x-max bounds))} x)
       (#{(dec (:y-min bounds)) (inc (:y-max bounds))} y)))


(defn is-left-side-surrounding-game-bounds? [x y bounds]
  (and (= (dec (:x-min bounds)) x)
       ((into #{} (range (:y-min bounds) (inc (:y-max bounds)))) y)))


(defn is-right-side-surrounding-game-bounds? [x y bounds]
  (and (= (inc (:x-max bounds)) x)
       ((into #{} (range (:y-min bounds) (inc (:y-max bounds)))) y)))


(defn is-top-side-surrounding-game-bounds? [x y bounds]
  (and (= (dec (:y-min bounds)) y)
       ((into #{} (range (:x-min bounds) (inc (:x-max bounds)))) x)))


(defn is-bottom-side-surrounding-game-bounds? [x y bounds]
  (and (= (inc (:y-max bounds)) y)
       ((into #{} (range (:x-min bounds) (inc (:x-max bounds)))) x)))


(defn draw-outline [bounds]
  (doseq [x (range 0 (inc (inc (:x-max bounds))))
          y (range 0 (inc (inc (:y-max bounds))))]
    (cond

      (is-corner-surrounding-game-bounds? x y bounds)
      (draw-corner x y)


      (or (is-left-side-surrounding-game-bounds? x y bounds)
          (is-right-side-surrounding-game-bounds? x y bounds))
      (draw-vertical-line x y)


      (or (is-top-side-surrounding-game-bounds? x y bounds)
          (is-bottom-side-surrounding-game-bounds? x y bounds))
      (draw-horizontal-line x y))))


(defn draw-bounds [{:keys [game-bounds]}]
  (draw-outline game-bounds))


(defn draw-turn-number [{:keys [game-bounds turn]}]
  (draw-text (+ 3 (:x-max game-bounds)) 0 (str "Turn: " turn)))


(defn draw-cast-list [{:keys [entities game-bounds]}]
  (draw-text (+ 3 (:x-max game-bounds)) 2 (str "Cast: " (map #(:entity/name (second %)) entities))))


(defn draw-ui [state]
  (draw-bounds state)
  (draw-turn-number state)
  (draw-cast-list state))


(defn place-player []
  (let [entity {:entity/kind :entity.kind/player
                :entity/name "The Player"}
        entity-id (random-uuid)
        [x y] (get-random-empty-location)]
    (db/create-entity entity-id entity)
    (db/place-entity [x y] entity-id)
    (db/elevate-entity-to-player entity-id)))


(defn place-cast []
  (doseq [character ["Neil" "Karen"]]
    (let [[x y] (get-random-empty-location)
          entity-id (random-uuid)
          entity {:entity/kind :entity.kind/character
                  :entity/name character}]
      (db/create-entity entity-id entity)
      (db/place-entity [x y] entity-id))))


(defn display-game-world [state]
  (display-world state)
  #_(display-player player))


(defn clear []
  (.clear display))


(defn render []
  (let [frame-state @db/state]
    (clear)
    (draw-ui frame-state)
    (display-game-world frame-state)
  (js/requestAnimationFrame render)))


(defn init []
  (mount-display)
  (generate-maze @db/state)
  (place-player)
  (place-cast)
  (println @db/state)
  (render)
  (add-keyboard-listeners)
  (println "initted"))


(defn reload []
  (println "reloaded"))

