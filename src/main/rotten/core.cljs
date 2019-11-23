(ns rotten.core
  (:require ["rot-js" :as ROT :refer [Display
                                      FOV
                                      KEYS
                                      Map]]
            [clojure.string :as str]))

(defonce state (atom {:player      {:x 3
                                    :y 3}
                      :cast        [{:name "Neil"
                                     :x    10
                                     :y    10}
                                    {:name "Karen"
                                     :x    12
                                     :y    12}]
                      :game-bounds {:x-min 1
                                    :x-max 20
                                    :y-min 1
                                    :y-max 18}
                      :turn        0
                      :world       {}}))


(def display-config
  {:width    40
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
  (if (false? (:walkable? (get world [x y])))
    false
    true))


(defn can-move? [direction]
  (let [{:keys [player
                game-bounds
                world]} @state
        {:keys [x y]} player]
    (case direction
      :left  (and (> x (:x-min game-bounds))
                  (is-walkable? (dec x) y world))
      :right (and (> (:x-max game-bounds) (:x player))
                  (is-walkable? (inc x) y world))
      :up    (and (> (:y player) (:y-min game-bounds))
                  (is-walkable? x (dec y) world))
      :down  (and (> (:y-max game-bounds) (:y player))
                  (is-walkable? x (inc y) world))

      false)))


(defn move-player [direction]
  (when (can-move? direction)
    (swap! state (fn [old] (update old :turn inc)))
    (case direction
      :left  (swap! state update-in [:player :x] dec)
      :right (swap! state update-in [:player :x] inc)
      :up    (swap! state update-in [:player :y] dec)
      :down  (swap! state update-in [:player :y] inc))))


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


(defn display-player [{:keys [x y]}]
  (draw x y (:player glyphs) "goldenrod"))


(defn draw-character [{:keys [x y name]}]
  (draw x y (str (first name))))


(defn display-cast [{:keys [cast]}]
  (doseq [character cast]
    (draw-character character)))


(defonce RecursiveShadowcasting
  (.. FOV -RecursiveShadowcasting))


(defn light-passes? [x y]
  (let [world (:world @state)]
    (if-let [tile (get world [x y])]
      (:walkable? tile)
      true)))


(defonce FOVHandler
  (RecursiveShadowcasting. light-passes?))


(defonce DividedMaze
  (.. Map -DividedMaze))


(defn generate-dungeon [{:keys [game-bounds]}]
  (let [maze (DividedMaze. (:x-max game-bounds) (:y-max game-bounds))]
    (.create maze
             (fn [x y contents]
               (when (= 1 contents)
                 (swap! state update :world
                        assoc [(+ x (:x-min game-bounds))
                               (+ y (:y-min game-bounds))]
                        {:kind      :wall
                         :walkable? false}))))))


(defn draw-wall [x y]
  (draw x y (:wall glyphs)))


(defn draw-empty-space [x y]
  (draw x y "" "#000" "rgba(250,150,60, 0.4)"))


(defn draw-tile [x y tile]
  (case (:kind tile)
    :wall (draw-wall x y)

    (draw-empty-space x y)))


(defn with-360-fov [x y range world]
  (.compute FOVHandler x y range
            (fn [x y r _visibility]

              (if-let [tile (get world [x y])]
                (draw-tile x y tile)
                (draw-empty-space x y)))))


(defn display-world [{:keys [world
                             player]}]
  (let [{:keys [x y]} player]
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


(defn draw-cast-list [{:keys [cast game-bounds]}]
  (draw-text (+ 3 (:x-max game-bounds)) 2 (str "Cast: " (str/join ", " (map :name cast)))))


(defn draw-ui [state]
  (draw-bounds state)
  (draw-turn-number state)
  (draw-cast-list state))


(defn display-game-world [{:keys [player] :as state}]
  (display-world state)
  (display-player player)
  (display-cast state))


(defn clear []
  (.clear display))


(defn render []
  (let [frame-state @state]
    (clear)
    (draw-ui frame-state)
    (display-game-world frame-state)
  (js/requestAnimationFrame render)))


(defn init []
  (mount-display)
  (render)
  (add-keyboard-listeners)
  (generate-dungeon @state)
  (println "initted"))


(defn reload []
  (println "reloaded"))

