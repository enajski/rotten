(ns rotten.core
  (:require ["rot-js" :as ROT :refer [Display
                                      KEYS]]))

(defonce state (atom {:player {:x 3
                               :y 3}
                      :game-bounds {:x-min 1
                                    :x-max 20
                                    :y-min 1
                                    :y-max 18}}))


(def display-config
  {:width    30
   :height   20
   :fontSize 30})


(def glyphs
  {:player        "@"
   :game-bounds   "*"
   :ui-horizontal "-"
   :ui-vertical   "|"
   :ui-corner     "+"})


(defonce display
  (Display. (clj->js display-config)))


(defonce body
  (.-body js/document))


(defonce key-code->vk
  (into {} (map (fn [[k v]] [v k]) (js->clj KEYS))))


(defn can-move? [direction]
  (let [{player      :player
         game-bounds :game-bounds} @state]
    (case direction
      :left  (> (:x player) (:x-min game-bounds))
      :right (> (:x-max game-bounds) (:x player))
      :up    (> (:y player) (:y-min game-bounds))
      :down  (> (:y-max game-bounds) (:y player)))))


(defn move-player [direction]
  (when (can-move? direction)
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


(defn display-player [x y]
  (draw x y (:player glyphs)))


(defn draw-corners [bounds]
  (draw (dec (:x-min bounds)) (dec (:y-min bounds)) (:ui-corner glyphs))
  (draw (dec (:x-min bounds)) (inc (:y-max bounds)) (:ui-corner glyphs))
  (draw (inc (:x-max bounds)) (dec (:y-min bounds)) (:ui-corner glyphs))
  (draw (inc (:x-max bounds)) (inc (:y-max bounds)) (:ui-corner glyphs)))


(defn draw-bounds [bounds]
  (draw-corners bounds))


(defn draw-ui [state]
  (draw-bounds (:game-bounds state)))


(defn clear []
  (.clear display))


(defn render []
  (let [{player :player} @state]
    (clear)
    (draw-ui @state)
    (display-player (:x player) (:y player)))
  (js/requestAnimationFrame render))


(defn init []
  (mount-display)
  (render)
  (add-keyboard-listeners)
  (println "initted"))


(defn reload []
  (println "reloaded"))

