(ns rotten.core
  (:require ["rot-js" :as ROT :refer [Display
                                      KEYS]]))

(defonce state (atom {:player {:x 3
                               :y 3}}))


(def display-config
  {:width  9
   :height 9
   :fontSize 40})


(def glyphs
  {:player "@"})


(defonce display
  (Display. (clj->js display-config)))


(defonce body
  (.-body js/document))


(defonce key-code->vk
  (into {} (map (fn [[k v]] [v k]) (js->clj KEYS))))


(defn move-player [direction]
  (case direction
    :left  (swap! state update-in [:player :x] dec)
    :right (swap! state update-in [:player :x] inc)
    :up    (swap! state update-in [:player :y] dec)
    :down  (swap! state update-in [:player :y] inc)))


(defn handle-input [vk]
  (case vk
    "VK_LEFT"  (move-player :left)
    "VK_RIGHT" (move-player :right)
    "VK_UP"    (move-player :up)
    "VK_DOWN"  (move-player :down)))


(defn add-keyboard-listeners []
  (.addEventListener body "keydown"
                     (fn [e]
                       (let [key-code (.-keyCode e)
                             vk       (get key-code->vk key-code)]
                         (println vk)
                         (handle-input vk)))))


(defn mount-display []
  (.appendChild body (.getContainer display)))


(defn draw
  ([x y glyph] (.draw display x y glyph))
  ([x y glyph fgcolor] (.draw display x y glyph fgcolor))
  ([x y glyph fgcolor bgcolor] (.draw display x y glyph fgcolor bgcolor)))


(defn display-player [x y]
  (draw x y (:player glyphs)))


(defn clear []
  (.clear display))


(defn render []
  (let [{player :player} @state]
    (clear)
    (display-player (:x player) (:y player)))
  (js/requestAnimationFrame render))


(defn init []
  (mount-display)
  (render)
  (add-keyboard-listeners)
  (println "initted"))


(defn reload []
  (println "reloaded"))

