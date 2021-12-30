(ns site.core
  (:require [reagent.dom :as rdom]
            [reagent.core :as r]
            [site.game :refer [add-random-blob
                               create-game
                               change-direction
                               grow-snake
                               has-position?
                               step]]))

(println "Loaded!")
(js/console.log "Reloaded!")

(defonce game-atom (r/atom nil))
(defonce time-atom (atom nil))

(defn get-rect-size
  [element]
  (let [bounding-client-rect (.getBoundingClientRect element)]
    {:height (.-height bounding-client-rect)
     :width  (.-width bounding-client-rect)}))

(defn get-screen-size
  []
  {:height (.-innerHeight js/window)
   :width  (.-innerWidth js/window)})

(defn pause-game!
  []
  (swap! time-atom #(js/clearInterval %)))

(defn start-game!
  []
  (do
    (pause-game!)
    (reset! time-atom
            (js/setInterval #(swap! game-atom step) 350))))

(defn reset-game!
  [& [game-size]]
  (do
    (pause-game!)
    (reset! game-atom (create-game (or game-size 7) (js/Date.now)))
    (swap! game-atom add-random-blob)
    (reset! time-atom nil)))

(def comic-sans ["Comic Sans MS", "Comic Sans", "cursive"])

(defn game-component
  [size]
  (let [game @game-atom]
    [:div
     [:h2 {:style {:font-family comic-sans}} (str "Score: " (quot (:score game) 2))]
     [:input {:type     "button" :value "Start game!"
              :on-click start-game!}]
     [:input {:type     "button" :value "Pause game!"
              :on-click pause-game!}]
     [:input#sizebox {:type "text" :placeholder "Game size"}]
     [:input {:type     "button" :value "Restart game!"
              :on-click #(reset-game! (let [maybe-int
                                            (js/parseInt
                                              (-> "sizebox"
                                                  js/document.getElementById
                                                  .-value))]
                                        (when (int? maybe-int) maybe-int)))}]
     [:input {:type     "button" :value "Grow snake!"
              :on-click #(swap! game-atom grow-snake)}]
     [:div#game-box
      (let [rect (get-screen-size)
            screen-size (min (:width rect)
                             (quot (:height rect) 2))]
        (when (= (:status game) :game-over)
          (pause-game!)
          [:h1 {:style {:font-family comic-sans}} "Game over!"])
        (when (= (:status game) :game-won)
          (pause-game!)
          [:h1 {:style {:font-family comic-sans}} "You won the game!"])
        (->> (range size)
             (map (fn [y]
                    [:div {:key   y
                           :style {:display "flex"}}
                     (->> (range size)
                          (map (fn [x]
                                 [:div
                                  {:key   x
                                   :style {:width            (/ screen-size size)
                                           :height           (/ screen-size size)
                                           :background-color "white"
                                           :border           "1px solid white"
                                           :box-sizing       "border-box"}}
                                  (let [snake (:snake game)
                                        body (:body snake)
                                        blobs (:blobs game)]
                                    [:div {:style {:position         "absolute"
                                                   :width            (/ screen-size size)
                                                   :height           (/ screen-size size)
                                                   :background-color (let [head (last body)]
                                                                       (cond
                                                                         (and (= (:x head) x)
                                                                              (= (:y head) y)) "indianred"
                                                                         (has-position? body {:x x :y y}) "lightcoral"
                                                                         :default "lightgray"))
                                                   :border-radius    (if (has-position? blobs {:x x :y y})
                                                                       "100%"
                                                                       "30%")
                                                   :transition       "all 300ms ease"
                                                   :transform        (cond (some #(and (= (:x %) x)
                                                                                       (= (:y %) y))
                                                                                 body) "scale(1)"
                                                                           (has-position? blobs {:x x :y y}) "scale(0.66)"
                                                                           :default "scale(0.2)")}}])])))]))))]]))

(defn my-app-component
  []
  [:div
   [:h1 {:style {:font-family comic-sans}} "Snek"]
   [:p {:style {:font-family comic-sans}} "Snaky game"]
   [:div [game-component (:size @game-atom)]]])

(defn render
  []
  (rdom/render [my-app-component] (js/document.getElementById "app")))

(js/addEventListener "resize" render)
(js/addEventListener "keydown" (fn [event]
                                 (case (.-code event)
                                   "ArrowUp" (swap! game-atom change-direction :up)
                                   "ArrowDown" (swap! game-atom change-direction :down)
                                   "ArrowRight" (swap! game-atom change-direction :right)
                                   "ArrowLeft" (swap! game-atom change-direction :left)
                                   "KeyP" pause-game!
                                   :else)))

(reset-game!)
(render)

(defn ^:export main []
  (reset-game!)
  (render))