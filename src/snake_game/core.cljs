(ns snake_game.core
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [register-handler register-sub subscribe dispatch dispatch-sync]]
            [goog.events :as events]))

(enable-console-print!)

(def board [150 100])

(defn rand-free-position
  "This function takes the snake and the board-size as arguments, and returns a random position not colliding with the snake body"
  [snake [x y]]
  (let [snake-positions-set (into #{} (:body snake))
        board-positions (for [x-pos (range x)
                              y-pos (range y)]
                          [x-pos y-pos])]
    (when-let [free-positions (seq (remove snake-positions-set board-positions))]
      (rand-nth free-positions))))

(def snake {:direction [1 0]
            :body      [[4 2] [3 2] [2 2] [1 2] [0 2]]})


(def initial-state {:board             board
                    :snake             snake
                    :point             (rand-free-position snake, board)
                    :points            0
                    :game-running?     true
                    :direction-changed false
                    :stored-direction  false})

(register-handler
  :initialize
  (fn
    [db _]
    (merge db initial-state)))

(defn regsub
  [field]
  (register-sub
    field
    (fn
      [db _]
      (reaction (field @db)))))

(regsub :board)
(regsub :points)
(regsub :snake)
(regsub :point)
(regsub :game-running?)

(defn valid-head
  "Change the value of the head if it may run out of the board"
  [head board]
  (cond
    (= (first head) -1) [(- (first board) 1) (second head)]
    (= (first head) (first board)) [0 (second head)]
    (= (second head) -1) [(first head) (- (second board) 1)]
    (= (second head) (second board)) [(first head) 0]
    :else head))

(defn move-snake
  "Move the whole snake positions and directions of all snake elements"
  [{:keys [direction body] :as snake}]
  (let [head-new-position (valid-head (mapv + direction (first body)) board) ]
    (update-in snake [:body] #(into [] (drop-last (cons head-new-position body))))))

(def key-code->move
  "Mapping from the integer key code to the direction vector corresponding to that key"
  {38 [0 -1]
   40 [0 1]
   39 [1 0]
   37 [-1 0]})

(defonce key-handler
         (events/listen js/window "keydown"
                        (fn [e]
                          (let [key-code (.-keyCode e)]
                            (when (contains? key-code->move key-code)
                              (dispatch [:change-direction (key-code->move key-code)]))))))

(defonce snake-moving
         (js/setInterval #(dispatch [:next-state]) 50))

(defn collisions
  "Todo this should be changed to check if the head isn't hitting another snake."
  [snake]
  (let [head (first (:body snake))
        body (rest (:body snake))]
    (some #(= head %) body)))

(defn grow-snake
  "Computes a value for the tail position and returns whole snake"
  [{:keys [body] :as snake}]
  (let [last-2 (take-last 2 body)]
    (let [direction (mapv - (second last-2) (first last-2))]
      (assoc snake :body (conj body (mapv + (last body) direction))))))

(defn process-movement
  "Handles movement stuff"
  [{:keys [snake point board] :as db-before}]
  (let [db (assoc db-before :direction-changed false)]
    (if (= (first (:body snake)) point)
      (-> db
          (update :snake grow-snake)
          (update :points inc)
          (assoc :point (rand-free-position snake board)))
      db)))

(defn pop-stored-direction
  [{:keys [stored-direction direction-changed] :as db}]
  (if (false? direction-changed)
    (if (false? stored-direction)
      db
      (-> db
          (assoc-in [:snake :direction] stored-direction)
          (assoc :stored-direction false)))
    db))

(register-handler
  :change-direction
  (fn [{:keys [snake direction-changed] :as db} [_ new-direction]]
    (if (not direction-changed)
      (if (not= (map #(* % -1) (:direction snake)) new-direction)
        (-> db
            (assoc-in [:snake :direction] new-direction)
            (assoc :direction-changed true))
        db)
      (assoc db :stored-direction new-direction))))

(register-handler
  :next-state
  (fn
    [{:keys [snake board] :as db} _]
    (if (:game-running? db)
      (if (collisions snake)
        (assoc-in db [:game-running?] false)
        (-> db
            (pop-stored-direction)
            (update :snake move-snake)
            (as-> after-move
                  (process-movement after-move))))
      db)))

(defn render-board
  "Renders the board area of the game"
  []
  (let [board (subscribe [:board])
        snake (subscribe [:snake])
        point (subscribe [:point])]
    (fn []
      (let [[width height] @board
            snake-positions (into #{} (:body @snake))
            current-point @point
            cells (for [y (range height)]
                    (into [:tr]
                          (for [x (range width)
                                :let [current-pos [x y]]]
                            (cond
                              (= current-pos current-point) [:td.point]
                              (snake-positions current-pos) [:td.snake-on-cell]
                              :else [:td.cell]))))]
        (into [:table.stage {:style {:height 637
                                     :width  1027}}]
              cells)))))

(defn score
  "Renders the player's score"
  []
  (let [points (subscribe [:points])]
    (fn
      []
      [:div.score (str "Score: " @points)])))

(defn game-over
  "Renders the game overlay if the game is finished"
  []
  (let [game-state (subscribe [:game-running?])]
    (fn
      []
      (if @game-state
        [:div]
        [:div.overlay
         [:div.play
          [:h1 "↺"]]]))))

(defn game
  "The main rendering function"
  []
  [:div
   [render-board]
   [score]
   [game-over]])

(defn run
  "The main app function"
  []
  (dispatch-sync [:initialize])
  (reagent/render [game]
                  (js/document.getElementById "app")))

(run)