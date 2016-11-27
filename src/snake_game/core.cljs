(ns snake_game.core
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [reg-event-db reg-sub-raw subscribe dispatch dispatch-sync]]
            [goog.events :as events]))

(enable-console-print!)

(def board [50 40])

(defn logjs
    "This function prints an argument to the js console"
    [argument]
    (.log js/console (clj->js argument)))

(defn rand-free-position
  "This function takes the snake, locations of the sweets and the board-size as arguments, and returns a random position not colliding with the snake body or sweets"
  [snake locations [x y]]
  (let [positions-set (concat  (into #{} (:body snake)) locations)
        board-positions (for [x-pos (range x)
                              y-pos (range y)]
                          [x-pos y-pos])
        free-position? (atom (rand-nth board-positions))]
        (while (some #(= @free-position? %) positions-set)(reset! free-position? (rand-nth board-positions)))
        @free-position?))

(defn valid-head
  "Change the value of the head if it may run out of the board"
  [head board]
  (cond
    (= (first head) -1) [(- (first board) 1) (second head)]
    (= (first head) (first board)) [0 (second head)]
    (= (second head) -1) [(first head) (- (second board) 1)]
    (= (second head) (second board)) [(first head) 0]
    :else head))

(defn grow-snake
  "Computes a value for the tail position and returns whole snake"
  [{:keys [body] :as snake}]
  (let [last-2 (take-last 2 body)]
    (let [direction (mapv - (second last-2) (first last-2))]
      (assoc snake :body (conj body (mapv + (last body) direction))))))

(defn rand-snake
    "this function creates a new random snake, based only on the board"
    [[x y]]
    (let [valid-directons [[0 1][0 -1][-1 0][1 0]]
            snake (atom {})
            start-position [[(rand-int x)(rand-int y)]]]
    (reset! snake (assoc @snake :direction (rand-nth valid-directons)))
    (reset! snake (assoc @snake :body (conj start-position (valid-head (mapv + (:direction @snake) start-position) [x y]))))
    (dotimes [n 4] (swap! snake grow-snake))
    @snake
    ))



(def snake {:direction [1 0]
            :body      [[3 2] [2 2] [1 2] [0 2]]})

(def sweets {:max-number 20
             :locations []})

(def initial-state {:board             board
                    :snake             (rand-snake board)
                    :sweets            sweets
                    :points            0
                    :game-running?     false
                    :direction-changed false
                    :stored-direction  false})

(def restart-state {:board             board
                    :snake             (rand-snake board)
                    :sweets            sweets
                    :points            0
                    :game-running?     true
                    :direction-changed false
                    :stored-direction  false})

(reg-event-db
  :initialize
  (fn
    [db _]
    (merge db initial-state)))

(defn regsub
  [field]
  (reg-sub-raw
    field
    (fn
      [db _]
      (reaction (field @db)))))

(regsub :board)
(regsub :sweets)
(regsub :points)
(regsub :snake)
(regsub :game-running?)

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
         (js/setInterval #(dispatch [:next-state]) 150))

(defn collisions
  "Todo this should be changed to check if the head isn't hitting another snake, itself is fine."
  [snake]
  (let [head (first (:body snake))
        body (rest (:body snake))]
    (some #(= head %) body)))

(defn remove-sweet
  "Remove a certain sweet cause it's been eaten"
  [{:keys [locations] :as sweets} sweet]
  (assoc sweets :locations (remove #{sweet} locations)))

(defn process-movement
  "Handles movement stuff"
  [{:keys [snake sweets] :as db-before}]
  (let [db (assoc db-before :direction-changed false)
        sweet (some #{(first (:body snake))}  (:locations sweets))]
    (if sweet
      (-> db
          (update :snake grow-snake)
          (update :points inc)
          (update :sweets remove-sweet sweet))
      db)))

(defn handle-sweets
  "Adds new sweet if there are less sweets than the max number, removes the oldest one otherwhise"
  [{:keys [max-number locations] :as sweets} snake board]
  (if (> max-number (count locations))
    (update-in sweets [:locations] #(conj locations (rand-free-position snake locations board)))
    (update-in sweets [:locations] #(remove #{(last locations)}  locations))))

(defn pop-stored-direction
  [{:keys [stored-direction direction-changed] :as db}]
  (if (false? direction-changed)
    (if (false? stored-direction)
      db
      (-> db
          (assoc-in [:snake :direction] stored-direction)
          (assoc :stored-direction false)))
    db))

(reg-event-db
  :change-direction
  (fn [{:keys [snake direction-changed] :as db} [_ new-direction]]
    (if (not direction-changed)
      (if (not= (map #(* % -1) (:direction snake)) new-direction)
        (-> db
            (assoc-in [:snake :direction] new-direction)
            (assoc :direction-changed true))
        db)
      (assoc db :stored-direction new-direction))))

(reg-event-db
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
                  (process-movement after-move))
            (update :sweets handle-sweets snake board)))
      db)))

(reg-event-db
  :switch-game-running
  (fn
    [{:keys [game-running? snake] :as db} _]
        (if (collisions snake)
        (merge db restart-state)
        (assoc-in db [:game-running?] (not (:game-running? db)))
        )))

(defn render-board
  "Renders the board area of the game"
  []
  (let [board (subscribe [:board])
        snake (subscribe [:snake])
        sweets (subscribe [:sweets])]
    (fn []
      (let [[width height] @board
            snake-positions (into #{} (:body @snake))
            sweet-positions (into #{} (:locations @sweets))
            cells (for [y (range height)]
                    (into [:div.row.flex-items-xs-center]
                          (for [x (range width)
                                :let [current-pos [x y]]]
                            (cond
                              (sweet-positions current-pos) [:div.col-xs.board-element.sweet]
                              (snake-positions current-pos) [:div.col-xs.board-element.snake-on-cell]
                              :else [:div.col-xs.board-element.cell]))))]
        (into [:div.container]
              cells)))))

(defn score
  "Renders the player's score"
  []
  (let [points (subscribe [:points])]
    (fn
      []
      [:div.score (str "Score: " @points)])))

(defn start-stop
  "Renders the button to start/pause the game"
  []
  (let [game-running? (subscribe [:game-running?])]
    (fn
      []
      [:button.btn.btn-primary {:type "button" :on-click #(dispatch [:switch-game-running])}
      (str (if @game-running? "Pause" "Start"))
      ])))

(defn game
  "The main rendering function"
  []
  [:div
   [:div.container.controls [:div.row.flex-items-xs-center
       [:div.col-xs.board-element [score]]
       [:div.col-xs.board-element [start-stop]]
    ]]
   [render-board]])

(defn run
  "The main app function"
  []
  (dispatch-sync [:initialize])
  (reagent/render [game]
                  (js/document.getElementById "app")))

(run)