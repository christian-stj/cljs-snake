(ns site.game
  (:require [ysera.random :refer [get-random-int]]))

(defn create-game
  [size & [seed]]
  {:time   0
   :score  0
   :seed   (or seed
               1234)
   :status :live
   :size   size
   :snake  {:direction :up
            :length    1
            :body      #queue [{:x (quot size 2) :y (quot size 2)}]}
   :blobs  '()})

(defn count-free-blocks
  [state]
  (let [size (:size state)
        no-of-blocks (* size size)
        snake-length (count (get-in state [:snake :body]))]
    (- no-of-blocks snake-length)))

(defn add-random-blob
  [state]
  (let [[seed x] (get-random-int (:seed state) (:size state))
        [seed y] (get-random-int seed (:size state))]
    (count-free-blocks state)
    (if (< (count-free-blocks state) 2)
      state
      (if (not (some #(and (= (:x %) x)
                           (= (:y %) y))
                     (concat (:blobs state)
                             (-> state :snake :body))))
        (-> state
            (update :blobs #(conj % {:x x :y y}))
            (assoc :seed seed))
        (-> state
            (assoc :seed seed)
            (add-random-blob))))))

(defn grow-snake
  [state]
  (update-in state [:snake :length] inc))

(defn has-position?
  [position-list position]
  (some #(and (= (:x %) (:x position))
              (= (:y %) (:y position))) position-list))

(defn get-next-position
  [state direction]
  (let [size (:size state)
        head (last (get-in state [:snake :body]))]
    (case direction
      :up (update head :y #(mod (dec %) size))
      :down (update head :y #(mod (inc %) size))
      :right (update head :x #(mod (inc %) size))
      :left (update head :x #(mod (dec %) size)))))

(defn step
  [state]
  (let [snake-length (-> state :snake :length)
        snake (:snake state)
        body (:body snake)
        blobs (:blobs state)
        direction (get-in state [:snake :direction])
        new-position (get-next-position state direction)]
    (cond
      (= (count-free-blocks state) 0) (assoc state :status :game-won)
      (has-position? body new-position) (assoc state :status :game-over)
      :else (-> state
                (update :time inc)
                (update :score inc)
                (assoc-in [:snake :body] (conj (if (< (count body) snake-length)
                                                 body
                                                 (pop body))
                                               new-position))
                (as-> $
                      (if-not (has-position? blobs new-position)
                        $
                        (-> $
                            (assoc :blobs
                                   (remove (fn [blob] (and (= (:x blob) (:x new-position))
                                                           (= (:y blob) (:y new-position))))
                                           blobs))
                            (add-random-blob)
                            (update :score + 20)
                            (grow-snake))))))))

(defn valid-direction?
  [state direction]
  (let [next-position (get-next-position state direction)
        body (get-in state [:snake :body])]
    (has-position? body next-position)))

(defn change-direction
  [state direction]
  (if (valid-direction? state direction)
    state
    (assoc-in state [:snake :direction] direction)))