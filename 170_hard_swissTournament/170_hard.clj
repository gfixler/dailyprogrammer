(defn make-player [called]
  (atom {:name called :score [] :bonus [] :opponent []}))

(defn record-game [player score bonus opponent]
  (swap! player #(merge-with into % {:score [score]
                                     :bonus [bonus]
                                     :opponent [(:name @opponent)]})))

(defn final-score [] (nth [[15 5] [10 10] [5 15]] (rand-int 3)))
(defn play-bonus [] (- (rand-int 11) 5))

(defn play-game [[player1 player2]]
  (let [[score1 score2] (final-score)]
    (record-game player1 score1 (play-bonus) player2)
    (record-game player2 score2 (play-bonus) player1)))

(defn tally [player]
  (reduce + (:scores player)))

(let [players (for [i (range 1 33)] (make-player (str "Player" i)))]
  (loop [round 1
         matchups (partition 2 (shuffle players))]
    (map play-game matchups)
    (for [player players]
      (println (:name @player) (:score @player)))))

