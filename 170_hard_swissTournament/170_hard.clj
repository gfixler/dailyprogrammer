(defn make-player [player-name]
  {:name player-name :scores [] :opponents []})

(defn record-game [player score bonus opponent]
  (merge-with into player {:scores [[score bonus]]
                           :opponents [(:name opponent)]}))

(defn final-score [] (nth [[15 5] [10 10] [5 15]] (rand-int 3)))
(defn play-bonus [] (- (rand-int 11) 5))

(defn play-game [[player1 player2]]
  (let [[score1 score2] (final-score)]
    [(record-game player1 score1 (play-bonus) player2)
     (record-game player2 score2 (play-bonus) player1)]))

(defn tally [player]
  (reduce + (map #(reduce + %) (:scores player))))

(defn pair-players-by-scores [players]
  (partition 2 (shuffle players)))

(defn play-tourney [players]
  (loop [round 1
         matchups (partition 2 (shuffle players))]
    (let [results (for [matchup matchups] (play-game matchup))]
      (if (< round 6)
        (recur (inc round) (pair-players-by-scores (flatten results)))
        results))))

(let [players (for [i (range 1 33)] (make-player (str "Player" i)))]
  (play-tourney players))
