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

(defn already-played [player1 player2]
  (> (count (filter #(= % (:name player2)) (:opponents player1))) 0))

(defn tally [player]
  (reduce + (map #(reduce + %) (:scores player))))

(defn pair-players-by-scores [players]
  ; TODO - ACTUALLY SORT THINGS BY SWISS RULES
  (let [paired-players (sort-by tally players)]
    (partition 2 paired-players)))

(defn play-tourney [players]
  (loop [round 1
         matchups (partition 2 (shuffle players))]
    (let [results (for [matchup matchups] (play-game matchup))]
      (if (< round 6)
        (recur (inc round) (pair-players-by-scores (flatten results)))
        results))))

(defn format-player-results [player]
  (apply str (:name player) "\t" (interpose "\t" (map #(reduce + %) (:scores player)))))

(let [players (for [i (range 1 33)] (make-player (str "P" i)))]
  (let [tourney (play-tourney players)
        ordered (reverse (sort-by tally (flatten tourney)))
        display (for [ranked ordered] (format-player-results ranked))
        header (apply str (interpose "\t" (for [i (range 1 7)] (str "Rnd" i))))]
    (print "Name\t" header "\n")
    (print (apply str (interpose "\n" display)))))
