(defn get-winner [[[p1 p1s] [p2 p2s]]]
  (if (> p1s p2s) p1 p2))

(defn get-game-results []
  (let [possible [[15 5] [10 10] [5 15]]
        actual (rand-int 3)
        [score1 score2] (nth possible actual)]
    [(+ score1 (- (rand-int 11) 5))
     (+ score2 (- (rand-int 11) 5))]))

(defn play-tourney [pairs]
  (let [rounds (atom [])]
    (loop [pairs (partition 2 (shuffle players))]
      (let [results (for [pair pairs]
                      (sort-by last (map vector pair (get-game-results))))]
        (swap! rounds conj results)
        (let [winners (map (comp first first) results)]
          winners)))))

(let [players (for [i (range 32)] (str "player" (inc i)))]
  (play-tourney players))
