(defn hex->bin [hex]
  (Integer/toBinaryString (Integer/parseInt (str hex) 16)))

(defn gather-hex [digits]
  (re-seq #"[0-9A-Fa-f]+" digits))

(defn hex-dots [v]
  {:pre [(<= 0 v 255)]}
  (loop [i 7 s ""]
    (if (< i 0)
      s
      (if (< v (Math/pow 2 i))
        (recur (dec i) (str " " s))))))

(hex-dots 55)

(let [hexes (gather-hex "FF EA 97 00 11 FF C3 55")]
  (map (comp hex->bin) hexes))

