(def chardata {\  "00000000000000000000000000000000000"
               \A "00100010101000111111100011000110001"
               \B "11110100011000111110100011000111110"
               \C "01110100011000010000100001000101110"
               \D "11110100011000110001100011000111110"
               \E "11111100001000011110100001000011111"
               \F "11111100001000011110100001000010000"
               \G "01111100001000010011100011000101111"
               \H "10001100011000111111100011000110001"
               \I "01110001000010000100001000010001110"
               \J "00001000010000100001000011000101111"
               \K "10001100101010011000101001001010001"
               \L "10000100001000010000100001000011111"
               \M "10001110111010110001100011000110001"
               \N "10001100011100110101100111000110001"
               \O "01110100011000110001100011000101110"
               \P "11110100011000111110100001000010000"
               \Q "01110100011000110001101010111000011"
               \R "11110100011000111110101001001010001"
               \S "01110100011000001110000011000101110"
               \T "11111001000010000100001000010000100"
               \U "10001100011000110001100011000101110"
               \V "10001100011000110001100010101000100"
               \W "10001100011000110101110111000110001"
               \X "10001100010101000100010101000110001"
               \Y "10001100010101000100001000010000100"
               \Z "11111000010001000100010001000011111"})

(defn unlines [s]
  (apply str (interpose "\n" s)))

(defn bit-rows [msg]
  (->> (for [c msg] (chardata c))
       (map #(partition 5 %))
       (apply mapv vector)
       (map #(interpose \0 %))
       (map flatten)
       (map #(apply str %))))

(defn make-pbm [msg]
  (let [bits (bit-rows msg)
        width (count (first bits))
        height (count bits)
        size (str width " " height)]
    (unlines ["P1" size (unlines bits)])))

