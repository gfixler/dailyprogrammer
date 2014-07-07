(defn rotate-string
  "Returns string rotated by places, e.g. [\"Hello\" -2] => \"loHel\""
  [string places]
  (let [places (mod places (count string))]
    (apply str (concat (drop places string) (take places string)))))

(defn re-words
  "Split string around words, e.g. \"Ah, so.\" => [\"Ah\" \",\" \" \" \"so\" \".\"]"
  [string]
  (re-seq #"\w+|\W+" string))

(defn alien-typo-corrections
  "Creates a map of key names mapped to keys shifted 'places' on the keyboard.
  Shifts either way wrap (a shift of 1 corrects q to w, but -2 makes it o)."
  [places]
  (apply merge (for [row ["qwertyuiop" "asdfghjkl" "zxcvbnm"
                          "QWERTYUIOP" "ASDFGHJKL" "ZXCVBNM"]]
                 (apply hash-map
                        (interleave (rotate-string row places) row)))))

(defn correct-alien-message
  "Aliens can't type for crap. This attempts to correct their nonsense"
  [msg]
  (let [dict (slurp "/usr/share/dict/words")]
    (apply str (for [word (re-words msg)]
                 (if (re-find #"\w" word)
                   (if (re-find (re-pattern (str #"\b" word #"\b")) dict)
                     (clojure.string/upper-case word))
                   word)))))

(correct-alien-message "The quick ntpem fox jumped over rgw lazy dog.")

