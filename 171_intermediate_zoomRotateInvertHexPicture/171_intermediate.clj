(defn lines
  "Splits input string around newlines"
  [image]
  (clojure.string/split image #"\n"))

(defn unlines
  "Joins strings with newlines"
  [lines]
  (apply str (interpose "\n" lines)))

(defn zoom-in
  "Doubles lines and characters of input string"
  [image]
  (unlines
    (for [row (lines image)]
      (let [rrooww (apply str (interleave row row))]
        (str rrooww "\n" rrooww)))))

(defn rotate
  "'Rotates' input string 90° clockwise"
  [image]
  (unlines
    (for [line (apply map vector (lines image))]
      (clojure.string/join (reverse line)))))

(defn invert
  "Toggle existence of x in input image"
  [image]
  (reduce #(str % (cond (= %2 \x) " "
                        (= %2 \ ) \x
                        :else %2))
          "" image))

(defn every-other
  "Removes every other thing in a collection"
  [coll]
  (map first (partition 2 coll)))

(defn zoom-out
  "Removes every other character and line in input string"
  [image]
  (unlines
    (every-other (for [line (lines image)]
                    (apply str (every-other line))))))

(defn intermediate171 [image]
  ((comp zoom-out invert zoom-in rotate zoom-in) image))

