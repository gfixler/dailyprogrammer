(use '[clojure.set :only [subset?]])

(defn conversions [n a b]
  (let [unit (filter
               #(subset? (set [a b]) %)
               [{"meters" 1.0
                 "inches" 39.3701
                 "miles" 0.000621371
                 "attoparsecs" 32.4077929}
                {"kilograms" 1.0
                 "pounds" 2.20462
                 "ounces" 35.274
                 "hogsheads of beryllium" 440.7}])]
    (str n " " a
         (if-let [ratio (first unit)]
           (str " is " (* n (/ (ratio b) (ratio a))) " ")
           (str " can't be converted to "))
         b)))

(conversions 23 "pounds" "ounces")
;=> "23 pounds is 368.0008346109534 ounces"
(conversions 3.9 "kilograms" "hogsheads of beryllium")
;=> "3.9 kilograms is 1718.73 hogsheads of beryllium"
(conversions 10 "inches" "miles")
;=> "10 inches is 1.5782814877279968E-4 miles"
(conversions 5 "meters" "ounces")
;=> "5 meters can't be converted to ounces"

