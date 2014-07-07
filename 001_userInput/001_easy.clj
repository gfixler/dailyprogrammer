(defmacro interrogate [& queries]
  `(for [query# [~@queries]]
     (do
       (println query#)
       (read-line))))

(let [[n, a, rn] (interrogate "What is your name?"
                              "How old are you?"
                              "What is your reddit username?")]
  (println (str "your name is " n ", you are " a
                " years old, and your username is " rn)))
