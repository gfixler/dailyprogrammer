(ns inputclj.core
  (:use [clojure.string :only (replace-first)]))

(defn mad-lib
  "Pass string with embedded queries, e.g. \"Your name is {Your name?}.\"
  Asks user the questions and fills them in; returns message when done.
  Pass an extra argument - a file path - to save your story at the end."
  ([story-template] (mad-lib story-template nil))
  ([story-template file-path]
   (loop [story story-template]
     (if (re-find #"\{" story)
       (let [query (re-find #"\{.*?\}" story)]
         (do
           (println (subs query 1 (- (count query) 1)))
           (let [answer (read-line)]
             (if (= answer "")
               (recur story)
               (recur (replace-first story query answer))))))
       (do
         (if file-path
           (spit file-path story))
         story)))))

; example usage
(defn -main [& args]
  (println (mad-lib (str "your name is {What is your name?}, you are "
                         "{How old are you?} years old, and your username "
                         "is {What is your reddit username?}")
                    "/home/gfixler/madlibstory")))

