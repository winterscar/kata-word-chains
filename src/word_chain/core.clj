(ns word-chain.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

(use 'clojure.data)

(def all-words (map str/lower-case (str/split (slurp (io/resource "words.txt")) #"\n")))

(defn contains-str?
  [coll string]
  (not (nil? (some #{string} coll))))

(defn distance
  [word other-word]
  (let [word (seq word)
        other-word (seq other-word)
        word-length (count word)]
    (-> (diff word other-word)      
        (nth 2)               ; things that are in both sets
        (->> (remove nil?))   ; diff pads with nil to keep position
        (count)
        (->> (- word-length)))))

(defn isNeighbour?
  [word other-word]
    (= 1 (distance word other-word) ))

(defn get-neighbours
  [word words]
  (filter #(isNeighbour? word %1) words))

(defn find-path
  ([start end words] (find-path start end words [start]))
  ([start end words path]
    (let [neighbours
         (->> (get-neighbours start words)
              (sort-by #(distance %1 end)))]
      (if (contains-str? neighbours end)
        (conj path end)
        (find-path (first neighbours) end words (conj path (first neighbours)))))))
 
(defn -main [start-word end-word]
  (if (= (count start-word) (count end-word))
   (let [words-of-correct-len (filter #(= (count %) (count start-word)) all-words)]
    (println (find-path start-word end-word words-of-correct-len)))))