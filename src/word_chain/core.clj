(ns word-chain.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

(def all-words (map str/lower-case (str/split (slurp (io/resource "words.txt")) #"\n")))

(defn distance
  ([a b] (distance a b 0))
  ([[a & rest-a] [b & rest-b] curr]
   (if (nil? a)
     curr
     (recur rest-a rest-b (if (= a b) curr (inc curr))))))

(defn isNeighbour?
  [word other-word]
  (= 1 (distance word other-word)))

(defn closest-neighbour
  [word words]
  (some #(when (isNeighbour? word (:word %)) (:word %)) words))

(defn find-path
  [path end words]
  (let [next-word (closest-neighbour (last path) words)
        ; Remove next word from the search space, ensure we dont get stuck in a loop
        reduced-words (remove #(= (last path) (:word %)) words)
        ; if we've hit an empty path, back up by one word and continue
        path (if (nil? next-word) (pop path) (conj path next-word))]
    ; if we've run out of words, give up, else recur
    (if (empty? path) nil
        (if (= next-word end)
          path
          (recur path end reduced-words)))))

(defn prepare-words
  [start-word end-word words]
  (let [word-count (count start-word)]
    (->> (filter #(= (count %) word-count) words)
         (map #(conj {:word % :distance (distance % end-word)}))
         (sort-by :distance))))

(defn find-chain [start-word end-word]
  (when (= (count start-word) (count end-word))
    (let [prepared-words (prepare-words start-word end-word all-words)]
      (find-path [start-word] end-word prepared-words))))


(defn -main [start-word end-word]
  (let [a (future (find-chain start-word end-word))
        b (future (find-chain end-word start-word))]
    (while (and (not (realized? a)) (not (realized? b))))
    (if (realized? a) @a (reverse @b))))

(time (-main "ruby" "code"))
(time (-main "winter" "bubble"))
