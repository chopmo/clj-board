(ns board.core
  (:require [board.trie :as t]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]
            [clojure.set :as s]))


(defn neighbours
  [board tile]
  (let [[x y] tile]
    (for [nx (range (dec x) (+ x 2))
          ny (range (dec y) (+ y 2))
          :when (and
                 (<= 0 nx (dec (count (first board))))
                 (<= 0 ny (dec (count board)))
                 (not (= [x y] [nx ny])))]
      [nx ny])))

(defn unvisited
  [visited tiles]
  (s/difference
   (set tiles)
   (set visited)))

(defn tiles
  [board]
  (for [nx (range (count (first board)))
        ny (range (count board))]
    [nx ny]))

(defn char-at
  [board tile]
  (let [[x y] tile]
    (nth (nth board y) x)))

(defn words
  [board trie visited word tile]
  (let [char (char-at board tile)
        word (str word char)
        visited (conj visited tile)]
    (when-let [sub-trie (t/children trie char)]
      (->> (neighbours board tile)
           (unvisited visited)
           (mapcat (partial words board sub-trie visited word))
           (concat (if (t/term? sub-trie) [word] []))))))

(defn all-words
  "Find all words on the board that are in the dictionary"
  [board dict]
  (let [trie (t/build dict)]
    (->> board
         tiles
         (mapcat (partial words board trie #{} "")))))

;; (count (clojure.string/split-lines (slurp "/usr/share/dict/words")))

;; (def real-dict-trie
;;   (t/build (clojure.string/split-lines (slurp "/usr/share/dict/words"))))
