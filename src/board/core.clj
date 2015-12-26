(ns board.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]
            [clojure.set :as s]))

(defn update-trie
  [trie word]
  (when-let [[hd & tl] (seq word)]
    (assoc-in
     trie [:children hd]
     (let [sub-trie (get-in trie [:children hd])]
       (if (seq tl)
         (update-trie sub-trie tl)
         (assoc sub-trie :terminal? true))))))

(defn trie
  [dictionary]
  (reduce update-trie nil dictionary))

(defn lookup
  [trie word]
  (when trie
    (let [[hd & tl] (seq word)]
      (let [sub-trie (get-in trie [:children hd])]
        (if tl
          (lookup sub-trie tl)
          (:terminal? sub-trie))))))

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
  (when-let [sub-trie (get-in trie [:children (char-at board tile)])]
    (let [visited (conj visited tile)
          char (char-at board tile)
          word (str word char)
          word-here? (:terminal? sub-trie)]
      (->> (neighbours board tile)
           (unvisited visited)
           (mapcat (partial words board sub-trie visited word))
           (concat (if word-here? [word] []))))))

(defn all-words
  [board trie]
  (->> board
       tiles
       (mapcat (partial words board trie #{} ""))))

;; (count (clojure.string/split-lines (slurp "/usr/share/dict/words")))

;; (time (do (trie (clojure.string/split-lines (slurp "/usr/share/dict/words")))
;;             nil))
