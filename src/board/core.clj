(ns board.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]
            [clojure.set :as s]))

;; Tries
(defn update-trie
  [trie word]
  (when-let [[hd & tl] (seq word)]
    (assoc-in
     trie [:children hd]
     (let [sub-trie (get-in trie [:children hd])]
       (if (seq tl)
         (update-trie sub-trie tl)
         (assoc sub-trie :terminal? true))))))

(defn build-trie
  [dictionary]
  (reduce update-trie nil dictionary))

(defn children
  [trie char]
  (get-in trie [:children char]))

(defn term?
  [trie]
  (:terminal? trie))

;; Board structure
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

;; Solver
(defn words
  [board trie visited word tile]
  (let [char (char-at board tile)
        word (str word char)
        visited (conj visited tile)]
    (when-let [sub-trie (children trie char)]
      (->> (neighbours board tile)
           (unvisited visited)
           (mapcat (partial words board sub-trie visited word))
           (concat (if (term? sub-trie) [word] []))))))

(defn all-words
  "Find all words on the board that are in the dictionary"
  [board dict]
  (let [trie (build-trie dict)]
    (->> board
         tiles
         (mapcat (partial words board trie #{} "")))))


;; (def real-dict
;;   (clojure.string/split-lines (slurp "/usr/share/dict/words")))

;; (def demo-board
;;   [[\h \e \x \m]
;;    [\w \t \l \a]
;;    [\q \l \o \s]])
