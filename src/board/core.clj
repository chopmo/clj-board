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

(defn sub-trie
  [trie char]
  (get-in trie [:children char]))

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

(defn words-here
  [trie word]
  (flatten (when (:terminal? trie)
             (list word))))

(defn words
  [board trie path word tile]
  (when-let [sub-trie (sub-trie trie (char-at board tile))]
    (let [path (conj path tile)
          char (char-at board tile)
          word (str word char)
          neighbor-words (->> (neighbours board tile)
                              (unvisited path)
                              (mapcat (partial words board sub-trie path word)))]
      (concat
       (words-here sub-trie word)
       neighbor-words))))

(defn all-words
  [board trie]
  (->> board
       tiles
       (mapcat (partial words board trie [] ""))))

(deftest board-test
  (testing "Finding words"
    (let [dict-trie (trie ["i" "bi" "bib"])
          board [[\b \i]]]
      (is (=
           (set (all-words board dict-trie))
           (set ["bi" "i"])))))
  (testing "Finding more words"
    (let [dict ["hello"
                "world"
                "xmas"
                "wello"]
          board [[\h \e \x \m]
                 [\w \t \l \a]
                 [\q \l \o \s]]
          words ["hello" "xmas" "wello"]]
      (is (= (set words)
             (set (all-words board (trie dict))))))))

(board-test)

;; (count (clojure.string/split-lines (slurp "/usr/share/dict/words")))

;; (time (do (trie (clojure.string/split-lines (slurp "/usr/share/dict/words")))
;;             nil))
