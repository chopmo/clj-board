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
  [board x y]
  (for [nx (range (dec x) (+ x 2))
        ny (range (dec y) (+ y 2))
        :when (and
               (<= 0 nx (dec (count (first board))))
               (<= 0 ny (dec (count board)))
               (not (= [x y] [nx ny])))]
    [nx ny]))

(defn unvisited
  [tiles visited]
  (let [st (set tiles)
        sv (set visited)]
    (s/difference st sv)))

(defn tiles
  [board]
  (for [nx (range (count (first board)))
        ny (range (count board))]
    [nx ny]))

(defn char-at
  [board tile]
  (let [[x y] tile]
    (nth (nth board y) x)))

(defn word-here
  [board trie path]
  (if (:terminal? trie)
    (let [chars (map #(char-at board %) path)]
      (apply str chars))))

(defn words-here
  [board trie path]
  (let [w (word-here board trie path)]
    (if w [w] [])))

(defn words
  [board trie path tile]
  (let [[x y] tile
        c (char-at board tile)
        sub-trie (get-in trie [:children c])
        path (conj path tile)]
    (when sub-trie
      (let [unvisited-neighbors (unvisited (neighbours board x y) path)
            neighbor-words (mapcat
                            (partial words board sub-trie path)
                            unvisited-neighbors)]
        (concat
         (words-here board sub-trie path)
         neighbor-words)))))

(defn all-words
  [board trie]
  (mapcat (partial words board trie []) (tiles board)))

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
