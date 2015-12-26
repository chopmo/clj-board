(ns board.core
  (:require [clojure.pprint]
            [clojure.test :refer :all]
            [clojure.set :as s]])

;; (def dictionary
;;   ["hello"
;;    "world"
;;    "xmas"
;;    "wello"])

;; (def board
;;   [[\h \e \x \m]
;;    [\w \t \l \a]
;;    [\q \l \o \s]])

;; hello, xmas, wello

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

#_(count (clojure.string/split-lines (slurp "/usr/share/dict/words")))

#_(time (do (trie (clojure.string/split-lines (slurp "/usr/share/dict/words")))
          nil))

(defn lookup
  [trie word]
  (when trie
    (let [[hd & tl] (seq word)]
      (let [sub-trie (get-in trie [:children hd])]
        (if tl
          (lookup sub-trie tl)
          (:terminal? sub-trie))))))


(lookup
 (trie ["hello" "world"])
 "world")

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
        sv (set visited)
        (s/difference st sv)]))

(defn tiles
  [board]
  (for [nx (range (count (first board)))
        ny (range (count board))]
    [nx ny]))

(defn char-at
  [board x y]
  (nth (nth board y) x))

(defn words-at
  [board x y]
  (let [c (char-at board x y)]
    ))

(defn build-words
  [board x y visited chars trie]
  (let [c (char-at board x y)
        sub-trie (get-in trie [:children c])]
    (when sub-trie
      (if (:terminal? sub-trie)
        (println c)
        ))))

(defn word-here
  [board trie path]
  (if (:terminal? trie)
    (let [chars (map #(char-at board (first %) (last %)) path)]
      (apply str chars))))

(def dictionary
  ["a"])

(def board
  [[\a \b]
   [\c \d]])

(def dic-trie
  (trie dictionary))


(defn words
  [board trie path tile]
  (let [[x y] tile
        c (char-at board x y)
        sub-trie (get-in trie [:children c])
        path (conj path [x y])]
    (when sub-trie
      (let [w (word-here board sub-trie path)
            unvisited-neighbors (...)]
        (conj
         (mapcat (partial words board sub-trie path) unvisited-neighbors)
         w)))))

(defn all-words
  [board trie]
  (compact (map (partial words board trie []) (tiles board))))

(deftest board-test
  (testing "Finding words"
    (let [dict-trie (trie ["i" "bi" "bib"])
          board [[\b \i]]
          ]
      (is (=
           (all-words board dict-trie)
           '("i" "bi"))))))

(board-test)
