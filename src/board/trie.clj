(ns board.trie)

(defn update-trie
  [trie word]
  (when-let [[hd & tl] (seq word)]
    (assoc-in
     trie [:children hd]
     (let [sub-trie (get-in trie [:children hd])]
       (if (seq tl)
         (update-trie sub-trie tl)
         (assoc sub-trie :terminal? true))))))


(defn children
  [trie char]
  (get-in trie [:children char]))

(defn build
  [dictionary]
  (reduce update-trie nil dictionary))

(defn term?
  [trie]
  (:terminal? trie))
