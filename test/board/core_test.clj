(ns board.core-test
  (:require [clojure.test :refer :all]
            [board.core :refer :all]))

(deftest board-test
  (testing "Finding a minimal set of words"
    (let [dict ["i" "bi" "bib"]
          board [[\b \i]]
          words ["bi" "i"]]
      (is (=
           (set words)
           (set (all-words board dict))))))
  (testing "Finding more words"
    (let [dict ["hello" "world" "xmas" "wello" "heth" "xtle"]
          board [[\h \e \x \m]
                 [\w \t \l \a]
                 [\q \l \o \s]]
          words ["hello" "xmas" "wello" "xtle"]]
      (is (= (set words)
             (set (all-words board dict)))))))
