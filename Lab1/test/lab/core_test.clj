(ns lab.core-test
  (:require [clojure.test :refer :all]
            [lab.core :refer :all]))

(deftest parse-object-test
  (testing [parse-object]
    (is (= 15 (count (parse-object (slurp "butterfly.txt") #"\r\n"))))))

(deftest squared-difference-test
  (testing 
    (is (= (squared-difference ["1" "-1" "1"] ["5" "1" "6"]) 45.0)) 
    (is (= (squared-difference ["0" "5" "6"] ["0" "5" "6"]) 0.0))))

(deftest hamming-distance-test
  (testing 
    (is (= (hamming-distance ["1" "-1" "1"] ["5" "1" "6"]) 3)) 
    (is (= (hamming-distance ["0" "5" "6"] ["0" "5" "6"]) 0))))

(deftest potential-value-test
  (testing 
    (is (= 2.8625185805493937E-20 (potential-value ["1" "-1" "1"] ["5" "1" "6"] 1 squared-difference)))))

