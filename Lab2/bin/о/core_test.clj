(ns о.core-test
  (:require [clojure.test :refer :all]
            [о.core :refer :all]))

(deftest get-urls-test
  (with-redefs [get-urls]    
    (is (= 6 (count (get-urls "https://clojuredocs.org/clojure.test/deftest"))))
    (is (= 7 (count (get-urls "https://www.google.by"))))
    (is (= 7 (count (get-urls "https://clojuredocs.org/clojure.core/set"))))))

(deftest check-status-test
  (with-redefs [check-status]
    (is (= false (check-status nil "" (ref {}) nil nil))))
    (is (= false (check-status {:status 404} "" (ref {}) nil nil)))
    (is (= true (check-status {:status 200} nil nil nil nil))))
