(ns clj-piet.main-test
  (:require [clj-piet.main :as main]
            [clojure.test :refer [deftest testing is]]))

(deftest interpreter-test
  (testing "Testing sample programs for correct output"
    (let [pi (main/piet {:image "programs/pi.png"
                         :codel-size 3
                         :verbose? false})]
      (is (= "31405" pi) "Calculating truncated pi failed"))))
