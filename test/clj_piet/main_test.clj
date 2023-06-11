(ns clj-piet.main-test
  (:require [clj-piet.main :as main]
            [clojure.test :refer [deftest testing is]]))

(deftest interpreter-test
  (testing "Testing sample programs for correct output"
    (let [pi (main/piet {:image "programs/pi.png"
                         :codel-size 3
                         :verbose? false})
          hw1 (main/piet {:image "programs/hello-world-1.png"
                         :verbose? false})
          hw2 (main/piet {:image "programs/hello-world-2.png"
                          :verbose? false})]
      (is (= "31405" pi) "Calculating truncated pi failed")
      (is (= "Hello, world!" hw1) "Artistic hello world failed")
      (is (= "Hello, world!" hw2) "Another hello world failed"))))
