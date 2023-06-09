(ns clj-piet.editor
  (:import javax.swing.JFrame)
  (:import java.awt.Color)
  (:require [clj-piet.interpreter :as interpreter :only [colors]]))

(def colors (map (partial map #(eval `(Color. ~@%)))
                  (partition 6 (keys interpreter/colors))))

(defn -main []
  (let [frame (doto (JFrame. "Piet Editor")
                (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
                (.setSize 800 600)
                (.setVisible true))
        graphics (doto (.getGraphics frame)
                   (.setColor Color/WHITE)
                   (.fillRect 0 0 800 600)
                   (.setColor Color/BLACK))]
    (doseq [x (doall (map (partial * 20) (range 1 (int (/ 800 20)))))]
    (.drawLine graphics x 40 x 580))
    (doseq [y (map (partial * 20) (range 1 (/ 600 20)))]
      (.drawLine graphics 20 y (- 800 20) y))
    (doseq [n (range (count colors))
            :let [line (nth colors n)]]
      (doseq [m (range (count line))
              :let [color (nth line m)]]
        (doto graphics
          (.setColor color)
          (.fillRect (+ (* n 20) 720) (* (+ 3 m) 20) 20 20))))))
