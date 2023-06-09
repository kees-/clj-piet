(ns clj-piet.core
  (:require [clj-piet.interpreter :refer [piet-interpreter]])
  (:gen-class))

(defn -main [{:keys [image codel-size]}]
  (piet-interpreter image (read-string codel-size)))
