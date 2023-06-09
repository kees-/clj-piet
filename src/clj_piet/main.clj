(ns clj-piet.main
  (:require [clj-piet.interpreter :refer [piet-interpreter]])
  (:gen-class))

(defn piet [{:keys [image codel-size verbose?]
             :or {verbose? true codel-size 1}}]
  (piet-interpreter image codel-size verbose?))
