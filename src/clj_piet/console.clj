(ns clj-piet.console)

(defn location
  [coords new-codel]
  (println (format "%s | %s" coords new-codel)))

(defn command
  [verbose? new-codel codel-map coords next-color cmd machine]
  (when verbose?
    (location coords new-codel)
    (println (format "prev: %s, next: %s" (get-in codel-map coords) next-color))
    (println (format "command: %s" (:name (meta cmd))))
    (println (format "machine: %s\n" machine))))

(defn rotate
  [verbose? new-codel coords tool toggle machine]
  (when verbose?
    (println (format "toggle %s | rotating %s | at %s seeing %s | dp: %s | cc: %s"
                     toggle (name tool) coords new-codel (first (:dp machine)) (first (:cc machine))))))
