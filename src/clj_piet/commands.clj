(ns clj-piet.commands)

(defn piet-push [m]
  (update m :stack (partial cons (:value m))))

(defn piet-update [m f n]
  (piet-push (update (->> m :stack (take n) reverse (apply f) (assoc m :value))
                     :stack nthnext n)))

(defn piet-pop [m]
  (update m :stack next))

(defn piet-add [m]
  (piet-update m + 2))

(defn piet-subtract [m]
  (piet-update m - 2))

(defn piet-multiply [m]
  (piet-update m * 2))

(defn piet-divide [m]
  (piet-update m (comp int /) 2))

(defn piet-mod [m]
  (piet-update m clojure.core/mod 2))

(defn piet-not [m]
  (piet-update m #(if (zero? %) 1 0) 1))

(defn piet-greater [m]
  (piet-update m #(if (< %1 %2) 1 0) 2))

(defn rotate [n coll]
  (if (zero? n)
    coll
    (if (pos? n)
      (->> coll cycle (drop 1) (take (count coll)) (rotate (dec n)))
      (->> coll butlast (cons (last coll)) (rotate (inc n))))))

(defn piet-pointer [m]
  (update (update m :dp (partial rotate (first (:stack m))))
          :stack rest))

(defn piet-switch [m]
  (update (update m :cc (partial rotate (first (:stack m))))
          :stack rest))

(defn piet-duplicate [m]
  (->> m :stack first (assoc m :value) piet-push))

(defn piet-roll [m]
  (let [[k n] (take 2 (:stack m))]
    (update (update m :stack nnext)
            :stack (comp (partial apply concat)
                         (juxt (comp (partial rotate k)
                                     (partial take n))
                               (partial drop n))))))

(defn piet-in-char [m]
  (print "Char input: ")
  (flush)
  (->> (read-line) first int (assoc m :value) piet-push))

(defn piet-in-number [m]
  (print "Number input: ")
  (flush)
  (->> (read-line) read-string eval (assoc m :value) piet-push))

(defn piet-out-char [m]
  (update (update m :out str (char (first (:stack m))))
          :stack rest))

(defn piet-out-number [m]
  (update (update m :out str (first (:stack m)))
          :stack rest))

(def piet-nop identity)
