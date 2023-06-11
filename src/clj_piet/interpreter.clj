(ns clj-piet.interpreter
  (:import javax.imageio.ImageIO)
  (:import java.io.File)
  (:import java.awt.Color)
  (:require [clj-piet.commands :refer :all]
            [clj-piet.console :as console]
            [clojure.set :as set]))

(def pointer-cycle '(:right :down :left :up))
(def chooser-cycle '(:left :right))
(def lightness-cycle '(:light :normal :dark))
(def hue-cycle '(:red :yellow :green :cyan :blue :magenta))

(def seed {:dp pointer-cycle :cc chooser-cycle :value nil :stack [] :out ""})

(def colors
  {[0xFF 0xC0 0xC0] [:light :red]
   [0xFF 0xFF 0xC0] [:light :yellow]
   [0xC0 0xFF 0xC0] [:light :green]
   [0xC0 0xFF 0xFF] [:light :cyan]
   [0xC0 0xC0 0xFF] [:light :blue]
   [0xFF 0xC0 0xFF] [:light :magenta]
   [0xFF 0x00 0x00] [:normal :red]
   [0xFF 0xFF 0x00] [:normal :yellow]
   [0x00 0xFF 0x00] [:normal :green]
   [0x00 0xFF 0xFF] [:normal :cyan]
   [0x00 0x00 0xFF] [:normal :blue]
   [0xFF 0x00 0xFF] [:normal :magenta]
   [0xC0 0x00 0x00] [:dark :red]
   [0xC0 0xC0 0x00] [:dark :yellow]
   [0x00 0xC0 0x00] [:dark :green]
   [0x00 0xC0 0xC0] [:dark :cyan]
   [0x00 0x00 0xC0] [:dark :blue]
   [0xC0 0x00 0xC0] [:dark :magenta]
   [0xFF 0xFF 0xFF] :white})

(defn grab-pixels
  "Columns of colors from a path to a source image.
   Represented by `[:tone :hue]`, `nil` for black, `:white` for white."
  [image codel-size]
  (let [img (ImageIO/read (File. image))]
    (vec (for [x (range (/ (.getWidth img) codel-size))]
           (vec (for [y (range (/ (.getHeight img) codel-size))
                      :let [c (Color. (.getRGB img (* x codel-size) (* y codel-size)))
                            color (get colors [(.getRed c) (.getGreen c) (.getBlue c)])]
                      :when (not= c [0x00 0x00 0x00])]
                  color))))))

(defn- neighbors [l]
  (set (mapcat (fn [[x y]]
                 [[(inc x) y]
                  [(dec x) y]
                  [x (inc y)]
                  [x (dec y)]])
               l)))

(defn find-color-block-in [codel-map [x y]]
  (if-let [codel-color (get-in codel-map [x y])]
    (let [neighborly? (comp (partial = codel-color)
                            (partial get-in codel-map))]
      (if (= codel-color :white)
        #{[x y]}
        (loop [block #{[x y]}
               queue #{[x y]}]
          (if (empty? queue)
            block
            (let [next-spread (->> (neighbors queue) (filter neighborly?) set)]
              (recur (set/union block queue)
                     (set/difference next-spread block)))))))
    #{}))

(defn pick
  "Assess upcoming adjacent codel based on orientation and direction."
  [codel-block dp cc]
  (let [[f g h i j k]
        (case dp
          :right [first  > second (case cc :left < :right >) inc identity]
          :down  [second > first  (case cc :left > :right <) identity inc]
          :left  [first  < second (case cc :left > :right <) dec identity]
          :up    [second < first  (case cc :left < :right >) identity dec])
        dir (sort-by f g codel-block)
        [x y] (first (sort-by h i (take-while (comp #(= (f (first dir)) %) f) dir)))]
    [(j x) (k y)]))

(defn call [prev-color next-color]
  (let [commands [[#'piet-nop       #'piet-push       #'piet-pop]
                  [#'piet-add       #'piet-subtract   #'piet-multiply]
                  [#'piet-divide    #'piet-mod        #'piet-not]
                  [#'piet-greater   #'piet-pointer    #'piet-switch]
                  [#'piet-duplicate #'piet-roll       #'piet-in-number]
                  [#'piet-in-char   #'piet-out-number #'piet-out-char]]
        f (fn [l g]
            (if (or (= :white prev-color)
                    (= :white next-color))
              0
              (->> (concat (drop-while #(not= (g prev-color) %) l)
                           (take-while #(not= (g prev-color) %) l))
                   (take-while #(not= (g next-color) %))
                   count)))
        hue-change (f hue-cycle second)
        lightness-change (f lightness-cycle first)]
    (get-in commands [hue-change lightness-change])))

(defn piet-interpreter
  "Run a piet program image, possibly interactively."
  [image codel-size verbose?]
  (let [codel-map (grab-pixels image codel-size)
        piet (loop [state seed
                    [x y] [0 0]
                    toggle 1]
               (if (<= toggle 8)
                 (let [codel-block (find-color-block-in codel-map [x y])
                       new-codel (pick codel-block (first (:dp state)) (first (:cc state)))]
                   (if-let [next-color (get-in codel-map new-codel)]
                     (let [command (call (get-in codel-map [x y]) next-color)
                           new-state (command (assoc state :value (count codel-block)))]
                       (console/command verbose? new-codel codel-map [x y] next-color command new-state)
                       (recur new-state new-codel 1))
                     (let [tool (if (zero? (mod toggle 2)) :dp :cc)
                           new-state (update state tool (partial rotate 1))]
                       (console/rotate verbose? new-codel [x y] tool toggle new-state)
                       (recur new-state [x y] (inc toggle)))))
                 (:out state)))]
    (println (format "\nOutput: %s" piet))
    piet))
