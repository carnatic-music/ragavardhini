(ns ragavardhini.scripts.repetitions)

(defn selective-repeat [pattern]
  (if (apply = pattern)
    pattern
    [pattern pattern]))

(defn repeat-at-matra [matra aksharas]
  (->> aksharas
       (partition matra)
       (mapcat selective-repeat)
       flatten))

(defn repeat-avartanas [generated-aksharas {:keys [playback-gati] :as props}]
  (let [num-matras-in-avartana (* playback-gati 8)]
    (->> generated-aksharas
         (repeat-at-matra 2)
         (repeat-at-matra 4)
         (repeat-at-matra 8))))
