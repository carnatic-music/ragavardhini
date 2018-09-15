(ns ragavardhini.scripts.repetitions)

(defn repeat-avartanas [generated-aksharas {:keys [playback-gati] :as props}]
  (let [num-matras-in-avartana (* playback-gati 8)]
    (->> generated-aksharas
         (partition 16)
         (mapcat (juxt identity identity))
         flatten)))
