(ns ragavardhini.scripts.markov
  (:require [ragavardhini.scripts.frequencies :as f]
            [ragavardhini.scripts.samples :as samples]
            [markov-chains.core :as mc]
            [medley.core :as m]
            [ragavardhini.scripts.dsp-adjustments :as adj]
            [ragavardhini.scripts.demo :as demo]
            [ragavardhini.transcribe :as transcribe]
            [ragavardhini.playback :as playback]
            [ragavardhini.scripts.charts :as charts]
            [ragavardhini.recording :as record]
            [ragavardhini.scripts.util :as u]))

(defn non-prominent-swarams [swarams npr-factor]
  (let [hist (->> (frequencies swarams)
                  u/->perc-histogram
                  (sort-by second >))]
    (->> (drop (* npr-factor (count hist)) hist)
         (into {})
         keys
         set)))

(defn remove-non-prominent-swarams [swarams npr-factor]
  (let [npr-swarams (non-prominent-swarams swarams npr-factor)]
    (remove npr-swarams swarams)))

(defn reduce-tonic-prominence [form tonic-prominence]
  (clojure.walk/postwalk
   (fn [x]
     (if (map? x)
       (update x :s #(Math/round (* tonic-prominence (or % 0))))
       x))
   form))

(defn generate-swarams
  [transcribed-swarams {:keys [order duration-seconds generation-gati playback-gati]}]
  (let [swarams          (get transcribed-swarams generation-gati)
        playable-swarams (remove-non-prominent-swarams swarams 0.4)
        collated-swarams (reduce-tonic-prominence
                          (mc/collate playable-swarams order)
                          0.6)]
    (take (* playback-gati duration-seconds)
          (mc/generate (repeat order :.s) collated-swarams))))

(def fill-in-probabilities
  (memoize
   (fn [swarams level]
     (->> (u/n-ads 9 swarams)
          (pmap (fn [ad] {[(first ad) (last ad)] (butlast (drop 1 ad))}))
          (apply merge-with conj)
          (m/map-vals frequencies)))))

(defn with-alankara-swarams
  [varna-swarams
   {:keys [level order generation-gati] :as varna-params}]
  (let [duads (u/n-ads 2 varna-swarams)
        probs (fill-in-probabilities (get mohanam-swarams-with-gati 8) level)]
    (mapcat (fn [[fs ls :as duad]] (cons fs (mc/select (get probs duad))))
            duads)))

(defn play
  [generated-swarams {:keys [order duration-seconds playback-gati mode bpm] :as props}]
  (try
    (record/stop)
    (record/start props)
    (case mode
      :continuous (playback/continuous generated-swarams playback-gati)
      :discreet   (playback/bpm-play-discreet generated-swarams playback-gati bpm))
    (catch Exception e
      (prn e)
      (record/stop))))

(defn generate-and-play [transcribed-swarams props]
  (let [generated-swarams (generate-swarams transcribed-swarams props)]
    (play generated-swarams props)))

(defn graph-and-play [transcribed-swarams props]
  (let [generated-swarams (generate-swarams transcribed-swarams props)]
    (charts/swaram-melograph generated-swarams (u/props-str props))
    (play generated-swarams props)))

(comment
  (def mohanam-swarams
    (doall
     (transcribe/bpm-transcribe-files samples/mohanam-files)))

  (generate-and-play mohanam-swarams
                     {:order 12
                      :duration-seconds 100
                      :playback-gati 2
                      :generation-gati 2
                      :mode :discreet
                      :bpm 80})

  (graph-and-play mohanam-swarams
                  {:order 24
                   :duration-seconds 100
                   :playback-gati 8
                   :generation-gati 4
                   :mode :continuous
                   :bpm 80}))
