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

(defn fill-in-probabilities
  [transcribed-swarams generation-gati]
  (let [swarams (get transcribed-swarams generation-gati)]
    (doall
     (->> (partition (+ 1 generation-gati) 1 swarams)
          (pmap (fn [ad] {[(first ad) (last ad)] (butlast (drop 1 ad))}))
          (apply merge-with conj)
          (m/map-vals frequencies)))))

(defn with-alankara-swarams
  ([transcribed-swarams props]
   (with-alankara-swarams
     transcribed-swarams (generate-swarams transcribed-swarams props) props))
  ([transcribed-swarams varna-swarams {:keys [generation-gati] :as props}]
   (let [duads (partition 2 1 varna-swarams)
         probs (fill-in-probabilities transcribed-swarams (* 2 generation-gati))]
     (mapcat
      (fn [[fs ls :as duad]]
        (cons fs (u/ensure-seq (mc/select (get probs duad)))))
      duads))))

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
                   :bpm 80})

  (let [props {:order 12
               :duration-seconds 30
               :playback-gati 2
               :generation-gati 2
               :mode :continuous
               :bpm 80}
        generated-swarams (generate-swarams mohanam-swarams props)
        as-props (assoc props :playback-gati 8)
        as (with-alankara-swarams mohanam-swarams generated-swarams props)]
    (prn "===generated swarams===")
    (transcribe/prescriptive-notation generated-swarams 16)
    (play as as-props)
    (charts/swaram-melograph as (u/props-str as-props))
    (prn "===with alankara swarams===")
    (transcribe/prescriptive-notation as 16)))
