(ns ragavardhini.transcribe
  (:use overtone.core)
  (:require [ragavardhini.tanpura :as tanpura]
            [ragavardhini.core-old :as c]
            [overtone.live :as olive]
            [medley.core :as m]
            [ragavardhini.swarams :as sw]
            [ragavardhini.ragams :as r]
            [ragavardhini.scripts.frequencies :as f]
            [ragavardhini.random :as random-play]
            [ragavardhini.layam :as layam]))

(defn roughly-transcribe [file]
  (let [swarams (f/freqs->swarams (f/freqs-from-file file))
        threshold 40]
    (->> (partition-by identity swarams)
         (map (fn [sws] {:swaram (first sws) :count (count sws)}))
         (filter #(> (:count %) threshold)))))

(defn mode [coll]
  (ffirst (sort-by second > (frequencies coll))))

(defn swaram-buckets [swarams bucket-size]
  (let [midis          (map #(sw/swaram->midi :c %) swarams)
        [min-s max-s]  ((juxt first last) (sort midis))
        lower-cutoff-s (- max-s 24)]
    (->> (partition-by identity midis)
         (filter #(> (count %) (/ bucket-size 2))) ;; remove infrequent outliers
         (filter #(> (first %) lower-cutoff-s)) ;; remove low outliers
         (flatten)
         (partition-by identity)
         (map (fn [ms] {:swaram (sw/midi->swaram (first ms) 60)
                        :count (count ms)})))))

(defn bpm-transcribe [file bpm gati offset]
  (let [swaram-dps (drop offset (remove nil? (f/freqs->swarams (f/freqs-from-file file))))
        beats-per-second (/ bpm 60)
        dps-per-beat (/ 300 beats-per-second)
        bucket-size (Math/round (float (/ dps-per-beat gati)))]
    (prn :swaram-dps-count (count swaram-dps)
         :dps-per-beat (float dps-per-beat)
         :bucket-size bucket-size)
    (->> (swaram-buckets swaram-dps bucket-size)
         (mapcat (fn [{:keys [swaram count]}]
                   (repeat (Math/round (float (/ count bucket-size))) swaram))))))

(defn play-bpm-transcribed [file bpm gati offset]
  (let [phrase (bpm-transcribe file bpm gati offset)]
    (c/play-phrase (c/phrase phrase (repeat (count phrase) 1) (/ gati 2)))))

(comment
  (do (play-bpm-transcribed "moh-varsha-2.mp3.wav.pitch.frequencies" 87 2 60)
      (layam/play-avartanams 87 2 (cycle [(:sarvalaghu layam/sequences)])))

  )
