(ns ragavardhini.transcribe
  (:use overtone.core)
  (:require [clojure.string :as string]
            [ragavardhini.tanpura :as tanpura]
            [ragavardhini.core-old :as c]
            [overtone.live :as olive]
            [medley.core :as m]
            [ragavardhini.swarams :as sw]
            [ragavardhini.ragams :as r]
            [ragavardhini.playback :as playback]
            [ragavardhini.scripts.samples :as samples]
            [ragavardhini.scripts.frequencies :as f]
            [ragavardhini.random :as random-play]
            [ragavardhini.layam :as layam]))

(defn convert-to-swarams [file]
  (->> file
       f/freqs-from-file
       f/freqs->swarams
       (remove nil?)))

(defn roughly-transcribe
  "[DEPRECATED] Given a file, transcribe the swarams independent of
  the bpm. The threshold removes infrequently ocurring swarams that
  might be the errors in autocorrelation."
  [file threshold]
  (let [threshold (or threshold 40)]
    (->> (convert-to-swarams file)
         (partition-by identity)
         (map (fn [sws] {:swaram (first sws) :count (count sws)}))
         (filter #(> (:count %) threshold)))))

(defn plausible-swaram? [swaram]
  (let [shruti-midi (sw/shruthis :c)
        swaram-midi (sw/swaram->midi :c swaram)]
    (< (- shruti-midi 14)
       swaram-midi
       (+ shruti-midi 14))))

(defn remove-outliers [swarams akshara-length]
  (->> (partition-by identity swarams)
       (filter #(> (count %) (/ akshara-length 2))) ;; remove infrequent outliers
       (filter #(plausible-swaram? (first %))) ;; remove unnatural swarams
       (flatten)))

(defn duration [swarams akshara-length]
  (->> (/ (count swarams) akshara-length)
       float
       Math/round))

(defn swaram-buckets
  "Given an akshara length, compute the swarams and their durations."
  [swarams akshara-length]
  (->> (remove-outliers swarams akshara-length)
       (partition-by identity)
       (map (fn [ms] {:swaram   (first ms)
                      :duration (duration ms akshara-length)}))))

(def akshara-length
  "Roughly equals the number of data points for a single akshara"
  (memoize
   (fn [bpm gati]
     (let [matras-per-second (/ bpm 60)
           dps-per-matra     (/ 200 matras-per-second)
           dps-per-akshara   (/ dps-per-matra gati)]
       (Math/round (float dps-per-akshara))))))

(defn bpm-transcribe
  [{:keys [bpm gati offset file]
    :or   {bpm 80 gati 4 offset 0}
    :as input-file}]
  (when (nil? file) (throw (ex-info "No file given" input-file)))
  (let [swaram-dps (drop offset (convert-to-swarams file))]
    (->> (swaram-buckets swaram-dps (akshara-length bpm gati))
         (mapcat (fn [{:keys [swaram duration]}] (repeat duration swaram))))))

(def prescriptive-swarams
  {:.g1 :.r2
   :g1 :r2
   :.n1 :.d2
   :n1 :d2})

(defn prescriptive-notation [swarams num-in-line]
  (->> swarams
       (map #(get prescriptive-swarams % %))
       sw/actual-swarams->simple-swarams
       (partition-by identity)
       (mapcat (fn [sws] (cons (name (first sws)) (repeat (dec (count sws)) ","))))
       (clojure.string/join " ")
       (partition (* 2 num-in-line))
       (map #(apply str %))
       (clojure.pprint/pprint)))

(comment
  (do
    (let [ts (bpm-transcribe (first samples/research-files))]
      (prescriptive-notation ts 16)
      (playback/play-bpm-transcribed ts 4)
      #_(playback/continuous (take 400 ts) 4))
    (layam/play-avartanams 87 2 (cycle [(:sarvalaghu layam/sequences)])))

  )
