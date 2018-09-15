(ns ragavardhini.scripts.demo
  (:require [leipzig.live :as l]
            [markov-chains.core :as mc]
            [medley.core :as m]
            [ragavardhini.playback :as playback]
            [ragavardhini.random :as r]
            [ragavardhini.recording :as record]
            [ragavardhini.scripts.charts :as charts]
            [ragavardhini.scripts.demo :as demo]
            [ragavardhini.scripts.dsp-adjustments :as adj]
            [ragavardhini.scripts.frequencies :as f]
            [ragavardhini.scripts.markov :as mar]
            [ragavardhini.scripts.samples :as samples]
            [ragavardhini.scripts.util :as u]
            [ragavardhini.tanpura :as tanpura]
            [ragavardhini.transcribe :as transcribe]
            [overtone.core :as o]))

(defn stop []
  (l/stop)
  (o/stop)
  (o/recording-stop))

(defn mohanam []
  (time
   (def mohanam-probs
     (f/two-swaram-probabilities samples/mohanam-files
                                 :tonic-prominence 1 :npr-factor 1))))

(comment
  (mohanam)
  (r/play-completely-random-phrase :mohana 4 100)
  (r/play-single-swaram-prob-phrase (:one mohanam-probs) 4 20)
  (r/play-with-two-swaram-weights (:two mohanam-probs) :.s 1/20 300))

(defn revati []
  (time
   (def revati-probs
     (f/three-swaram-probabilities samples/revati-files
                                   :tonic-prominence 1 :npr-factor 0.5))))

(comment
  (revati)
  (tanpura/play 60 0.2)
  (r/play-completely-random-phrase :revati 4 100)
  (r/play-single-swaram-prob-phrase (:one revati-probs) 4 100)
  (r/play-with-two-swaram-weights (:two revati-probs) :.s 1/20 300)
  (r/play-with-three-swaram-weights (:three revati-probs) :.s 1/20 300))

(defn kalyani []
  (time
   (def kalyani-probs
     (f/three-swaram-probabilities samples/kalyani-files
                                   :tonic-prominence 1 :npr-factor 0.38))))

(comment
  (kalyani)
  (charts/swaram-histogram (:one kalyani-probs) "kalyani")
  (r/play-completely-random-phrase :kalyani 4 100)
  (r/play-single-swaram-prob-phrase (:one kalyani-probs) 4 100)
  (r/play-with-two-swaram-weights (:two kalyani-probs) :.s 1/20 300)
  (r/play-with-three-swaram-weights (:three kalyani-probs) :.s 1/20 300))

(defn pancharatna-kritis []
  (time
   (def jagadanandakaraka-probs
     (f/two-swaram-probabilities [(nth samples/pancharatna-kritis 0)]
                                 :tonic-prominence 0.4 :npr-factor 0.25)))

  (time
   (def dudukugala-probs
     (f/two-swaram-probabilities [(nth samples/pancharatna-kritis 1)]
                                 :tonic-prominence 0.4 :npr-factor 0.25)))

  (time
   (def sadhinchane-probs
     (f/two-swaram-probabilities [(nth samples/pancharatna-kritis 2)]
                                 :tonic-prominence 0.4 :npr-factor 0.25)))

  (time
   (def kanakanaruchira-probs
     (f/two-swaram-probabilities [(nth samples/pancharatna-kritis 3)]
                                 :tonic-prominence 0.4 :npr-factor 0.25)))

  (time
   (def endaro-probs
     (f/two-swaram-probabilities [(nth samples/pancharatna-kritis 4)]
                                 :tonic-prominence 0.4 :npr-factor 0.25))))

;;=================================
;; Markov demonstrations
(comment

  ;; transcribe mohanam swarams
  (def mohanam-swarams
    (doall
     (transcribe/bpm-transcribe-files samples/mohanam-files)))

  ;; discrete at gati 2
  (mar/generate-and-play mohanam-swarams
                         {:order            12
                          :duration-seconds 100
                          :playback-gati    2
                          :generation-gati  2
                          :mode             :discreet
                          :bpm              80})

  ;; continuous at gati 4
  (mar/graph-and-play mohanam-swarams
                      {:order            24
                       :duration-seconds 100
                       :playback-gati    8
                       :generation-gati  4
                       :mode             :continuous
                       :bpm              80})

  ;; with alankaras filled in
  (let [props             {:order            12
                           :duration-seconds 30
                           :playback-gati    2
                           :generation-gati  2
                           :mode             :continuous
                           :bpm              80}
        generated-swarams (mar/generate-swarams mohanam-swarams props)
        as-props          (assoc props :playback-gati 8)
        as                (mar/with-alankara-swarams mohanam-swarams generated-swarams props)]
    (prn "===generated swarams===")
    (transcribe/prescriptive-notation generated-swarams 16)
    (mar/play as as-props)
    (charts/swaram-melograph as (u/props-str as-props))
    (prn "===with alankara swarams===")
    (transcribe/prescriptive-notation as 16))

  ;; discrete, with repetitions
  (let [props {:order            12
               :duration-seconds 50
               :playback-gati    2
               :generation-gati  2
               :mode             :discreet
               :bpm              80}
        generated-swarams (mar/generate-swarams mohanam-swarams props)
        rs (ragavardhini.scripts.repetitions/repeat-avartanas generated-swarams props)]
    (transcribe/prescriptive-notation rs 16)
    (mar/play generated-swarams props)
    (charts/swaram-melograph generated-swarams (u/props-str props))))
