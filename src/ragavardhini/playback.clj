(ns ragavardhini.playback
  (:use overtone.core)
  (:require [ragavardhini.tanpura :as tanpura]
            [ragavardhini.core-old :as c]
            [overtone.live :as olive]
            [medley.core :as m]
            [overtone.inst.synth]
            [ragavardhini.swarams :as sw]
            [ragavardhini.ragams :as r]
            [ragavardhini.scripts.frequencies :as f]
            [ragavardhini.random :as random-play]
            [ragavardhini.layam :as layam]))

(defn play-bpm-transcribed [phrase gati]
  (c/play-phrase (c/phrase phrase
                           (repeat (count phrase) 1)
                           (/ gati 2))))

(definst ignore-this [x 1]
  (def *mx *)
  (sin-osc))

(defmacro play-in-carnatic-style [pitch-env ampl-env dur]
  `((overtone.sc.synth/synth
     "audition-synth"
     (out 0 (hold
             (*mx (var-saw (env-gen ~pitch-env))
                  (env-gen ~ampl-env))
             ~dur
             :done FREE)))))

(defn amp-env [phrase-duration]
  (let [dp5 (* 0.05 phrase-duration)
        d9 (* 0.99 phrase-duration)]
    (envelope [0 1 1 0] [dp5 d9 dp5])))

(defn generic-play [swarams sample-nth]
  (let [total-num        (count swarams)
        playable-swarams (take-nth sample-nth swarams)
        playable-num     (count playable-swarams)
        ;; total-duration   (/ total-num 157)
        total-duration   (/ total-num 10)
        gathi            (* sample-nth (/ total-duration total-num))
        freqs            (->> playable-swarams
                              (mapv (partial sw/swaram->midi :.b))
                              (mapv midi->hz))
        durations        (vec (repeat playable-num gathi))
        pitch-env        (envelope freqs durations)]
    #_(prn :playable-swarams playable-swarams)
    #_(prn :total-duration (float total-duration))
    #_(recording-start (str "resources/kosha/" (gensym "generic-play-") ".wav"))
    (play-in-carnatic-style pitch-env
                            (amp-env total-duration)
                            total-duration)))

(defn continuous [swarams gati]
  (let [total-num        (count swarams)
        playable-swarams swarams
        playable-num     (count playable-swarams)
        total-duration   (/ total-num gati)
        freqs            (->> playable-swarams
                              (mapv (partial sw/swaram->midi :.b))
                              (mapv midi->hz))
        durations        (vec (repeat playable-num (/ 1 gati)))
        pitch-env        (envelope freqs durations
                                   (->> (repeat (dec (/ gati 4)) :welch)
                                        (cons :step)))]
    (play-in-carnatic-style pitch-env
                            (amp-env total-duration)
                            total-duration)))

(defn bpm-play-discreet [swarams gati _]
  (let [phrase (->> (partition-by identity swarams)
                    (map (fn [sws] {:swaram (first sws) :count (count sws)})))
        continuous-swarams (map :swaram phrase)
        continuous-durations (map :count phrase)]
    (c/play-phrase (c/phrase continuous-swarams
                             continuous-durations
                             (/ gati 2)))))

(defn duration [swarams gati bpm]
  (/ (* (count swarams) (/ 60 bpm)) gati))

(defn make-phrase [swarams gati bpm]
  (let [notes
        (->> (partition-by identity swarams)
             (map (fn [sws]
                    {:freq     (midi->hz (sw/swaram->midi :c (first sws)))
                     :duration (duration sws gati bpm)})))]
    {:freqs     (map :freq notes)
     :durations (map :duration notes)}))

(defn swaram-amp-env [swaram-duration]
  (let [dp5 (* 0.01 swaram-duration)
        d9  (* 0.99 swaram-duration)]
    {:levels    [0 1 1 0]
     :durations [0 swaram-duration 0]}))

(defn phrase-amp-env
  ([sw-amp-env]
   (phrase-amp-env {} swaram-amp-env))
  ([acc sw-amp-env]
   (-> acc
       (update :levels concat (:levels sw-amp-env))
       (update :durations concat (:durations sw-amp-env)))))

(defn play-swaram [freq duration]
  (let [amp (swaram-amp-env duration)]
    (play-in-carnatic-style (envelope [freq]
                                      [duration]
                                      :step)
                            (envelope (:levels amp)
                                      (:durations amp)
                                      :step)
                            duration))
  (Thread/sleep duration))

(defn play-phrase [phrase]
  (let [pitch-env       (envelope (:freqs phrase)
                                    (:durations phrase)
                                    (repeat (dec (count phrase)) :welch))
          phrase-duration (apply + (:durations phrase))
          amp-env         (reduce phrase-amp-env
                                  (map swaram-amp-env
                                       (:durations phrase)))]
    (play-in-carnatic-style pitch-env
                            (envelope (:levels amp-env)
                                      (:durations amp-env)
                                      :step)
                            phrase-duration)
    (Thread/sleep (* 800 phrase-duration)))
  #_(let [{:keys [freqs durations]} phrase]
    (doseq [[f d] (map (fn [f d] [f d]) freqs durations)]
                           (play-swaram f d)
                           (Thread/sleep (* 1000 d)))))

(defn bpm-play-continuous [swarams gati bpm]
  (let [phrases        (->> (partition-all gati swarams)
                            (map #(make-phrase % gati bpm)))]
    (doseq [phrase phrases]
      (play-phrase phrase))))

(defn sarvalaghu [bpm gati]
  (layam/play-avartanams bpm gati
   (cycle [(:sarvalaghu layam/sequences)])))

(defn foo [file]
  (let [sample-nth 2
        swarams (f/freqs->swarams (f/freqs-from-file file))]
    (generic-play (vec swarams) sample-nth)))
