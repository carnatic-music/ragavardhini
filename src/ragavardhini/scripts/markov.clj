(ns ragavardhini.scripts.markov
  (:require [ragavardhini.scripts.frequencies :as f]
            [ragavardhini.scripts.samples :as samples]
            [markov-chains.core :as mc]
            [medley.core :as m]
            [ragavardhini.scripts.dsp-adjustments :as adj]
            [ragavardhini.scripts.demo :as demo]
            [ragavardhini.transcribe :as transcribe]
            [ragavardhini.playback :as playback]))

(defn rs []
  (overtone.core/recording-stop))

(defn rstart [mode raga-name nth-order gathi]
  (overtone.core/recording-start (str "resources/generated/" (gensym "gen-")
                                      "-mode-" (name mode)
                                      "-raga-" (name raga-name)
                                      "-nth-order-" nth-order
                                      "-gathi-" gathi
                                      ".wav")))

(defn non-prominent-swarams [swarams npr-factor]
  (let [hist (->> (frequencies swarams)
                  f/->perc-histogram
                  (sort-by second >))]
    (->> (drop (* npr-factor (count hist)) hist)
         (into {})
         keys
         set)))

(defn make-adjustments [swarams form tonic-prominence npr-factor]
  (let [nprs (non-prominent-swarams swarams npr-factor)]
    (prn nprs)
    (clojure.walk/postwalk
     (fn [x]
       (if (map? x)
         (as-> x m
           (update m :s #(Math/round (* tonic-prominence (or % 0))))
           (apply dissoc m nprs))
         x))
     form)))

(defn prominent-notes [hist perc]
  (->> hist
       (sort-by second >)
       (take (* 1/100 perc (count hist)))
       keys
       set))

(defn swarams [file]
  (->> file
       f/freqs-from-file
       f/freqs->swarams
       (remove nil?)))

(defn bpm-swarams [{:keys [file bpm offset gati]
                    :or {gati 4}}]
  (transcribe/bpm-transcribe file bpm gati offset))

(defn swarams-for-raga [raga-files npr-factor]
  (let [raw-swarams (mapcat swarams raga-files)
        hist (frequencies raw-swarams)
        prominent-notes (prominent-notes hist npr-factor)]
    (filter prominent-notes raw-swarams)))

(defonce collations (atom {}))

(defn get-or-add-collation [raga-name swarams nth-order]
  (or (get-in @collations [raga-name nth-order])
      (do (swap! collations assoc-in [raga-name nth-order] (mc/collate swarams nth-order))
          (get-or-add-collation raga-name swarams nth-order))))

(defn play-collated [raga-name swarams nth-order duration-seconds gathi]
  (try
    (rs)
    (let [collated-swarams (get-or-add-collation raga-name swarams nth-order)]
      (rstart :continuous raga-name nth-order gathi)
      (playback/generic-play
       (take (* gathi duration-seconds 10)
             (mc/generate (repeat nth-order :.s) collated-swarams))
       gathi))
    (catch Exception e
      (rs))))

(defn generate-bpm-collated-swarams [swarams nth-order duration-seconds gathi]
  (let [npr-swarams      (non-prominent-swarams swarams 0.4)
        playable-swarams (remove npr-swarams swarams)
        collated-swarams (make-adjustments playable-swarams
                                           (mc/collate playable-swarams nth-order)
                                           0.6
                                           1)]
    (take (* gathi duration-seconds)
          (mc/generate (repeat nth-order :.s) collated-swarams))))

(defn play-bpm-collated
  ([raga-name swarams nth-order duration-seconds gathi mode]
   (play-bpm-collated raga-name
                      (generate-bpm-collated-swarams
                       swarams nth-order duration-seconds gathi)
                      nth-order gathi mode))
  ([raga-name generated-swarams nth-order gathi mode]
   (try
     (rs)
     (rstart mode raga-name nth-order gathi)
     (case mode
       :continuous (playback/continuous generated-swarams gathi)
       :discreet   (playback/bpm-play-discreet generated-swarams gathi 84))
     (catch Exception e
       (prn e)
       (rs)))))

(defn transcribe [files gati]
  (doall (flatten (pmap #(bpm-swarams (assoc % :gati gati)) files))))

(defn sarvalaghu [bpm gati]
  (ragavardhini.layam/play-avartanams
   bpm
   gati
   (cycle [(:sarvalaghu ragavardhini.layam/sequences)])))

(defn n-ads [n coll]
  (loop [c coll
         acc []]
    (if (< (count c) n)
      acc
      (recur (drop 1 c)
             (conj acc (take n c))))))

(defn fill-in-probabilities [swarams level]
  (->> (n-ads 9 swarams)
       (pmap (fn [ad] {[(first ad) (last ad)] (butlast (drop 1 ad))}))
       (apply merge-with conj)
       (m/map-vals frequencies)))

(defonce fips (memoize fill-in-probabilities))

(defn with-alankara-swarams
  [varna-swarams
   {:keys [level order gati] :as varna-params}]
  (let [duads (n-ads 2 varna-swarams)
        probs (fips (get mohanam-swarams-with-gati 8) level)]
    (mapcat (fn [[fs ls :as duad]] (cons fs (mc/select (get probs duad))))
            duads)))

(defn graph-and-play [generated-swarams level order gati mode]
  (let []
    (ragavardhini.scripts.charts/swaram-melograph
     generated-swarams
     (format "level:%s|order:%s|gati: %s"
             level order gati))
    (play-bpm-collated :mohanam
                       generated-swarams
                       order
                       gati
                       mode)))

(comment
  (def mohanam-swarams-with-gati
    {1 (transcribe samples/mohanam-files 1)
     2 (transcribe samples/mohanam-files 2)
     4 (transcribe samples/mohanam-files 4)
     8 (transcribe samples/mohanam-files 8)})

  (def mohanam-swarams (doall (mapcat swarams samples/mohanam-files)))
  (def kalyani-swarams (doall (swarams-for-raga samples/kalyani-files 0.38)))
  (def collated-kalyani-swarams
    (doall
     (pmap (fn [nth-order]
             {nth-order (mc/collate kalyani-swarams nth-order)})
           (range 2 21))))

  (ragavardhini.tanpura/play 60 0.2)
  (play-collated :kalyani kalyani-swarams 2 20 30)
  (play-collated :kalyani kalyani-swarams 10 30 100)

  (play-bpm-collated :mohanam (get mohanam-swarams-with-gati 8) 10 100 12) ;; continuous
  (play-bpm-collated :mohanam (get mohanam-swarams-with-gati 4) 5 100 2 :discreet) ;; discreet
  (ragavardhini.scripts.charts/two-swaram-histogram
   (->> (mc/collate (get mohanam-swarams-with-gati 4) 1)
        (map (fn [[s probs]] (m/map-keys (fn [k] (conj s k)) probs)))
        (into {})
        f/->perc-histogram)
   "mohanam with gati 4"))
