(ns ragavardhini.layam
  (:use [overtone.core :as o]))

(def gatis
  {:tisra     3
   :chatusra  4
   :khanda    5
   :misra     7
   :sankeerna 9})

(def jatis gatis)

(defn angas [jati]
  {:U 1
   :O 2
   :I (get jatis jati)})

(def talas
  {:eka     [:I]
   :rupaka  [:O :I]
   :jhampa  [:I :U :O]
   :triputa [:I :O :O]
   :matya   [:I :O :I]
   :ata     [:I :I :O :O]
   :dhruva  [:I :O :I :I]})

(defn tala-accents [jati tala]
  (map
   (fn [anga] (get (angas jati) anga))
   (tala talas)))

(def mridangam-sounds
  {:bheem (o/freesound-sample 224030)
   :cha   (o/freesound-sample 224035)
   :dheem (o/freesound-sample 224161)
   :dhin  (o/freesound-sample 224219)
   :num   (o/freesound-sample 224267)
   :ta    (o/freesound-sample 224348)
   :thi   (o/freesound-sample 224983)
   :tha   (o/freesound-sample 224501)
   :tham  (o/freesound-sample 224695)
   :thom  (o/freesound-sample 225220)})

(def derived-sounds
  {:ki :thi
   :ka :thi
   :ri :tha})

(def sound-types
  {:ring      [:dhin :cha :bheem]
   :crisp     [:thi :ta :num :tha]
   :resonant  [:thom]
   :composite [:tham :dheem]})

(defn combo [number-of-beats]
  (concat
   [(rand-nth (concat (:ring sound-types)
                      (:resonant sound-types)
                      (:composite sound-types)))]
   (take (dec number-of-beats) (shuffle (:crisp sound-types)))))

(def sequences
  {:sarvalaghu [:tham :dheem :tham :dhin
                :num :dheem :tham :dheem]})

(defn play-avartanams*
  ([nome avartanams gati]
   (play-avartanams* (nome) nome avartanams gati))
  ([beat nome [avartanam & more-avartanams] gati]
   (doseq [[i sound] (map-indexed (fn [i s] [i s]) avartanam)
           :let [beatx (+ beat (/ i gati))]]
     (o/at (nome beatx) ((mridangam-sounds sound))))
   (let [next-beat (+ beat (/ (count avartanam) gati))]
     (o/apply-by (nome next-beat) play-avartanams* next-beat nome more-avartanams gati []))))

(defn play-avartanams [tempo gati avartanams]
  (clojure.pprint/pprint (take 10 avartanams))
  (play-avartanams* (o/metronome tempo) avartanams gati))

(comment
  (play-avartanams 60 4
                   (cycle [(:sarvalaghu sequences)]))

  (play-avartanams 60 4
                   (->> #(mapcat combo (tala-accents :chatusra :rupaka))
                        (repeatedly 10))))
