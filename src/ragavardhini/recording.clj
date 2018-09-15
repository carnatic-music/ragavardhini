(ns ragavardhini.recording
  (:require [overtone.core :as o]
            [ragavardhini.scripts.util :as u]))

(defn filename [props]
  (str "resources/generated/" (gensym "gen-")
       (u/props-str props)
        ".wav"))

(defn stop []
  (o/recording-stop))

(defn start [props]
  (prn "recording " (filename props))
  (o/recording-start (filename props)))
