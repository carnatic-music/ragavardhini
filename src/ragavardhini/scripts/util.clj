(ns ragavardhini.scripts.util
  (:require [clojure.java.io :as io]
            [net.cgrand.enlive-html :as html]
            [medley.core :as m]))

(defn str->html-resource [string]
  (let [filename (str (gensym "html-resource") ".html")]
    (spit filename string)
    (-> filename
        io/file
        html/html-resource)))

(defn ->perc-histogram [hist]
  (let [total (reduce + (vals hist))]
    (m/map-vals #(* 100.0 (/ % total))
                hist)))

(defn prob-matrix->perc-hist [prob-matrix]
  (->> prob-matrix
       (map (fn [[s probs]] (m/map-keys (fn [k] (conj s k)) probs)))
       (into {})
       ->perc-histogram))

(defn props-str [props]
  (apply str
         (for [[k v] props]
           (str "-" k "-" v))))

(defn ensure-seq [x]
  (if (seq? x)
    x
    [x]))
