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

(defn n-ads [n coll]
  (loop [c coll
         acc []]
    (if (< (count c) n)
      acc
      (recur (drop 1 c)
             (conj acc (take n c))))))

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
