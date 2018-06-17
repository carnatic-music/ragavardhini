(ns ragavardhini.api
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.params :as params]
            [cider.nrepl :as cider]
            [clojure.tools.nrepl.server :as nrepl]
            [cheshire.core :as json]
            [bidi.ring :as br]
            [ragavardhini.html :as p]
            [ragavardhini.ragams :as r]
            [ragavardhini.search :as s]))

(defn html-response [html]
  {:status 200
   :body html
   :content-type "text/html"})

(defn json-response [response]
  {:status 200
   :body (json/encode response)
   :content-type "application/json"})

(defn accepts-json? [request]
  (when-let [accepts (get-in request [:headers "accept"])]
    (.contains accepts "application/json")))

(defn index [request]
  {:status 200
   :body (p/html-skeleton nil)})

(defn melakarthas-index [request]
  (if (accepts-json? request)
    (json-response r/melakarthas)
    (html-response
     (-> (p/melakartha-rows)
         p/html-rows
         p/make-html))))

(defn all [request]
  (if (accepts-json? request)
    (json-response r/ragams)
    (html-response
     (p/make-html (p/html-rows)))))

(defn show-ragam [{:keys [params] :as request}]
  (let [ragam-name (keyword (:name params))
        ragam {ragam-name (r/ragams ragam-name)}]
    (if (accepts-json? request)
      (json-response ragam)
      (html-response
       (->> [(p/row false ragam)]
            p/html-rows
            p/make-html)))))

(defn show-kriti [{:keys [params] :as request}]
  (if-let [search-result (s/search-kriti (:name params))]
    (if (accepts-json? request)
      (json-response search-result)
      (html-response (p/show-kriti search-result)))
    (if (accepts-json? request)
      (json-response "Sorry, no such kriti.")
      (html-response (p/html-skeleton "Sorry, no such kriti.")))))

(defn search-ragam [{:keys [params] :as request}]
  (let [query (or (get (:params (params/params-request request)) "q")
                  (:query params))]
    (if-let [search-result (s/search-ragam query)]
      (if (accepts-json? request)
        (json-response search-result)
        (html-response (p/search-ragam-result search-result)))
      (if (accepts-json? request)
        (json-response "Sorry, no such ragam.")
        (html-response (p/html-skeleton "Sorry, no such ragam."))))))

(defn search-kriti [{:keys [params] :as request}]
  (let [query (or (get (:params (params/params-request request)) "q")
                  (:query params))]
    (if-let [search-result (s/search-kriti query)]
      (if (accepts-json? request)
        (json-response search-result)
        (html-response (p/search-kriti-result search-result)))
      (if (accepts-json? request)
        (json-response "Sorry, no such kriti.")
        (html-response (p/html-skeleton "Sorry, no such kriti."))))))

(defn search [{:keys [params] :as request}]
  (let [type (or (get (:params (params/params-request request)) "t")
                 (:type params))]
    (case type
      "ragam" (search-ragam request)
      "kriti" (search-kriti request))))

(def routes ["/" [["" index]
                  ["all" all]
                  ["" (br/->ResourcesMaybe {:prefix "public/"})]
                  ["melakarthas/" {"" melakarthas-index
                                   [:name] show-ragam}]
                  ["ragams/" {"" all
                              [:name] show-ragam}]
                  ["kritis/" {[:name] show-kriti}]
                  ["search" search]
                  ["search/" {[:type "/"] {[:query] search}}]]])

(def handler
  (br/make-handler routes))

(defn start! [port nrepl-port]
  (nrepl/start-server :port nrepl-port :handler cider/cider-nrepl-handler)
  (jetty/run-jetty handler {:port port}))
