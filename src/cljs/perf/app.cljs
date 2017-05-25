(ns perf.app
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.pprint :refer [pprint]]
            [cljs.core.async :as async :refer [put! chan <! >! timeout close!]]
            [ajax.core :refer [GET ajax-request json-request-format json-response-format]]))

(enable-console-print!)

;; data

(defonce default-frequency 500)
(defonce default-workers 5)

(def log
  (atom {:frequency default-frequency
         :workers default-workers
         :started? false
         :calls 0
         :errors 0
         :responses []
         :uri "/sample.json"}))

;; helpers
(defn- toggle!
  []
  (swap! log update :started? not))

;; handlers

(defn handle-response
  [[ok response]]
  (swap! log (fn [data]
               (cond-> data
                 true     (update :calls inc)
                 (not ok) (update :errors inc)
                 true     (update :responses conj response)))))

(defn start
  []
  (toggle!)
  (println "Started")
  (dotimes [_ (:workers @log)]
    (go-loop []
      (<! (timeout (:frequency @log)))
      (when (:started? @log)
        (ajax-request
         {:uri (:uri @log)
          :method :get
          :handler handle-response
          :format (json-request-format)
          :response-format (json-response-format {:keywords? true})})
        (recur)))))

(defn stop
  []
  (toggle!)
  (println "Stopped")
  (pprint @log))

(defn reset
  []
  (swap! log assoc
         :calls 0
         :errors 0
         :responses []
         :started? false
         :frequency default-frequency)
  (js/console.clear))

(defn set-value
  ([k]
   (set-value k identity))

  ([k f]
   (fn  [e]
     (swap! log assoc k (f (.. e -target -value))))))

;; ui
(defn perf-test
  [data]
  (let [started? (:started? @data)]
    [:div.perf
     [:hgroup
      [:h1 "Performance Test"]
      [:p "Open the browser devtools and watch the network tab for serialized requests."]]

     [:div.row
      [:div.col.s6.range-field
       [:label (str "Ajax Delay: " (:frequency @data) "ms")
        [:input {:value (:frequency @data)
                 :type
                 :range
                 :min 100
                 :step 100
                 :max 10000
                 :onChange (set-value :frequency js/parseInt)}]]]
      [:div.col.s6.range-field
       [:label (str "Workers: " (:workers @data) (when (:started? @data) " (disabled while running)"))
        [:input {:disabled (:started? @data)
                 :value (:workers @data)
                 :class (if (:started? @data) "disabled" "")
                 :type :range
                 :min 1
                 :step 1
                 :max 20
                 :onChange (set-value :workers js/parseInt)}]]]]
     [:div.row
      [:div.col.s12.input-field
       [:input {:disabled (:started? @data)
                :placeholder "JSON Endpoint URI"
                :value (:uri @data)
                :class (if (:started? @data) "disabled" "")
                :type :text
                :onChange (set-value :uri)}]]]
     [:div.row
      [:table.bordered
       [:thead
        [:tr
         [:th.blue.darken-1.white-text "Calls"]
         [:th.blue.darken-1.white-text "Errors"]]]
       [:tbody
        [:tr
         [:td (str (:calls @data))]
         [:td (str (:errors @data))]]]]]
     [:div.row
      [:div.col
       [:button.green.darken-3.waves-effect.waves-light.btn.s1.white-text
        {:onClick start :class (if (not started?) "" "disabled")
         :disabled started?}
        "Start"]]
      [:div.col
       [:button.red.darken-3.waves-effect.waves-light.btn.s1.white-text
        {:onClick stop :class (if started? "" "disabled")
         :disabled (not started?)}
        "Stop"]]
      [:div.col
       [:button.blue.darken-3.waves-effect.waves-light.btn.s1.white-text
        {:onClick reset}
        "Reset"]]]]))

(defn init []
  (reagent/render-component
   [perf-test log]
   (.getElementById js/document "container")))
