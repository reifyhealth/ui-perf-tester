(ns perf.app
  "A test UI that allows us to generate a large number of XHR requests."
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.pprint :refer [pprint]]
            [cljs.core.async :as async :refer [put! chan <! >! timeout close!]]
            [ajax.core :refer [ajax-request json-request-format json-response-format]]))

(enable-console-print!)

;; -- application state --

(defonce default-delay 200)
(defonce default-workers 20)
(defonce default-uri "/sample.json")

(defn initial-state
  "Gets the initial configured state."
  []
  {:delay default-delay
   :workers default-workers
   :uri default-uri
   :started? false
   :calls 0
   :errors 0
   :responses []
   :xhr []})

(defonce app-state
  (atom (initial-state)))

(defn ^:export current-state
  "Gets the current configured state."
  []
  (clj->js @app-state))

(defn- save-ref
  "Save a reference to an object in the app-state under the key, k."
  [k o]
  (swap! app-state update k conj o))

(defn- toggle!
  "Toggles the started/stopped app-state value."
  []
  (swap! app-state update :started? not))

;; -- request helpers --

(defn- cachebuster
  "Appends the current time to the URI."
  [uri]
  (let [sep (if (pos? (.indexOf uri "?")) "&" "?")]
    (str uri sep "t=" (.getTime (js/Date.)))))

(defn ^:export http-get
  "Performs an XHR GET request of the supplied URI."
  [uri]
  (ajax-request
   {:uri (cachebuster (:uri @app-state))
    :method :get
    :format (json-request-format)
    :response-format (json-response-format {:keywords? true})
    :handler
    (fn [[ok response]]
      (swap! app-state (fn [data]
                   (cond-> data
                     true     (update :calls inc)
                     (not ok) (update :errors inc)
                     true     (update :responses conj response)))))}))

;; -- event handlers --

(defn start
  "Event handler for the start button. Kicks off N workers
  to make XHR JSON requests to the URI."
  []
  (toggle!)
  (println "Started")
  (dotimes [_ (:workers @app-state)]
    (go-loop []
      (<! (timeout (:delay @app-state)))
      (when (:started? @app-state)
        (save-ref :xhr (http-get (:uri @app-state)))
        (recur)))))

(defn stop
  "Stops the XHR submission loop. Note that requests sitting in the
  browser's network queue will continue to complete."
  []
  (toggle!)
  (println "Stopped"))

(defn reset
  "Reset the app state to its initial defaults."
  []
  (reset! app-state (initial-state))
  (js/console.clear))

(defn set-value
  "Sets the value in app-state stored under the key, k, to the
  value of the event target. Optionally transforms the value with
  the function, f, if supplied."
  ([k]
   (set-value k identity))

  ([k f]
   (fn  [e]
     (swap! app-state assoc k (f (.. e -target -value))))))

;; -- ui component --

(defn perf-test
  "The Reagent component that renders the UI."
  [data]
  (let [started? (:started? @data)]
    [:div.perf
     [:hgroup
      [:h1 "Performance Test"]
      [:p "Open the browser devtools and watch the network tab for serialized requests."]]

     [:div.row
      [:div.col.s6.range-field
       [:label (str "Ajax Delay: " (:delay @data) "ms")
        [:input {:value (:delay @data)
                 :type
                 :range
                 :min 100
                 :step 100
                 :max 10000
                 :onChange (set-value :delay js/parseInt)}]]]
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
         [:th.blue.darken-1.white-text "Errors"]
         [:th.blue.darken-1.white-text "Responses"]]]
       [:tbody
        [:tr
         [:td (str (:calls @data))]
         [:td (str (:errors @data))]
         [:td (str (count (:responses @data)))]]]]]
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

;; -- entry point--

(defn init []
  (reagent/render-component
   [perf-test app-state]
   (.getElementById js/document "container")))
