(ns perf.app
  "A test UI that allows us to generate a large number of XHR requests."
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [reagent.core :as reagent :refer [atom]]
            [promesa.core :as p]
            [cljs.core.async :as async :refer [put! chan <! >! timeout close! chan]]
            [ajax.core :refer [ajax-request json-request-format json-response-format]]))

(enable-console-print!)

;; -- application state --

(defonce default-delay 100)
(defonce max-delay 10000)
(defonce default-workers 20)
(defonce min-workers 1)
(defonce default-uri "/sample.json")

(defn initial-state
  "Gets the initial configured state."
  []
  {:delay     default-delay
   :workers   default-workers
   :uri       default-uri
   :started?  false
   :calls     0
   :errors    0
   :responses []
   :promises  []})

(defonce app-state
  (atom (initial-state)))

(defn ^:export current-state
  []
  (clj->js @app-state))

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
  (p/promise
   (fn [resolve reject]
     (ajax-request
      {:uri             (cachebuster (:uri @app-state))
       :method          :get
       :format          (json-request-format)
       :response-format (json-response-format {:keywords? true})
       :handler
       (fn [[ok? response]]
         (swap! app-state update :calls inc)
         (if ok?
           (resolve response)
           (reject response)))}))))

(defn start-consumer!
  "Starts a consumer to pull a promise off the work channel
  and update the app-state with its value."
  [work-chan]
  (println "Starting 1 work channel promise consumer.")
  (go-loop []
    (when (:started? @app-state)
      (when-let [promise (<! work-chan)]
        (p/branch
         promise
         (fn [response]
           (swap! app-state
                  (fn [data]
                    (update data :responses conj response))))
         (fn [error]
           (swap! app-state
                  (fn [data]
                    (-> data
                        (update :errors inc)
                        (update :responses conj error))))))
        (recur)))))

(defn start-workers!
  "Starts n producers that put XHR promises on the work channel."
  [work-chan n]
  (println (str "Starting " n " work channel XHR promise producers."))
  (dotimes [_ n]
    (go-loop []
      (<! (timeout (:delay @app-state)))
      (when (:started? @app-state)
        (let [promise (http-get (:uri @app-state))]
          (swap! app-state update :promises conj promise)
          (>! work-chan promise))
        (recur)))))

;; -- event handlers --

(defn start
  "Event handler for the start button. Kicks off N workers
  to make XHR JSON requests to the URI."
  []
  (let [workers   (:workers @app-state)
        work-chan (chan workers)]
    (toggle!)
    (start-consumer! work-chan)
    (start-workers! work-chan workers)))

(defn stop
  "Stops the XHR submission loop. Note that requests sitting in the
  browser's network queue will continue to complete."
  []
  (toggle!)
  (println "Stopping new requests (queued requests will still complete)."))

(defn reset
  "Reset the app state to its initial defaults."
  []
  (reset! app-state (initial-state))
  (js/console.clear))

(defn report
  "Prints the current state to the console"
  []
  (js/console.dir (current-state)))

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
                 :min default-delay
                 :step 100
                 :max max-delay
                 :onChange (set-value :delay js/parseInt)}]]]
      [:div.col.s6.range-field
       [:label (str "Workers: " (:workers @data) (when (:started? @data) " (disabled while running)"))
        [:input {:disabled (:started? @data)
                 :value (:workers @data)
                 :class (if (:started? @data) "disabled" "")
                 :type :range
                 :min min-workers
                 :step 1
                 :max default-workers
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
        "Reset"]]
      [:div.col
       [:button.indigo.darken-3.waves-effect.waves-light.btn.s1.white-text
        {:onClick report}
        "Report"]]]]))

;; -- entry point--

(defn init []
  (reagent/render-component
   [perf-test app-state]
   (.getElementById js/document "container")))
