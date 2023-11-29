(ns ^:figwheel-hooks ruru.core
  (:require
   [goog.dom :as gdom]
   [clojure.string :as str]
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   [ruru.ruru-lang :as ruru]))

(defonce cells (atom [{:val "" :selection 0 :result nil :expression-list '()}]))

(defonce notebook-environment (atom ruru/default-environment))

(defn tokenize-cell [cells cell-id]
  (let [cell-val (get-in @cells [cell-id :val])
        new-expression-list (ruru/expression-list (str/replace cell-val #"\\ " "‿"))]
    (swap! cells #(assoc-in % [cell-id :expression-list] new-expression-list))))

(defn run-cells! [cells env n]
  (cond (= n (count @cells)) (reset! notebook-environment env)
        :else (let [cell-exp-list (:expression-list (@cells n))
                    [cell-result new-env] (ruru/interpret-exp-list cell-exp-list env)]
                (do (swap! cells assoc-in [n :result] cell-result)
                    (run-cells! cells new-env (inc n))))))

(defn atom-input [value selection cell-id]
  [:textarea {:type "text"
              :rows 1
              :cols 90
              :style ruru/cell-input-style
              :value @value
              :on-change #(do
                            (reset! value
                                    (-> %
                                        .-target
                                        .-value))
                            (reset! selection
                                    (-> %
                                        .-target
                                        .-selectionStart))
                            (tokenize-cell cells cell-id)
                            (run-cells! cells ruru/default-environment 0))
              :on-key-up #(do (reset! selection
                                      (-> %
                                          .-target
                                          .-selectionStart)))}])

(def formatted-input
  [:div {:class "below"
         :opacity "1.0"
         :style {:font-family "monospace" :outline "2px solid grey"}}])

(declare show-result)

(defn cols->row-hiccup [cols row]
  (let [row-vals (map #(nth % row) cols)]
    (into [] (concat [:tr {:style {:border "0px solid black"}}]
                     (mapv #(into []
                                  (concat [:td {:style {:border "0px solid black"}}]
                                          [(show-result %)])) row-vals)))))

(defn show-array [arr]
  (let [dims (arr 'array_dims)
        value (arr 'value)
        cols (partition-all (/ (reduce * dims) (second dims)) value)]
    [:div {:style {:outline "2px solid grey"
                   "max-height" "15em"
                   "overflow-x" "scroll" "overflow-y" "scroll" "white-space" "nowrap"}}
     [:span (str (apply str (interpose "x" dims)) " array\n")]
     (into [] (concat [:table {:style {:border "0px solid black"}}] (map #(cols->row-hiccup cols %) (range (first dims)))))]))

(defn show-map [m & header-names]
  [:div
   [:table
    [:tr
     [:th
      {:style {:text-align "left"}}
      (if header-names (first header-names) "Key")]
     [:th
      {:style {:text-align "left"}}
      (if header-names (second header-names) "Value")]]
    (for [kv (seq m)] [:tr [:td (show-result (first kv))] [:td (show-result (second kv))]])]])

(defn extract-list [v]
  (mapv #(if (ruru/ruru-array? %) (extract-list (% 'value)) %) v))

(defn extract-string [v]
  (mapv #(cond
           (and (seq? %) (= :#_string (first %))) (second %)
           (sequential? %) (extract-string %)
           :else %) v))

(defn show-html [x]
  (cond (ruru/ruru-array? (x 'html))
        (extract-string (first (extract-list [(x 'html)])))
        :else (x 'html)))

(defn show-result [r]
  (cond
    (ruru/ruru-string? r) [:span {:style {:color "red"}} (str "\"" (second r) "\"")]
    (ruru/ruru-array? r) (show-array r)
    (ruru/html? r) (show-html r)
    (map? r) (show-map r)
    (keyword? r) [:span (apply str (rest (str r)))]
    :else [:span (str r)]))

(defn create-cell [val selection cell-id]
  [:div
   [:div {:class "outer"}
    [:div {:class "top" :style {:opacity 0}} [atom-input val selection cell-id]]
    (into [] (concat formatted-input (ruru/get-hiccup (get-in @cells [cell-id :expression-list]) @selection)))]
   [:div {:style {:font-size "1.3em" :font-family "monospace"
                  :padding-top "2px"
                  :outline "2px solid grey"
                  :background-color "lightsalmon"}}
    (show-result (get-in @cells [cell-id :result])) [:br]]])

(defn add-new-cell! []
  (do (swap! cells #(into [] (concat % [{:val "" :selection 0 :result nil :expression-list '()}])))))

(defn show-environment [env]
  (let [ks (clojure.set/difference (set (keys env)) (set (keys ruru/default-environment)))
        defined-vars (into {} (for [k ks] [k (:value (get env k nil))]))]
    [:div {:style {"max-width" "400px" :overflow "scroll" :outline "2px solid grey"}}
     (show-map defined-vars "Name" "Value")]))

(defn notebook-page []
  (fn [] [:sp.main
          [:title "ruru notebook"]
          ; TODO use a grid to format better
          [:div [:img {:src "assets/ruru_icon.png" :style {:width "192px" :height "108px" :margin-left "-45px"}}]
           [:div {:style {:font-size "2em" :margin-top "-75px" :margin-left "110px"}}
            "Welcome to ruru"]]
          [:br]
          [:br]
          [:div "A lightweight programming environment for solving problems of any size."]
          [:br]
          [:div {:style {"max-width" "700px"}}
           (into [] (concat [:div] (mapv #(create-cell (reagent/cursor cells [% :val]) (reagent/cursor cells [% :selection]) %) (range (count @cells))))) 
           [:button {:on-click #(swap! cells (fn [c] (into [] (drop-last c))))} "Delete"]
           [:button {:on-click add-new-cell!} "Create new cell"]
           [:button {:on-click #(reset! notebook-environment ruru/default-environment)} "Reset notebook"]
           [:div {:style {:position "absolute" :top 0 :right 0}} (show-environment @notebook-environment)]]]))


(defn get-app-element []
  (gdom/getElement "app"))

(defn mount [el]
  (rdom/render [notebook-page] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(mount-app-element)

;; specify reload hook with ^:after-load metadata
(defn ^:after-load on-reload []
  (mount-app-element)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
