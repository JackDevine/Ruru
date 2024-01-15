(ns ruru.shared-input
  (:require
   [clojure.string :as str]
   [reagent.core :as reagent :refer [atom]]
   [ruru.style :as style]
   [ruru.ruru-lang :as ruru]
   [ruru.format :as format]
   [ruru.parser :as parser]))

; TODO only run cells below the current cell
(defn run-cells! [cells cell-order notebook-environment env]
  (cond (empty? cell-order) (reset! notebook-environment env)
        :else (let [first-cell (first cell-order)
                    cell-exp-list (:expression-list (@cells first-cell))
                    cell-value (:val (@cells first-cell))
                    [[cell-result new-env] time] (let [start (. js/performance now)
                                                       res (try (ruru/interpret-exp-list cell-exp-list env)
                                                                (catch js/Error e [(list 'error (str "Unable to evaluate: " cell-value)) env]))
                                                       end (. js/performance now)]
                                                   [res (- end start)])]
                (do (swap! cells assoc-in [first-cell :result] cell-result)
                    (swap! cells assoc-in [first-cell :execution] time)
                    (run-cells! cells (rest cell-order) notebook-environment new-env)))))

(defn shared-input-field [[value selection]]
  [:textarea {:type "text"
              :rows (count (str/split-lines @value))
              :cols 90
              :class "top"
              :value (str/replace @value #"\\ " "‿")
              :focus (not (nil? @selection))
              :on-blur #(reset! selection nil)
              :on-click #(reset! selection (-> %
                                               .-target
                                               .-selectionStart))
              :on-change #(do
                            (reset! value
                                    (-> %
                                        .-target
                                        .-value))
                            (reset! selection
                                    (-> %
                                        .-target
                                        .-selectionStart)))
              :on-key-up #(do (reset! selection
                                      (-> %
                                          .-target
                                          .-selectionStart)))}])

(defn show-formatted-input-value [value selection]
  (into [] (concat [:div {:font-family "monospace"
                          :class "bottom"
                          :background-color "lightgoldenrodyellow"}]
                   (format/get-hiccup
                    (parser/expression-list (-> @value
                                                (str/replace #"\\ " "‿")
                                                (str/replace #"\\" " ")))
                    @selection))))

(defn shared-input [var-name v+selection env cells cell-order]
  (let [value (atom (let [s (get-in @env [var-name :value 'shared-input :value 1]
                                    (second (first (v+selection 'value))))]
                      (subs s 1 (dec (count s)))))
        selection (atom (second (v+selection 'value)))
        input-field [:div
                     {:class "outer"
                      :style (assoc style/cell-output-style
                                    :background-color "lightgoldenrodyellow"
                                    :border "2px solid grey"
                                    :border-radius "3px")}
                     [shared-input-field [value selection]]
                     [show-formatted-input-value value selection]]]
    {'html
     [:div
      {:on-change #(do
                     (println "Putting " var-name " into environment")
                     (swap! env assoc
                            var-name {:role :variable
                                      :value {'shared-input
                                              {:value [:#_string (str "\"" @value "\"")]
                                               :selection @selection
                                               'html [:div {:on-change (fn [] (do
                                                                                (println "Updating shared inputs from a downstream cell")
                                                                                (swap! env assoc-in
                                                                                       [var-name :value 'shared-input :value]
                                                                                       [:#_string (str "\"" @value "\"")])
                                                                                (swap! env assoc-in
                                                                                       [var-name :value 'shared-input :selection]
                                                                                       @selection)
                                                                                (run-cells! cells @cell-order env @env)))}
                                                      input-field]}}})
                     (println "Shared input running cells")
                     (run-cells! cells @cell-order env @env))}
      input-field]
      :value [:#_string (str "\"" @value "\"")]}))

(defn shared-input-value [v] (get-in v [:value 'shared-input :value]))
