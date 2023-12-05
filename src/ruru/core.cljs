(ns ^:figwheel-hooks ruru.core
  (:require
   [goog.dom :as gdom]
   [clojure.string :as str]
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   [reitit.frontend :as rf]
   [reitit.frontend.easy :as rfe]
   [reitit.coercion.spec :as rss]
   [clojure.edn :as edn]
   [ruru.ruru-lang :as ruru]
   [ruru.style :as style]))

(defn home-page []
  [:div
   [:title "Welcome to ruru"]
   ; TODO use a grid to format better
   [:div [:img {:src "assets/ruru_icon.png" :style {:width "192px" :height "108px" :margin-left "-45px"}}]
    [:div {:style {:font-size "2em" :margin-top "-75px" :margin-left "110px"}}
     "Welcome to ruru"]]
   [:br]
   [:br]
   [:br]
   [:div "A lightweight programming environment for solving problems of any size."]
   [:ul
    [:li [:a {:href (rfe/href ::notebook)} "Open an interactive notebook"]]]])

(defonce cells (atom [{:val "" :selection nil :result nil :expression-list '()}]))

(defonce notebook-environment (atom ruru/default-environment))

(defn tokenize-cell [cells cell-id]
  (let [cell-val (get-in @cells [cell-id :val])
        new-expression-list (ruru/expression-list (str/replace cell-val #"\\ " "‿"))]
    (swap! cells #(assoc-in % [cell-id :expression-list] new-expression-list))))

(defn run-cells! [cells env n]
  (cond (= n (count @cells)) (reset! notebook-environment env)
        :else (let [cell-exp-list (:expression-list (@cells n))
                    [[cell-result new-env] time] (let [start (. js/performance now)
                                                       res (ruru/interpret-exp-list cell-exp-list env)
                                                       end (. js/performance now)]
                                                   [res (- end start)])]
                (do (swap! cells assoc-in [n :result] cell-result)
                    (swap! cells assoc-in [n :execution] time)
                    (run-cells! cells new-env (inc n))))))

(defn atom-input [value selection cell-id]
  [:textarea {:type "text"
              :rows (count (str/split-lines @value))
              :cols 90
              :style style/cell-input-style
              :id (str "ruru-cell-" cell-id)
              :value (str/replace @value #"\\ " "‿")
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
     [:span (str (apply str (interpose "×" dims)) " array\n")]
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


(defn extract-string-scalar [x]
  (cond (ruru/ruru-string? x) (subs (second x) 1 (dec (count (second x))))
        (map? x) (into {} (for [[k v] x] [(extract-string-scalar k) (extract-string-scalar v)]))
        :else x))

(defn extract-string [v]
  (mapv #(cond
           (vector? %) (extract-string %)
           :else (extract-string-scalar %)) v))

(defn show-html [x]
  (cond (ruru/ruru-array? (x 'html)) (extract-string (first (extract-list [(x 'html)])))
        :else (x 'html)))

(defn show-result [r]
  (cond
    (ruru/ruru-string? r) [:span {:style style/string-style} (second r)]
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
   [:div {:style style/cell-output-style} [:div {:class "grid-container"}
    [:div (show-result (get-in @cells [cell-id :result]))]
    [:div {:style {:text-align "right"}}
     (str (get-in @cells [cell-id :execution] "") "ms")]]]])

(defn add-new-cell! []
  (do (swap! cells #(into [] (concat % [{:val "" :selection nil :result nil :expression-list '()}])))))

(defn show-environment [env]
  (let [ks (clojure.set/difference (set (keys env)) (set (keys ruru/default-environment)))
        defined-vars (into {} (for [k ks] [k (:value (get env k nil))]))]
    [:div {:style style/variable-explorer-style}
     (show-map defined-vars "Variable name" "Value")]))

(defonce saved-notebooks (atom []))

(defonce notebook-name (atom ""))

(defonce current-notebook (atom ""))

(defonce display-create-new-notebook (atom "none"))

(defonce selected-notebook (atom ""))

(defn save-as-input [name]
  [:input {:type "text"
              :value @name
              :on-change #(reset! name
                           (-> %
                               .-target
                               .-value))}])

(defn update-saved-notebooks! []
  (let [n (.-length (.-localStorage js/window))
        notebooks (for [i (range n)]
                    (.key (.-localStorage js/window) i))]
    (reset! saved-notebooks notebooks)))

(defn available-name [saved-notebooks notebook-name]
  (let [[name extension] (str/split notebook-name #"\.")]
    (str (str name "_edit.") extension)))

(defn save-notebook-impl! [name notebook-content]
  (cond (= "" name) (save-notebook-impl! "untitled.ruru" notebook-content)
        (and
         (not= name @current-notebook)
         (contains? (set @saved-notebooks) name)) (let
                                                   [new-name (available-name @saved-notebooks name)]
                                                    (save-notebook-impl! new-name notebook-content))
        :else (do
                (.setItem (.-localStorage js/window) name
                          notebook-content)
                (update-saved-notebooks!)
                (reset! current-notebook name))))

(defn select-cell! [n]
  (swap! cells (fn [x] (assoc-in x [n :selection] (count (get-in x [n :val])))))
  (swap! cells (fn [x] (assoc-in x [n :focus] true)))
  (-> js/document
      (.getElementById "ruru-cell-0")
      .focus))

(defn save-notebook!
  ([name]
   (save-notebook-impl! name (pr-str (mapv :val @cells))))
  ([name notebook-content] (save-notebook-impl! name (pr-str notebook-content))))

(defn load-notebook [name]
  (let [cell-data (.getItem (.-localStorage js/window) name)
        cell-data (edn/read-string cell-data)]
    (do
      (save-notebook! @current-notebook)
      (reset! notebook-environment ruru/default-environment)
      (reset! cells (mapv (fn [x] {:val x :selection nil :result nil
                                   :expression-list (ruru/expression-list x)}) cell-data))
      (reset! notebook-name name)
      (reset! current-notebook @notebook-name)
      (run-cells! cells @notebook-environment 0)
      (select-cell! 0))))

(defn list-saved-notebooks [saved-notebooks]
  [:div
   (into []
         (concat
          [:ul]
          (for [n saved-notebooks]
            [:li
             [:div [:span {:on-click #(load-notebook n)} n]
              [:span "  "]
              [:button {:on-click #(do
                                     (.removeItem (.-localStorage js/window) n)
                                     (update-saved-notebooks!))} "X"]]])))])

(defn create-new-notebook-dialog []
  [:span [:button
          {:on-click (fn [] (do (swap! display-create-new-notebook
                                       (fn [x] (if (= x "none") "block" "none")))))}
          "Create new notebook"]
   [:span {:style {:display @display-create-new-notebook}}
    [:input {:id "new-notebook-name" :type "text"}]
    [:button {:on-click (fn [] (let [new-notebook-name (-> js/document
                                                           (.getElementById "new-notebook-name")
                                                           .-value)]
                                 (do (save-notebook! @current-notebook)
                                     (save-notebook! new-notebook-name [""])
                                     (load-notebook new-notebook-name)
                                     (reset! selected-notebook @notebook-name)
                                     (swap! display-create-new-notebook
                                            (fn [x] (if (= x "none") "block" "none"))))))}
     "Save new notebook"]]])

(defn manage-notebooks-page []
  (fn [] [:span.main
          [:title "Manage notebooks"]
          (create-new-notebook-dialog)
          [:div (list-saved-notebooks @saved-notebooks)]]))

(defn notebook-page []
  (fn [] [:span.main
          {:on-load #(do (update-saved-notebooks!)
                         (load-notebook (first @saved-notebooks))
                         (reset! selected-notebook (first @saved-notebooks))
                         (reset! current-notebook (first @saved-notebooks))
                         (select-cell! 0))
          ;;  :on-key-down (fn [e]
          ;;                 (if (and (.-ctrlKey e) (= "s" (.-key e)))
          ;;                   (do
          ;;                     (.preventDefault e)
          ;;                     (try
          ;;                       (save-notebook! @current-notebook)
          ;;                       (catch js/Error e (println "Save failed" e)))) nil))
           }
          [:title @current-notebook]
          [:div
           [:img {:src "assets/ruru_icon.png" :style {:width "192px" :height "108px" :margin-left "-45px"}}]
           [:div {:style {:margin-top "-100px" :margin-left "150px" :margin-bottom "50px"}}
            (into [] (concat [:select {:value @selected-notebook
                                       :on-change #(do (save-notebook! @current-notebook)
                                                       (reset! current-notebook (-> % .-target .-value))
                                                       (reset! selected-notebook @current-notebook)
                                                       (load-notebook @current-notebook))}]
                             (for [name @saved-notebooks] [:option name])))
            [:button
             {:on-click #(do (save-notebook! @notebook-name)
                             (reset! current-notebook @notebook-name)
                             (load-notebook @notebook-name))}
             "Save notebook"]
            [:a {:href (rfe/href ::manage_notebooks)} "Manage saved notebooks"]]
           [:div {:style {:margin-top "-40px" :margin-left "150px" :margin-bottom "50px"}}
            (create-new-notebook-dialog)]]
          [:br]
          [:br]
          [:br]
          [:div {:style {:padding-left "20px" "maxWidth" "700px"}}
           (into [] (concat [:div] (mapv #(create-cell (reagent/cursor cells [% :val]) (reagent/cursor cells [% :selection]) %) (range (count @cells)))))
           [:button {:style {:margin-top "10px"} :on-click #(swap! cells (fn [c] (into [] (drop-last c))))} "Delete"]
           [:button {:on-click add-new-cell!} "Create new cell"]
           [:button {:on-click #(reset! notebook-environment ruru/default-environment)} "Reset notebook"]
           [:div {:style {:position "absolute" :top 0 :right 0}} (show-environment @notebook-environment)]]]))

(defn get-app-element []
  (gdom/getElement "app"))

(defn mount [el]
  (rdom/render [home-page] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
;; (mount-app-element)

(defonce match (reagent/atom nil))

(defn current-page []
  [:div
   (if @match
     (let [view (:view (:data @match))]
       [view @match]))
  ;;  [:pre (with-out-str (fedn/pprint @match))]
   ])

(def routes
  [["/"
    {:name ::frontpage
     :view home-page}]
   ["/notebook"
    {:name ::notebook
     :view notebook-page}]
   ["/manage_notebooks"
    {:name ::manage_notebooks
     :view manage-notebooks-page}]])

(defn init! []
  (rfe/start!
   (rf/router routes {:data {:coercion rss/coercion}})
   (fn [m] (reset! match m))
    ;; set to false to enable HistoryAPI
   {:use-fragment true})
  (rdom/render [current-page] (.getElementById js/document "app")))

;; specify reload hook with ^:after-load metadata
(defn ^:after-load on-reload []
  (do 
    (mount-app-element)
    (init!))
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(init!)
