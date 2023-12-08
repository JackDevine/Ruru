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

(defn new-cell-data [] {:val "" :selection nil :result nil :expression-list '() :show-code true})

(defonce cells (atom {(random-uuid) (new-cell-data)}))

(defonce cell-order (atom [(first (keys @cells))]))

(defonce notebook-environment (atom ruru/default-environment))

(defonce loading-done (atom false))

(defn tokenize-cell! [cells cell-id]
  (let [cell-val (get-in @cells [cell-id :val])
        new-expression-list (ruru/expression-list (str/replace cell-val #"\\ " "‿"))]
    (swap! cells #(assoc-in % [cell-id :expression-list] new-expression-list))))

(defn run-cells! [cells cell-order env]
  (cond (empty? cell-order) (reset! notebook-environment env)
        :else (let [first-cell (first cell-order)
                    cell-exp-list (:expression-list (@cells first-cell))
                    [[cell-result new-env] time] (let [start (. js/performance now)
                                                       res (ruru/interpret-exp-list cell-exp-list env)
                                                       end (. js/performance now)]
                                                   [res (- end start)])]
                (do (swap! cells assoc-in [first-cell :result] cell-result)
                    (swap! cells assoc-in [first-cell :execution] time)
                    (run-cells! cells (rest cell-order) new-env)))))

(defn atom-input [value selection cell-id]
  [:textarea {:type "text"
              :rows (count (str/split-lines @value))
              :cols 90
              :style style/cell-input-style
              :class "ruru-cell"
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
                                        .-selectionStart))
                            (tokenize-cell! cells cell-id)
                            (run-cells! cells @cell-order ruru/default-environment))
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
    (into [] (concat [:tr {:style {:border "0px solid black"
                                   :width "2px"}}]
                     (mapv #(into []
                             (concat [:td]
                                     [(show-result %)])) row-vals)))))

(defn show-array [arr]
  (let [dims (arr 'array_dims)
        value (arr 'value)
        cols (partition-all (/ (reduce * dims) (second dims)) value)]
    [:div {:style {:outline "2px solid grey"
                   "max-height" "15em"
                   "overflow-x" "scroll"
                   "overflow-y" "scroll"
                   "white-space" "nowrap"}}
     [:span (str (apply str (interpose "×" dims)) " array\n")]
     (into [] (concat [:table {:style {"table-layout" "fixed"
                                       :width (str (* 60 (second dims)) "px")
                                       :border "0px black"}}]
                      (map #(cols->row-hiccup cols %) (range (first dims)))))]))

(defn show-map [m & header-names]
  (into
   []
   (concat [:table {:style {"table-layout" "fixed" :width "100%"}}]
           [[:tr
             [:th
              {:style {:text-align "left"}}
              (if header-names (first header-names) "Key")]
             [:th
              {:style {:text-align "left"}}
              (if header-names (second header-names) "Value")]]
            (for [kv (seq m)] [:tr
                               [:td [:div {:style {:overflow-x "scroll"}} (show-result (first kv))]]
                               [:td [:div {:style {:overflow-x "scroll"}} (show-result (second kv))]]])])))

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
    (ruru/ruru-string? r) [:div
                           {:style (assoc style/string-style :width "580px" :overflow "scroll")}
                           (second r)]
    (ruru/ruru-array? r) [:div {:style {"width" "580px"}} (show-array r)]
    (ruru/html? r) [:div {:style {:width "580px"}} (show-html r)]
    (map? r) [:div {:style {"max-width" "580px"}} (show-map r)]
    (keyword? r) [:div {:style {:width "580px" :overflow "scroll"}}
                  (apply str (rest (str r)))]
    :else [:div {:style {:width "580px" :overflow "scroll"}} (str r)]))

(defn show-environment [env]
  (let [ks (clojure.set/difference (set (keys env)) (set (keys ruru/default-environment)))
        defined-vars (into {} (for [k ks] [k (:value (get env k nil))]))]
    [:div (assoc {:style style/variable-explorer-style} "width" "600px")
     (show-map defined-vars "Variable name" "Value")]))

(defonce current-notebook (atom ""))

(defonce display-create-new-notebook (atom "none"))

(defn set-notebook-into-storage [name notebook-content]
  (.setItem (.-localStorage js/window) name
            notebook-content))

(defn hide-all-code [cells-vec]
  (into {} (map #(assoc-in % [1 :show-code] false) cells-vec)))

(defn show-all-code [cells-vec]
  (into {} (map #(assoc-in % [1 :show-code] true) cells-vec)))

(defn get-saved-notebooks []
  (let [n (.-length (.-localStorage js/window))]
    (if (= 0 n)
      (do (set-notebook-into-storage "untitled.ruru"
                                     (let [id (random-uuid)]
                                       {:cell-order [id] :cells {id {:val "" :show-code true}}}))
          (get-saved-notebooks))
      (into [] (for [i (range n)]
               (.key (.-localStorage js/window) i))))))

(defn available-name [notebook-name]
  (let [[name extension] (str/split notebook-name #"\.")
        saved-notebooks (get-saved-notebooks)]
    (cond
      (= notebook-name "") (available-name "untitled.ruru")
      (contains? saved-notebooks notebook-name) (str (str name "_edit.") extension)
      :else notebook-name)))

(defn save-notebook-impl! [name notebook-content]
  (let [saved-notebooks (get-saved-notebooks)]
    (cond (= "" name) (do (reset! current-notebook "untitled.ruru")
                          (save-notebook-impl! "untitled.ruru" notebook-content))
          (and
           (not= name @current-notebook)
           (contains? (set saved-notebooks) name)) (let
                                                    [new-name (available-name name)]
                                                     (save-notebook-impl! new-name notebook-content))
          :else (set-notebook-into-storage name notebook-content))))

(defn select-first-cell! []
  (let [id (first @cell-order)]
    (do (swap! cells (fn [x] (assoc-in x [id :selection] (count (get-in x [id :val])))))
        (swap! cells (fn [x] (assoc-in x [id :focus] true)))))
  (-> js/document
      (.querySelector ".ruru-cell")
      .focus))

(defn select-cell! [n]
  (let [id (@cell-order n)]
    (do (swap! cells (fn [x] (assoc-in x [id :selection] (count (get-in x [id :val])))))
        (swap! cells (fn [x] (assoc-in x [id :focus] true)))))
  (-> js/document
      (.querySelectorAll ".ruru-cell")
      (.item n)
      .focus))

(defn save-notebook!
  ([name]
   (save-notebook-impl! name (pr-str {:cell-order @cell-order
                                      :cells (into {} (for [[id cell] @cells]
                                                        [id (select-keys cell [:val :show-code])]))})))
  ([name notebook-content] (save-notebook-impl! name notebook-content)))

(defn load-cell [c]
  {:show-code (:show-code c)
   :val (:val c)
   :selection nil :result nil
   :expression-list (ruru/expression-list (:val c))})

(defn load-notebook [name]
  (let [notebook-data (edn/read-string (.getItem (.-localStorage js/window) name))
        cell-order-data (:cell-order notebook-data)
        cell-data (:cells notebook-data)
        cell-data cell-data]
    (if (= (mapv :val (vals cell-data)) (mapv :val (vals @cells)))
      nil
      (do
        (println (str "Loading " name))
        (reset! notebook-environment ruru/default-environment)
        (reset! cell-order cell-order-data)
        (reset! cells (into {} (for [[id c] cell-data] [id (load-cell c)])))
        (run-cells! cells @cell-order @notebook-environment)
        (select-first-cell!)))))

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
                                     (.removeItem (.-localStorage js/window) n))} "X"]]])))])

(defn add-cell-below! [cell-id]
  (let [indexed-cell-order (map-indexed vector @cell-order)
        matching-cells (filter #(= cell-id (second %)) indexed-cell-order)
        [cell-number _] (first matching-cells)
        new-uuid (random-uuid)
        new-cell (new-cell-data)
        [before after] (split-at (inc cell-number) @cell-order)
        new-cell-order (into [] (concat before [new-uuid] after))]
    (do (save-notebook!
         @current-notebook
         (pr-str {:cell-order new-cell-order :cells (assoc @cells new-uuid new-cell)}))
        (load-notebook @current-notebook)
        (select-cell! (inc cell-number)))))

(defn delete-cell! [cell-id]
  (let [indexed-cell-order (map-indexed vector @cell-order)
        matching-cells (filter #(= cell-id (second %)) indexed-cell-order)
        [cell-number _] (first matching-cells)
        new-cell-order (into [] (remove #{cell-id} @cell-order))
        new-cells (dissoc @cells cell-id)]
    (do (save-notebook!
         @current-notebook
         (pr-str {:cell-order new-cell-order :cells new-cells}))
        (load-notebook @current-notebook)
        (select-cell! cell-number))))

(defn create-cell [val selection cell-id]
  [:div {:class "flex-container"}
   [:img {:on-click #(do (swap! cells (fn [x] (update-in x [cell-id :show-code] not)))
                         (save-notebook! @current-notebook))
          :src (if (get-in @cells [cell-id :show-code] false) "assets/favicon.svg" "assets/eye_crossed.svg")
          :style {:width "40px" :height "20px"}}]
   [:div
    [:div {:class "grid-container"}
     [:div {:class "outer"
            :style {:display (if (get-in @cells [cell-id :show-code]) "" "none")}}
      [:div {:class "top"
             :style {:opacity 0}}
       [atom-input val selection cell-id]]
      (into [] (concat formatted-input (ruru/get-hiccup (get-in @cells [cell-id :expression-list]) @selection)))]
     [:div {:style {:display (if (get-in @cells [cell-id :show-code]) "" "none")}}
      (str "  " (get-in @cells [cell-id :execution] "") "ms")]]
    [:div {:style style/cell-output-style}
     [:div {:class "grid-container"}
      [:div {:class "flex-container"} (show-result (get-in @cells [cell-id :result]))]
      [:div {:class "flex-container" :style {:justify-content "top"}}
       [:button {:on-click #(add-cell-below! cell-id)
                 :style {"font-size" "0.9em" :width "15px" :background "transparent" :border "0px black"}}
        "+"]
       [:button {:on-click #(delete-cell! cell-id)
                 :style {"font-size" "0.9em" :width "15px" :background "transparent" :border "0px black"}}
        [:img {:width "15px" :src "assets/delete.png"}]]]]]]])

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
                                     (save-notebook! new-notebook-name (let [id (random-uuid)]
                                                                         (pr-str
                                                                          {:cell-order [id]
                                                                           :cells {id {:val "" :show-code true}}})))
                                     (load-notebook new-notebook-name)
                                     (reset! current-notebook new-notebook-name)
                                     (swap! display-create-new-notebook
                                            (fn [x] (if (= x "none") "block" "none")))
                                     (select-first-cell!))))}
     "Save new notebook"]]])

(defn manage-notebooks-page []
  (fn [] [:span.main
          [:title "Manage notebooks"]
          (create-new-notebook-dialog)
          [:div (list-saved-notebooks (get-saved-notebooks))]]))

(defn notebook-page []
  (fn [] [:span.main
          {:on-load #(let [saved-notebooks (get-saved-notebooks)]
                       (if @loading-done
                         nil
                         (do (println "Loading...")
                             (load-notebook (first saved-notebooks))
                             (reset! current-notebook (first saved-notebooks))
                             (swap! loading-done not)
                             (select-first-cell!))))
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
            (into [] (concat [:select {:value @current-notebook
                                       :on-change #(do
                                                     (println (str "Current notebook " @current-notebook))
                                                     (println (str "Selecting " (-> % .-target .-value)))
                                                     (save-notebook! @current-notebook)
                                                     (reset! current-notebook (-> % .-target .-value))
                                                     (load-notebook @current-notebook)
                                                     (println (str "Current notebook " @current-notebook))
                                                     (select-first-cell!))}]
                             (for [name (get-saved-notebooks)] [:option name])))
            [:button
             {:on-click #(do (save-notebook! @current-notebook))}
             "Save notebook"]
            [:a {:href (rfe/href ::manage_notebooks)} "Manage saved notebooks"]]
           [:div {:style {:margin-top "-40px" :margin-left "150px" :margin-bottom "50px"}}
            (create-new-notebook-dialog)]]
          [:br]
          [:button {:on-click #(do (swap! cells show-all-code)
                                   (save-notebook! @current-notebook))}
           "Show all cells"]
          [:button {:on-click #(do (swap! cells hide-all-code)
                                   (save-notebook! @current-notebook))}
           "Hide all cells"]
          [:br]
          [:br]
          [:div {:style {:padding-left "100px"}}
           (into [] (concat [:div] (mapv #(create-cell
                                           (reagent/cursor cells [% :val])
                                           (reagent/cursor cells [% :selection]) %)
                                         @cell-order)))
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
