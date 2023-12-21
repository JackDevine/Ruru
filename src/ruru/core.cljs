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
   [ruru.style :as style]
   [ruru.base.base :as base]
   [ruru.format :as format]
   [ruru.parser :as parser]))

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
    [:li [:a {:href (rfe/href ::notebook)} "Open an interactive notebook"]]
    [:li [:a {:href (rfe/href ::spreadsheet)} "Open an interactive spreadsheet"]]]])

(defn new-cell-data [] {:val "" :selection nil :result nil :expression-list '() :show-code true})

(defonce cells (atom {(random-uuid) (new-cell-data)}))

(defonce cell-order (atom [(first (keys @cells))]))

(defonce all-code-showing (atom true))

(defonce presentation-mode? (atom false))

(defonce presented-cell (atom (first @cell-order)))

(defonce notebook-environment (atom ruru/default-environment))

(defonce loading-done (atom false))

(defn tokenize-cell! [cells cell-id]
  (let [cell-val (get-in @cells [cell-id :val])
        new-expression-list (parser/expression-list (str/replace cell-val #"\\ " "‿"))]
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

(defn inc-presented-cell [c]
  (let [indexed-cells (map-indexed vector @cell-order)
        current-presented-cell (filter #(= c (second %)) indexed-cells)
        current-number (ffirst current-presented-cell)]
    (cond (= current-number (dec (count @cells))) c
          (nil? current-number) (first @cell-order)
          :else (nth @cell-order (inc current-number)))))

(defn dec-presented-cell [c]
  (let [indexed-cells (map-indexed vector @cell-order)
        current-presented-cell (filter #(= c (second %)) indexed-cells)
        current-number (ffirst current-presented-cell)]
    (cond (= current-number 0) c
          (nil? current-number) (first @cell-order)
          :else (nth @cell-order (dec current-number)))))

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

(defn set-argument [hiccup path value]
  (cond
    (nil? (get-in hiccup path value)) value
    :else (assoc-in hiccup path value)))

(defn cols->row-hiccup [cols row & [selection]]
  (let [row-vals (map #(nth % row) cols)
        row-hiccup (into [] (concat [:tr {:style {:border "0px solid black"}}]
                                    (mapv #(into []
                                                 (concat [:td>div {:style {:overflow-x "scroll"
                                                                           :width "auto"
                                                                           :min-height "1em"}}]
                                                         [(set-argument (show-result %) [1 :style :width] "auto")])) row-vals)))]
    (cond
      (empty? selection) row-hiccup
      (= (dec (first (selection 'value))) row) (assoc-in row-hiccup
                                                         [(inc (second (selection 'value))) 1 :style :border]
                                                         "2px solid green")
      :else row-hiccup)))

(defn show-array [arr & [selection style-args]]
  (let [dims (arr 'array_dims)
        value (arr 'value)
        cols (partition-all (/ (reduce * dims) (second dims)) value)
        nested-array (not (empty? (filter #(or (base/ruru-array? %) (map? %)) value)))
        table (into [] (concat [:table {:style {"table-layout" (if nested-array "auto" "fixed")
                                                :width (if @presentation-mode? "100%" (str (* 60 (second dims)) "px"))
                                                :border "0px black"}}]
                               (map #(cols->row-hiccup cols % selection) (range (first dims)))))]
    [:div {:style (merge
                   {:outline "2px solid grey"
                    "max-height" (if @presentation-mode? "" "15em")
                    "max-width" "80vmax"
                    "overflow-x" (if @presentation-mode? "visible" "scroll")
                    "overflow-y" "scroll"
                    "white-space" "nowrap"}
                   style-args)}
     [:span (if (get arr 'show_dims true) (str (apply str (interpose "×" dims)) " array\n") "")]
     table]))

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

(defn show-html [x]
  (cond (base/ruru-array? (x 'html)) (ruru/extract-string (first (ruru/extract-list [(x 'html)])))
        :else (x 'html)))

(defn show-result [r & show-width]
  (let [show-width (if (empty? show-width) "580px" show-width)
        overflow-style (if @presentation-mode? "wrap" "scroll")
        html-font-family (if @presentation-mode? "Arial" "monospace")]
    (cond
      (base/ruru-string? r) [:div
                             {:style (assoc style/string-style
                                            :width (if @presentation-mode? "auto" show-width)
                                            :overflow "scroll")}
                             (second r)]
      (base/ruru-array? r) [:div {:style (if @presentation-mode?
                                           {:overflow overflow-style
                                            :width "auto"}
                                           {"width" show-width
                                            :overflow overflow-style})} (show-array r)]
      (base/html? r) [:div {:style {:white-space "normal" :font-family html-font-family}} (show-html r)]
      (map? r) [:div {:style {"max-width" show-width}} (show-map r)]
      (keyword? r) [:div {:style {:width (if @presentation-mode? "auto" show-width)
                                  :overflow "scroll"}}
                    (apply str (rest (str r)))]
      :else [:div {:style {:width (if @presentation-mode? "auto" show-width)
                           :overflow "scroll"}} (str r)])))

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
   :expression-list (parser/expression-list (:val c))})

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
      (into [] (concat formatted-input (format/get-hiccup (get-in @cells [cell-id :expression-list]) @selection)))]
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

(defn present-cell [val selection cell-id]
  [:div {:class "flex-container" :style {:padding-left "50px"}}
   [:div
    [:div {:class "grid-container"}
     [:div {:class "outer"
            :style {:display (if (get-in @cells [cell-id :show-code]) "" "none")
                    :padding-top "50px"}}
      [:div {:class "top"
             :style {:opacity 0}}
       [atom-input val selection cell-id]]
      (into [] (concat (-> formatted-input
                           (assoc-in [1 :style :font-size] "1.8em"))
                       (format/get-hiccup (get-in @cells [cell-id :expression-list]) @selection)))]]
    [:div {:style (assoc style/cell-output-style
                         :font-size "1.8em")}
     (show-result (get-in @cells [cell-id :result]) "60vmax")]]])

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

(defn render-interactive-mode []
  [:div [:div
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
          [:button {:on-click #(do (reset! presentation-mode? true)
                                   (reset! presented-cell (first @cell-order)))}
           "Presentation mode"]
          [:a {:href (rfe/href ::manage_notebooks)} "Manage saved notebooks"]]
         [:div {:style {:margin-top "-40px" :margin-left "150px" :margin-bottom "50px"}}
          (create-new-notebook-dialog)]]
   [:br]
   [:button {:on-click #(do (swap! cells show-all-code)
                            (reset! all-code-showing true)
                            (save-notebook! @current-notebook))
             :style {:border-color (if @all-code-showing "orange" "black")}}
    "Show all cells"]
   [:button {:on-click #(do (swap! cells hide-all-code)
                            (reset! all-code-showing false)
                            (save-notebook! @current-notebook))
             :style {:border-color (if (not @all-code-showing) "orange" "black")}}
    "Hide all cells"]
   [:br]
   [:br]
   [:div {:style {:padding-left "100px"}}
    (into [] (concat [:div] (mapv #(create-cell
                                    (reagent/cursor cells [% :val])
                                    (reagent/cursor cells [% :selection]) %)
                                  @cell-order)))
    [:button {:on-click #(reset! notebook-environment ruru/default-environment)} "Reset notebook"]
    [:div {:style {:position "absolute" :top 0 :right 0}} (show-environment @notebook-environment)]]])

(defn render-presentation-mode []
  [:div
   (do
     (println (str "Starting presentation mode for " @current-notebook))
     (present-cell (reagent/cursor cells [@presented-cell :val])
                   (reagent/cursor cells [@presented-cell :selection]) @presented-cell))])

(defn notebook-page []
  (fn [] [:span.main
          {:on-load #(let [saved-notebooks (get-saved-notebooks)]
                       (if @loading-done
                         nil
                         (do (println "Loading...")
                             (load-notebook (first saved-notebooks))
                             (reset! current-notebook (first saved-notebooks))
                             (swap! loading-done not)
                             (select-first-cell!)
                             (.addEventListener
                              js/window
                              "keydown"
                              (fn [e]
                                (cond
                                  (and (.-ctrlKey e) (= "s" (.-key e))) (do
                                                                          (.preventDefault e)
                                                                          (try
                                                                            (save-notebook! @current-notebook)
                                                                            (catch js/Error e (println "Save failed" e))))
                                  (= "Escape" (.-key e)) (reset! presentation-mode? false)
                                  (and (.-ctrlKey e) (= "ArrowRight" (.-key e))) (swap! presented-cell inc-presented-cell)
                                  (and (.-ctrlKey e) (= "ArrowLeft" (.-key e))) (swap! presented-cell dec-presented-cell)
                                  :else nil))))))}
          [:title @current-notebook]
          (if @presentation-mode?
            (render-presentation-mode)
            (render-interactive-mode))]))

(defonce spreadsheet-selection (atom {'array_dims [2 1] 'value [1 1]}))

(defonce spreadsheet-data (atom {'array_dims [5 5] 'value (into [] (repeat 25 "")) 'show_dims false}))

(defonce spreadsheet-formulas (atom {'array_dims [5 5] 'value (into [] (repeat 25 "")) 'show_dims false}))

(defonce spreadsheet-input (atom ""))

(defn spreadsheet [selection]
  (show-array @spreadsheet-data selection {:outline "0px solid grey"}))

(defn new-spreadsheet-selection [current-selection key-event]
  (let [key (.-key key-event)
        [row col] (current-selection 'value)]
    (cond (= key "ArrowRight") (assoc-in current-selection ['value] [row (inc col)])
          (= key "ArrowLeft") (assoc-in current-selection ['value] [row (dec col)])
          (= key "ArrowUp") (assoc-in current-selection ['value] [(dec row) col])
          (= key "ArrowDown") (assoc-in current-selection ['value] [(inc row) col])
          :else current-selection)))

(defn new-spreadsheet-formulas [current-formulas key-event]
  (let [key (.-key key-event)
        _ (println (str "Changing formulas following key " key))]
    (if (= key "Enter")
      (assoc-in current-formulas ['value
                                  (dec (base/ruru-linear-index
                                        (current-formulas 'array_dims)
                                        (@spreadsheet-selection 'value)))]
                @spreadsheet-input)
      current-formulas)))

(defn update-spreadsheet-data! []
  (swap! spreadsheet-data
         assoc-in ['value] (mapv #(first (ruru/interpret % (merge ruru/default-environment
                                                                  {:s {:role :variable :value @spreadsheet-data}})))
                                 (@spreadsheet-formulas 'value))))

(defn spreadsheet-page []
  (fn [] [:span.main
          [:title "Spreadsheet"]
          [:input {:type "text"
                   :value (str/replace @spreadsheet-input #"\\ " "‿")
                   :id "spreadsheet-input"
                   :on-change #(do
                                 (reset! spreadsheet-input
                                         (-> %
                                             .-target
                                             .-value)))
                   :on-key-down #(do
                                   (if (= "Escape" (.-key %))
                                     (-> js/document
                                         (.querySelector "#select-cell-scratch")
                                         .focus)
                                     nil)
                                   (swap! spreadsheet-formulas (fn [f] (new-spreadsheet-formulas f %)))
                                   ; TODO update the environment that cells are interpreted in so that the spreadsheet doesn't need to be updated twice
                                   (update-spreadsheet-data!)
                                   (update-spreadsheet-data!))}]
          [:input {:type "text"
                   :id "select-cell-scratch"
                   :style {:opacity 0}
                   :on-key-down #(do
                                   (if (= "Enter" (.-key %))
                                     (-> js/document
                                         (.querySelector "#spreadsheet-input")
                                         .focus)
                                     nil)
                                   (swap! spreadsheet-selection (fn [s] (new-spreadsheet-selection s %)))
                                   (reset! spreadsheet-input
                                           (get-in
                                            @spreadsheet-formulas
                                            ['value (dec (base/ruru-linear-index
                                                          (@spreadsheet-formulas 'array_dims)
                                                          (@spreadsheet-selection 'value)))])))}]
          [:div {:on-click #(println "spreadsheet clicked")} (spreadsheet @spreadsheet-selection)]]))

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
   ["/spreadsheet"
    {:name ::spreadsheet
     :view spreadsheet-page}]
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
