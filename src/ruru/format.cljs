(ns ruru.format
  (:require [ruru.ruru-lang :as ruru]
            [ruru.base.base :as base]
            [ruru.parser :as parser]))

(defn highlight-selection-impl [s selection]
  [:span (subs s 0 selection)
   [:span {:style {:background-color "lightgrey" "borderLeft" "1px solid black"}}
    (let [highlighted (get s selection)]
      highlighted)]
   [:span (subs s (+ 1 selection))]])

(defn highlight-selection-new-line [s selection]
  (let [length-s (count (last (last s)))]
    (cond (= 0 selection) [:span {:style {:background-color "lightgrey" "borderLeft" "1px solid black"}}
                           " \n" [:span {:style {:background-color "lightblue"}} (nthrest s 3)]]
          :else [:span "\n" (concat (repeat (- selection 1) [:span {:style {:background-color "lightblue"}} " "])
                                    [[:span {:style {:background-color "lightgrey" "borderLeft" "1px solid black"}} " "]]
                                    (repeat (- length-s selection) [:span {:style {:background-color "lightblue"}} " "]))])))

(defn highlight-selection [unhighlighted-hiccup tokens selected-token-index selection]
  (cond (nil? selection) unhighlighted-hiccup
        (and (empty? tokens) (not (nil? selection))) [[:span
                                                       {:style {:background-color "lightgrey" "borderLeft" "1px solid black"}}
                                                       " "]]
        (empty? tokens) []
        (> selection ((last tokens) :end)) (into []
                                                 (concat
                                                  unhighlighted-hiccup
                                                  [[:span
                                                    {:style {:background-color "lightgrey" "borderLeft" "1px solid black"}}
                                                    " "]]))
        (nil? selected-token-index) unhighlighted-hiccup
        :else (let [target-hiccup (nth unhighlighted-hiccup selected-token-index)
                    selected-token (nth tokens selected-token-index)
                    start (selected-token :start)
                    s (last target-hiccup)
                    target-hiccup (cond
                                    (not (string? s)) (highlight-selection-new-line target-hiccup (- selection start))
                                    :else (assoc-in target-hiccup [2] (highlight-selection-impl s (- selection start))))]
                (assoc-in unhighlighted-hiccup [selected-token-index] target-hiccup))))

(defn token->hiccup [t]
  (let
   [val (:value t)]
    (cond
      (= "," val) [:span {:style {:color "black"}} val]
      (base/ruru-string? val) [:span {:style {:color "red"}} (second val)]
      (= "\n" val) [:span {} " \n"]
      (contains? #{"\n\t" "\n  " "\n    "} val) [:span
                                                 {}
                                                 "\n"
                                                 [:span
                                                  {:style {"borderLeft" "1px solid pink"}}
                                                  (subs val 1)]]
      (= :whitespace (:role t)) [:span {} val]
      (= :function (:role t)) [:span {:style {:color "blue" :font-weight "bold"}} val]
      (and (:role-changed t) (= :variable (:role t))) [:span
                                                       (str "~" (subs (str (:name t)) 1))]
      (= :variable (:role t)) [:span {} (subs (str (:name t)) 1)]
      (= :number (:role t)) [:span {:style {:color "grey"}} (str val)]
      (= :comment (:role t)) [:span
                              {:style {:font-style "italic" :color "green"}}
                              (second val)]
      :else [:span {} (str val)])))

(defn selection-in-token [token selection]
  (and (<= (token :start) selection) (<= selection (token :end))))

(defn find-selected-token-impl [tokens selection [lower upper]]
  (let [lower-token (nth tokens lower)
        upper-token (nth tokens upper)
        center (Math/ceil (/ (+ lower upper) 2))
        center-token (nth tokens center)]
    (cond
      (selection-in-token lower-token selection) lower
      (selection-in-token center-token selection) center
      (selection-in-token upper-token selection) upper
      :else (cond
              (> selection (center-token :end)) (find-selected-token-impl tokens selection [(inc center) (dec upper)])
              (< selection (center-token :start)) (find-selected-token-impl tokens selection [(inc lower) (dec center)])
              :else nil))))

(defn find-selected-token [tokens selection [lower upper]]
  (cond (empty? tokens) nil
        (or (< selection 0) (> selection ((last tokens) :end))) nil
        :else (find-selected-token-impl tokens selection [lower upper])))

(defn get-hiccup-exp [exp selection]
  (let [tokens (into [] (parser/tokenize exp))
        selected-token-index (find-selected-token tokens selection [0 (dec (count tokens))])
        unhighlighted-hiccup (mapv token->hiccup tokens)]
    (highlight-selection unhighlighted-hiccup tokens selected-token-index selection)))

(defn get-hiccup [s selection]
  (let [exp-list (if (string? s) (parser/expression-list s) s)
        token-list (apply concat (interpose '("\n") exp-list))]
    (get-hiccup-exp token-list selection)))
