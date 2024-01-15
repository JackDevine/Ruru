(ns ruru.style)

(def string-style {:color "red"})

(def cell-font-size "1.5em")

(def cell-output-style
  {:font-family "monospace"
   :font-size cell-font-size
   :padding-top "2px"
   :padding-bottom "10px"})

(def cell-input-style
  {:background-color "white"
   :font-family "monospace"
   :font-size cell-font-size
   :autocomplete "off"
   :autocorrect "off"
   :autocapitalize "off"
   :spellcheck "false"})

(def cell-width "580px")

(def variable-explorer-style {"maxWidth" "400px" :overflow "scroll" :outline "2px solid grey"})