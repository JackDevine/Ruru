(ns ruru.style)

(def string-style {:color "red"})

(def cell-output-style
  {:font-size "1.3em" :font-family "monospace"
   :padding-top "2px"
   :padding-bottom "10px"})

(def cell-input-style
  {:background-color "white"
   :font-family "monospace"
   :autocomplete "off"
   :autocorrect "off"
   :autocapitalize "off"
   :spellcheck "false"})

(def variable-explorer-style {"maxWidth" "400px" :overflow "scroll" :outline "2px solid grey"})
