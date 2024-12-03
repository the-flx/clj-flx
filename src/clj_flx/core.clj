(ns clj-flx.core
  "Rewrite emacs-flx in Clojure"
  (:require [clojure.string :as str]))

(def word-separators
  "List of characters that act as word separators in flx."
  '(\  \- \_ \: \. \/ \\))

(defn word-p
  "Check if CHAR is a word character."
  [^Character char]
  (not (some #(= char %) word-separators)))

(defn capital-p
  "Check if CHAR is an uppercase character."
  [char]
  (and char
       (word-p char)
       (= (str char) (str/upper-case char))))

(defn score
  "Return best score matching QUERY against STR."
  [^String str ^String query]
  (when-not (or (str/blank? str)
                (str/blank? query))
    ;; TODO: ..
    ))
