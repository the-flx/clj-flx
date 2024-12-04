(ns clj-flx.core
  "Rewrite emacs-flx in Clojure"
  (:require [clojure.string :as str]))

(def word-separators
  "List of characters that act as word separators in flx."
  '(\  \- \_ \: \. \/ \\))

(defn word-p
  "Check if CHAR is a word character."
  [^Character ch]
  (not (some #(= ch %) word-separators)))

(defn capital-p
  "Check if CHAR is an uppercase character."
  [^Character ch]
  (and ch
       (word-p ch)
       (= (str ch) (str/upper-case (str ch)))))

(defn boundary-p
  "Check if LAST-CHAR is the end of a word and CHAR the start of the next.

  This function is camel-case aware."
  [^Character last-char ^Character char]
  (or (not (some? last-char))
      (and (not (capital-p last-char))
           (capital-p char))
      (and (not (word-p last-char))
           (word-p char))))

(defn inc-vec
  "Increment each element in VEC between BEG and END by INC.

  INC defaults to 1.  BEG defaults to 0 and is inclusive.
  END is not inclusive and defaults to the length of VEC."
  [vec ^Integer inc ^Integer beg ^Integer end]
  (let [_inc (or inc 1)
        _beg (or beg 0)
        _end (or end (count vec))]
    (loop [vec vec
           _count 0
           _result (list)]
      (if (empty? vec)
        _result
        (recur (rest vec)
               (+ _count 1)
               (concat _result (list (if (and (<= _beg _count)
                                              (< _count _end))
                                       (+ (first vec) _inc)
                                       (first vec)))))))))

(defn get-hash-for-string
  "Return hash-table for string where keys are characters.

  Value is a sorted list of indexes for character occurrences."
  [str]
  (let [str-len (count str)]
    (loop [res (hash-map)
           index (- str-len 1)]
      (if (<= 0 index)
        (let [ch (get str index)
              down-char (if (capital-p ch) (str/lower-case ch) ch)
              new-index (list index)]
          (recur (if (capital-p ch)
                   (let [n-res (assoc-in res [ch]
                                         (concat new-index (get res ch)))]
                     (assoc-in n-res [down-char]
                               (concat new-index (get n-res down-char))))
                   (assoc-in res [down-char] (concat new-index (get res down-char))))
                 (dec index)))
        res))))

(defn score
  "Return best score matching QUERY against STR."
  [^String str ^String query]
  (when-not (or (str/blank? str)
                (str/blank? query))
    (let [str-info (get-hash-for-string str)
          query-len (count query)
          full-match-boost (and (< 1 query-len)
                                (< query-len 5))]
      ;; TODO: ..
      )))
