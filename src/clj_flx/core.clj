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

(defn boundary-p
  "Check if LAST-CHAR is the end of a word and CHAR the start of the next.

  This function is camel-case aware."
  [last-char char]
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
        res
        (let [ch (nth index str)
              down-char (if (capital-p ch) (str/lower-case ch) ch)]
          (recur (if (capital-p ch)
                   (merge res
                          { ch index }
                          { down-char index })
                   (merge res
                          { down-char index }))
                 (dec index)))))))

(defn score
  "Return best score matching QUERY against STR."
  [^String str ^String query]
  (when-not (or (str/blank? str)
                (str/blank? query))
    ;; TODO: ..
    ))
