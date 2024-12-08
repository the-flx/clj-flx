(ns clj-flx.core
  "Rewrite emacs-flx in Clojure"
  (:require [clojure.string :as str]))

(def default-score "Default score." -35)

(def penalty-lead "Panality lead." (int \.))

(def word-separators
  "List of characters that act as word separators in flx."
  '(\  \- \_ \: \. \/ \\))

(defn word-p
  "Check if CHAR is a word character."
  [^Character ch]
  (and ch
       (not (some #(= ch %) word-separators))))

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
  (or (nil? last-char)
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

(defn fill-vec
  "Fill list with INITIAL-VALUE."
  [length inital-value]
  (loop [_vec []]
    (if (< (count _vec) length)
      (recur (concat _vec [inital-value]))
      _vec)))

(defn get-heatmap-str
  "Generate the heatmap vector of string."
  [str ^Character group-separator]
  (let [str-len (count str)
        scores (atom (fill-vec str-len default-score))
        str-last-index (- str-len 1)
        group-alist (atom (vector (vector -1 0)))
        last-char (atom nil)
        group-word-count (atom 0)
        index1 (atom 0)]
    ;; final char bonus
    (let [last-score (+ (last @scores) 1)]
      (reset! scores (concat (drop-last @scores) (list last-score))))
    (doseq [ch (seq (char-array str))]
      (let [effective-last-char (if (= @group-word-count 0) nil @last-char)]
        (when (boundary-p effective-last-char ch)
          (let [vec (nth @group-alist 0)
                first (vector (nth vec 0) (nth vec 1) @index1)
                rest (rest (rest vec))
                merged (into [] (concat first rest))]
            (reset! group-alist (assoc @group-alist 0 merged)))))
      (when (and (not (word-p @last-char))
                 (word-p ch))
        (swap! group-word-count inc))
      ;; ++++ -45 penalize extension
      (when (and last-char
                 (= last-char penalty-lead))
        (reset! scores (assoc @scores @index1 (- (nth @scores @index1) 45))))
      (when (and group-separator
                 (= group-separator ch))
        (reset! group-alist
                (assoc @group-alist 0
                       (assoc (nth @group-alist 0) 1 @group-word-count)))
        (reset! group-word-count 0)
        (reset! group-alist (concat [[@index1 @group-word-count]] @group-alist)))
      (if (= @index1 str-last-index)
        (reset! group-alist (assoc @group-alist 0
                                   (assoc (nth @group-alist 0) 1 @group-word-count)))
        (reset! last-char ch))
      (swap! index1 inc))

    (let [group-count (count @group-alist)
          separator-count (- group-count 1)
          index2 (atom separator-count)
          last-group-limit (atom nil)
          base-path-found (atom false)]
      ;; ++++ slash group-count penalty
      (when-not (= separator-count 0)
        (reset! @scores (inc-vec @scores (* group-count -2) nil nil)))
      ;; score each group further
      (doseq [group @group-alist]
        (let [group-start (nth group 0)
              word-count (nth group 1)
              ;; this is the number of effective word groups
              words-len (- (count group) 2)
              base-path-p (atom false)
              num (atom 0)]

          (when (and (not (= words-len 0))
                     (not @base-path-found))
            (reset! base-path-found true)
            (reset! base-path-p true))

          (if base-path-p
            ;; ++++ basepath separator-count boosts
            (let [boost (if (> separator-count 1) (- separator-count 1) 0)
                  penalty (- word-count 0)]
              ;; ++++ basepath word count penalty
              (reset! num (+ 35 boost penalty)))
            ;; ++++ non-basepath penalties
            (reset! num (if (= @index2 0)
                          -3
                          (+ -5 @index2 -1))))

          (reset! @scores (inc-vec @scores @num
                                   (+ group-start 1)
                                   @last-group-limit))

          (let [cddr-group (rest (rest group))
                word-index (atom (- words-len 1))
                last-word (atom (if (nil? @last-group-limit) str-len @last-group-limit))]
            (doseq [word cddr-group]
              ;; ++++  beg word bonus AND
              (reset! @scores (assoc @scores word (+ (nth @scores word) 85)))
              (let [index3 (atom word)
                    char-i (atom 0)]

                (while (< @index3 @last-word)
                  (reset! @scores (assoc @scores @index3
                                         (+ (nth @scores @index3)
                                            (- (* @word-index -3)  ; ++++ word order penalty
                                               @char-i)))) ; ++++ char order penalty
                  (swap! char-i inc)
                  (swap! index3 inc))
                (reset! last-word word)
                (swap! word-index dec)))
            (reset! last-group-limit (+ group-start 1))
            (swap! index2 dec)))))
    @scores))

(println (get-heatmap-str "switch-to-buffer" nil))

(let [group-alist (atom (vector (vector -1 0)))
      group-word-count 45
      index1 10]
  (println @group-alist)
  (reset! group-alist
          (assoc @group-alist 0 (assoc (nth @group-alist 0) 1 group-word-count)))
  (println @group-alist)
  ;;(reset! group-alist (concat [[index1 group-word-count]] @group-alist))
  @group-alist)

(assoc [1 2 3] 0 5)
(nth  [1 2 3] 0)

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
