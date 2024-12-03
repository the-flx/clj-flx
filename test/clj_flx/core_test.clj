(ns clj-flx.core-test
  (:require [clojure.test :refer :all]
            [clj-flx.core :refer :all]))

(deftest word-test-1
  (testing "Test word 1"
    (is (= (word-p \c) true))))

(deftest word-test-2
  (testing "Test word 2"
    (is (= (word-p \ ) false))))

(deftest capital-test-1
  (testing "Test capital 1"
    (is (= (capital-p \C) true))))

(deftest capital-test-2
  (testing "Test capital 2"
    (is (= (capital-p \c) false))))

(deftest inc-vec-test-2
  (testing "Test inc-vec 1"
    (is (= (inc-vec (list 1 2 3) 1 0 3) (list 2 3 4)))))

;; (deftest score-test-1
;;   (testing "Score `switch-to-buffer'"
;;     (is (= (first (score "switch-to-buffer" "stb")) 237))))
;;
;; (deftest score-test-2
;;   (testing "Score `TestSomeFunctionExterme'"
;;     (is (= (first (score "TestSomeFunctionExterme" "met")) 57))))
;;
;; (deftest score-test-3
;;   (testing "Score `MetaX_Version'"
;;     (is (= (first (score "MetaX_Version" "met")) 211))))
