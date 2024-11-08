(ns rads.json-key-fns
  (:require [charred.api :as charred]
            [charred.coerce :as coerce]
            [clojure.test :refer [deftest is] :as test]
            [cheshire.core :as cheshire]
            [clojure.data.json :as data-json]
            [camel-snake-kebab.core :as csk]
            [jsonista.core :as jsonista])
  (:import (charred JSONWriter)
           (clojure.lang MapEntry)
           (java.util List Map Map$Entry)
           (java.util.function BiConsumer)))

(def in {:foo {:bar-baz 1}})
(def out "{\"foo\":{\"bar_baz\":1}}")
(def encode-key csk/->snake_case_string)

(deftest jsonista-test
  (let [mapper (jsonista/object-mapper {:encode-key-fn encode-key})]
    (is (= out (jsonista/write-value-as-string in mapper)))))

(deftest data-json-test
  (is (= out (data-json/write-str in {:key-fn encode-key}))))

(deftest cheshire-test
  (is (= out (cheshire/generate-string in {:key-fn encode-key}))))

(defn ^{:tag BiConsumer} custom-obj-fn [encode-key-fn]
  (let [encode-key-fn' (or encode-key-fn identity)]
    (reify BiConsumer
      (accept [this w value]
        (let [^JSONWriter w w]
          (let [value (when-not (nil? value) (charred/->json-data value))]
            (cond
              (or (sequential? value)
                  (instance? List value)
                  (.isArray (.getClass ^Object value)))
              (.writeArray w (coerce/->iterator value))
              (instance? Map value)
              (.writeMap w (coerce/map-iter (fn [^Map$Entry e]
                                              (MapEntry. (charred/->json-data (encode-key-fn' (.getKey e)))
                                                         (.getValue e)))
                                            (.entrySet ^Map value)))
              :else
              (.writeObject w value))))))))

(deftest charred-test
  (is (= out (charred/write-json-str in {:obj-fn (custom-obj-fn encode-key)}))))

(defn -main [& _]
  (test/run-tests 'rads.json-key-fns))

(comment
  (-main))
