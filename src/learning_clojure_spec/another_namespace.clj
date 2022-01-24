(ns learning-clojure-spec.another-namespace
  (:require [clojure.spec.alpha :as s]
            #_[learning-clojure-spec.core :as core]))

(s/def :equities.order/type #{:ask :bid})

(s/valid? :equities.order/type :buy)

(s/def ::execution-status #{:waiting :partially-executed :totally-executed :cancelled})