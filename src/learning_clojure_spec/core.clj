(ns learning-clojure-spec.core
  (:require [clojure.spec.alpha :as s]
            [learning-clojure-spec.another-namespace :as another-namespace])
  (:gen-class)
  (:import (java.util UUID Date)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;;any fn that accepts a single arg and returns truthy value is a valid spec
(defn fixed-income? [asset]
  (#{:lci :lca :cdb :bond :private-credit} (:asset-type asset)))

(defn stock? [asset]
  (#{:stock} (:asset-type asset)))

;;conform returns the data or :clojure.spec.alpha/invalid
(s/conform fixed-income? {:asset-type :lci})

(s/conform even? 1001)

;;if dont want to check for :clojure.spec.alpha/invalid or use the value, there is the valid? helper

(s/valid? fixed-income? {:asset-type :lci})

(s/valid? (comp :money-boxes :source) {:source {:self-directed {:id 1}}})

(s/valid? #{:sell :buy} (:order-type {:order-id 1 :order-type :bug}))


;;s/def associates a namespaced keyword with a spec and adds it to a global registry
(s/def :equities.order/type #{:sell :buy})
(s/def :funds.order/type #{:purchase :redemption})

;;can be conformed/validated using the registered namespace
(s/valid? :equities.order/type (:order-type {:order-id 1 :order-type :bbuy}))

(s/valid? ::another-namespace/execution-status :partially-executed)
;;* registered spec identifies should be namespaced in order to avoid conflicts
(s/valid? :equities.order/type :bid)

;;specs can be instrospected by doc function
;(doc ::another-namespace/execution-status)

(s/def :string/number (s/and #(try (Integer/parseInt %)
                                   (catch Exception _ false))
                             string?))

(s/conform :string/number "1")
(s/valid? :string/number "1")

;;ive found a modeling challenge that i didnt figure it out yet:
;; design specs for entities or attributes?
(s/def :b3.governance/novo-mercado (s/and #(-> % :ticker (clojure.string/ends-with? "3"))
                                          stock?))

(s/conform :b3.governance/novo-mercado {:asset-type :stock :ticker "ABEV3"})
(s/conform :b3.governance/novo-mercado {:asset-type :stock :ticker "PETR4"})
(s/conform :b3.governance/novo-mercado {:asset-type :bdr :ticker "STNE33"})

;;question: what are the tradeoffs between comp, s/and, or (which I assume I will see later composing specs identifiers?)

;;s/or conditional composition accepts a tag
(s/def ::asset-type-or-class (s/or ::asset-class #{:fixed-income :equities :treasury-bonds}
                                   ::asset-type #{:cdb :lci :lca :preferred-stock :ordinary-stock :tesouro-ipca :tesouro-selic}))

;;the matching tag is returned by conform
(s/conform ::asset-type-or-class :cdb)

;;explain displays several informations on why a conform failed
;;prints to *out*
;(s/explain :string/number "1a")
;;return as string
;(s/explain-str :b3.governance/novo-mercado {:asset-type :bdr :ticker "STNE33"})
;;return as data
;(s/explain-data :b3.governance/novo-mercado {:asset-type :bdr :ticker "STNE33"})
;(s/explain-data ::asset-type-or-class :not-type)

;;entity maps
(s/def :security/type #{:cdb :lci :lca :preferred-stock :ordinary-stock :tesouro-ipca :tesouro-selic})
(s/def :security/class #{:fixed-income :equities :treasury-bonds})
(s/def :security/description string?)
(s/def :security/id int?)
(s/def :security/novo-mercado? :b3.governance/novo-mercado) ;novo mercado spec expects a asset type and ticker
;so its not an attribute level spec
;specs take value as input to the predicate
;and according to s/keys semantics, defining :opt [:security/novo-mercado?] checks if this key exist in the :asset/security map
;and if so, checks if its key value conforms to the b3.governance/novo-mercado spec, which expects a map, but not a :asset/security map

;;how to do conditional specs? like requiring :ticker if :asset-class :equities or match :type with :class
;;whats the idiomatic way to create specs for ticker "ABEV3" for example
(s/def :asset/security (s/keys :req [:security/type :security/class :security/description :security/id]
                               :opt [:security/novo-mercado?]))

(s/explain-data :asset/security #:security{:type        :cdb
                                           :class       :fixed-income
                                           :description "CDB Daycoval CDI 2023"
                                           :id          1})

;;req-un and opt-un looks for unnamespaced keys in the map, but the spec requirements should be defined as namespaced,
;;to match the registered specs

(s/def :asset-2/security (s/keys :req-un [:security/type]
                                 :opt-un [:security/class]
                                 :req [:security/id]))

(s/conform :asset-2/security #:security{:type :cdb, :id 1})
(s/explain :asset-2/security #:security{:type :cdb, :id 1})
(s/conform :asset-2/security {:type :cdb, :security/id 1})
(s/conform :asset-2/security {:type :cdb, :security/id 1, :class :fixed-income})

;;records can conform with unamespaced key specs
(s/def ::asset-class-type (s/keys :req-un [:security/class :security/type]))
(defrecord AssetClassType [class type])

(s/conform ::asset-class-type (->AssetClassType :fixed-income :cdb))
(s/explain-data ::asset-class-type (->AssetClassType :fixed-income nil))

;;s/keys* has support for sequential data structure but using same s/keys semantics
;;this is a special case for supporting sequential kv pairs, like named args
(s/def :funds.meta/tax-account int?)
(s/def :funds.meta/operational-window-opening inst?)
(s/def :funds.meta/operational-window-closing inst?)
(s/def :funds.meta/provider-id uuid?)

(s/def :funds/meta (s/keys* :req [:funds.meta/provider-id :funds.meta/tax-account]
                            :opt [:funds.meta/operational-window-closing :funds.meta/operational-window-opening]))

(s/valid? :funds/meta [:funds.meta/provider-id (UUID/randomUUID) :funds.meta/tax-account 1])
;true
(s/valid? :funds/meta [:funds.meta/tax-account 1 :funds.meta/provider-id (UUID/randomUUID)])
;true
(s/valid? :funds/meta [:funds.meta/provider-id (UUID/randomUUID) 1 :funds.meta/tax-account])
;false
(s/explain-data :funds/meta [:funds.meta/provider-id (UUID/randomUUID) 1 :funds.meta/tax-account])
;;explains :problems :in here is the seq index

;;above is some crazy experimentations to understand more and create my mental model
;;trying to use keys* to conform a seq with ascendent values, but this is not how it works
;;s/keys semantics expects keywords.

(s/def :value/one (partial = 1))
(s/def :value/two (partial = 2))
(s/def :value/three (partial = 3))

(s/def :vector/ascendent (s/keys* :req [:value/one :value/two]))
(s/valid? :vector/ascendent [:value/one 1 :value/two 2])
;true
(s/valid? :vector/ascendent [:value/two 2 :value/one 1])
;true
(s/valid? :vector/ascendent [:value/one 2 :value/two 1])
;false
(s/valid? :vector/ascendent [1 2])
(s/explain-data :vector/ascendent [1 2])

(s/explain-data :vector/ascendent [:value/one 1 :value/two 2])
(s/explain-data :vector/ascendent [:value/one 2 :value/two 1])
;;this is how to conform an ascendent vector using spec
(defn asc? [coll]
  (loop [greater Integer/MIN_VALUE
         [head & tail] coll]
    (if-not head
      true
      (if (> greater head)
        false
        (recur head tail)))))

(s/def :vector/ascendent? asc?)

(doc :vector/ascendent?)
(s/conform :vector/ascendent? [1 2 3])
(s/conform :vector/ascendent? [1 5 3])
(s/explain-data :vector/ascendent? [1 5 4])
;; explain data :in does not show which index didnt conform

;;it is possible to define specs separatedly to create common reusable abstractions
(s/def :asset/class #{:fixed-income :equities})
(s/def :asset/type #{:cdb :stock})
(s/def ::asset (s/keys :req [:asset/class :asset/type]))
(s/def :cdb/maturity-date inst?)
(s/def :cdb/hired-yield int?)
(s/def ::cdb (s/merge ::asset (s/keys :req [:cdb/hired-yield :cdb/maturity-date])))
(doc ::cdb)
(doc ::asset)
(doc :asset/class)

(s/conform ::cdb {:asset/class :fixed-income})
;;explain prints invalid hired yield in two different paths, i didnt understand why
(s/explain-data ::cdb {:asset/class :fixed-income :cdb/hired-yield "1"})
(def cdb {:asset/class       :fixed-income
          :asset/type        :cdb
          :cdb/hired-yield   2
          :cdb/maturity-date (Date.)})

(s/conform ::cdb cdb)

(s/def :stock/ticker string?)
(s/def :stock/type #{:ordinary :preferred})
(s/def ::stock (s/merge ::asset (s/keys :req [:stock/ticker :stock/type])))
(def stock {:asset/class  :equities
            :asset/type   :stock
            :stock/type   :preferred
            :stock/ticker "PETR4"})
(s/explain-data ::stock stock)

;;s/keys spec semantics is tolerant
(s/valid? ::stock (assoc stock :cdb/hired-yield 2))
;;well, it means that the s/keys :opt is not about what is optionally in the map
;;instead it is about to conform the optional keys to their specs

;;multi-spec lets you use a multimethod that dispatches on an attribute value that
;;defines the entity spec based in value returned.
(s/def :asset/class #{:fixed-income :equities})
(s/def :asset/type #{:cdb :stock})
(s/def :cdb/maturity-date inst?)
(s/def :cdb/hired-yield int?)
(s/def :stock/ticker string?)
(s/def :stock/type #{:ordinary :preferred})
(s/def ::base-asset (s/keys :req [:asset/class :asset/type]))

(defmulti asset-type :asset/type)
(defmethod asset-type :cdb [_]
  (s/merge ::base-asset (s/keys :req [:cdb/hired-yield :cdb/maturity-date])))

(defmethod asset-type :stock [_]
  (s/merge ::base-asset (s/keys :req [:stock/ticker :stock/type])))

(s/def ::asset (s/multi-spec asset-type :asset/type))

(s/conform ::asset {:asset/class  :equities
                    :asset/type   :stock
                    :stock/type   :preferred
                    :stock/ticker "PETR4"})

(s/explain-data ::asset {:asset/class  :equities
                    :asset/type   :cdb
                    :stock/type   :preferred
                    :stock/ticker "PETR4"})

(s/conform ::asset {:asset/class  :fixed-income
                    :asset/type   :cdb
                    :cdb/maturity-date (Date.)
                    :cdb/hired-yield 2})

;;the difference between just mergin is that with this way its possible to make
;;sure that entities with certain attribute values have certain keys
;;in the example about mergin, an ::stock could have :fixed income as asset type
;;this can look like a form of validation, but actually its a form of making open
;;the ::asset spec open
;;altough better than just mergiing, this is a poor form of validation, because although i could ensure certain fields
;;according to the asset/type, i couldnt with asset class
;;for example, the above conforms, although :cdb is not :equities
(s/conform ::asset {:asset/class  :equities
                    :asset/type   :cdb
                    :cdb/maturity-date (Date.)
                    :cdb/hired-yield 2})

;;collections
;;coll-of for arbitrary size of homogeneous pred
(s/conform (s/coll-of string?) ["a" "b"])
;;coll of can receive several arg options
;; :kind, :count, :min-count, :distinct, :into
;kind is a pred to the whole vector, differently than the coll-of first arg, which is a predicate
;that will be applied to each value
(s/conform (s/coll-of string? :kind vector?) '("a" "b"))
;invalid

;;so this is another way to spec like :vector/ascendent? as ive done before
(s/conform (s/coll-of int? :kind asc?) [1 3 2])
;invalid
;;but it has more power because can use with other coll-of options like
(s/conform (s/coll-of int? :kind asc? :min-count 4) [1 2 3])
;invalid

;;tuple for fixed size of possibly heterogeneous pred
(s/conform (s/tuple string? int?) ["a" 1])
(s/conform (s/tuple string? int?) ["a" 1 1])
;invalid
(s/conform (s/tuple string? int?) [1 1])
;invalid

;;s/map-of for conforming of maps with homogeneous key/val types
(s/conform (s/map-of string? int?) {"1" 1 "2" 2 "3" 3})
(s/explain-data (s/map-of string? int?) {"1" 1 "2" 2 4 3})
;;count related args of coll-of can be used with map-of as well
(s/conform (s/map-of string? int? :count 2) {"1" 1 "2" 2 "3" 3})
(s/conform (s/map-of string? int? :count 3) {"1" 1 "2" 2 "3" 3})

(s/conform (s/coll-of :asset/class) [:fixed-income :equities :equities])
(s/explain-data (s/coll-of ::asset) [{:asset/class  :equities
                                 :asset/type   :cdb
                                 :cdb/maturity-date (Date.)
                                 :cdb/hired-yield 2}
                                {:asset/class  :fixed-income
                                 :asset/type   :cdb
                                 :cdb/maturity-date (Date.)
                                 :cdb/hired-yield 2}
                                ;;invalid ::asset
                                {:asset/class  :equities
                                 :asset/type   :cdb
                                 :stock/type   :preferred
                                 :stock/ticker "PETR4"}])

;;explain here :via is ::asset, in is [2] (index)m and :path is :cdb (aparently the multispec dispatch value)

;;sequences
