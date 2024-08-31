(ns schema.core-test
  "Tests for schema.

   Uses helpers defined in schema.test-macros (for cljs sake):
    - (valid! s x) asserts that (s/check s x) returns nil
    - (invalid! s x) asserts that (s/check s x) returns a validation failure
      - The optional last argument also checks the printed Clojure representation of the error.
    - (invalid-call! s x) asserts that calling the function throws an error."
  (:refer-clojure :exclude [parse-long])
  #?(:clj (:use clojure.test [schema.test-macros :only [valid! invalid! invalid-call! is-assert!]]))
  #?(:cljs (:use-macros
             [cljs.test :only [is deftest testing are]]
             [schema.test-macros :only [valid! invalid! invalid-call! is-assert!]]))
  #?(:cljs (:require-macros [clojure.template :refer [do-template]]
                            [schema.macros :as macros]))
  (:require
   [clojure.string :as str]
   [#?(:clj clojure.pprint
       :cljs cljs.pprint) :as pprint]
   #?(:clj [clojure.template :refer [do-template]])
   clojure.data
   [schema.utils :as utils]
   [schema.core :as s]
   [schema.other-namespace :as other-namespace]
   [schema.spec.core :as spec]
   [schema.spec.collection :as collection]
   #?(:clj [schema.macros :as macros])
   #?(:cljs cljs.test)))

#?(:cljs
(do
  (def Exception js/Error)
  (def AssertionError js/Error)
  (def Throwable js/Error)))

(deftest if-cljs-test
  (is (= #?(:cljs true :clj false) (macros/if-cljs true false))))

(deftest try-catchall-test
  (let [a (atom 0)]
    (is (= 2 (macros/try-catchall (reset! a 1) (swap! a inc) (catch e (swap! a - 10)))))
    (is (= 2 @a)))
  (let [a (atom 0)]
    (is (= -9 (macros/try-catchall (reset! a 1) (swap! a #(throw (macros/error! (str %)))) (catch e (swap! a - 10)))))
    (is (= -9 @a))))

(deftest validate-return-test
  (is (= 1 (s/validate s/Int 1))))

(defn foo-bar [])

(deftest fn-name-test
  (is (= "odd?" (utils/fn-name odd?)))
  #?(:bb nil
     :default (is (= #?(:clj "schema.core-test/foo-bar" :cljs "foo-bar")
                     (utils/fn-name foo-bar))))
  #?(:bb nil
     :clj (is (= "schema.core-test$fn" (subs (utils/fn-name (fn foo [x] (+ x x))) 0 19))))
  #?(:cljs (is (= "foo" (utils/fn-name (fn foo [x] (+ x x))))))
  #?(:cljs (is (= "function" (utils/fn-name (fn [x] (+ x x)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Platform-specific leaf Schemas

#?(:clj
(do
  (deftest class-test
    (valid! String "a")
    (invalid! String nil "(not (instance? java.lang.String nil))")
    (invalid! String :a "(not (instance? java.lang.String :a))"))

  (deftest primitive-test
    (valid! double 1.0)
    (invalid! double (float 1.0) "(not (instance? java.lang.Double 1.0))")
    (valid! float (float 1.0))
    (invalid! float 1.0)
    (valid! long 1)
    (invalid! long (byte 1))
    (valid! boolean true)
    (invalid! boolean 1)
    (valid! longs (long-array 10))
    (invalid! longs (int-array 10))
    (doseq [f [byte char short int]]
      (valid! f (f 1))
      (invalid! f 1))
    (is (= 'double (s/explain double))))

  (deftest array-test
    (valid! (Class/forName"[Ljava.lang.String;") (into-array String ["a"]))
    (invalid! (Class/forName #?(:bb "[Ljava.lang.Double;" :default "[Ljava.lang.Long;")) (into-array String ["a"]))
    (valid! (Class/forName "[Ljava.lang.Double;") (into-array Double [1.0]))
    (valid! (Class/forName "[D") (double-array [1.0]))
    (invalid! (Class/forName "[D") (into-array Double [1.0]))
    (valid! doubles (double-array [1.0]))
    (is (= 'doubles (s/explain doubles))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cross-platform Schema leaves

(deftest any-test
  (valid! s/Any 10)
  (valid! s/Any nil)
  (valid! s/Any :whatever)
  (is (= 'Any (s/explain s/Any))))

(deftest eq-test
  (let [schema (s/eq 10)]
    (valid! schema 10)
    (invalid! schema 9 "(not (= 10 9))")
    (is (= '(eq 10) (s/explain schema)))))

(deftest isa-test
  (let [h (make-hierarchy)
        h (derive h ::square ::shape)
        schema-with-h (s/isa h ::shape)
        schema-no-h (s/isa ::number)]
    (derive ::integer ::number)
    (valid! schema-with-h ::square)
    (valid! schema-no-h ::integer)
    (invalid! schema-with-h ::form)
    (invalid! schema-no-h ::form)
    #?(:clj
       (valid! (s/isa java.lang.Number) java.lang.Long)
       :cljs
       (valid! (s/isa js/Number) js/Number))
    (is (= '(isa? ::shape) (s/explain schema-with-h)))
    (is (= '(isa? ::number) (s/explain schema-no-h)))))

(deftest enum-test
  (let [schema (s/enum :a :b 1)]
    (valid! schema :a)
    (valid! schema 1)
    (invalid! schema :c)
    (invalid! (s/enum :a) 2 "(not (#{:a} 2))")
    (is (= '(enum :a :b 1) (s/explain schema))))
  (is (= (cons 'enum (range 1000)) (s/explain (apply s/enum (range 1000)))))
  (testing "prints as if (distinct vs), which preserves original order"
    (is (= '(enum 1 2 3 4) (s/explain (s/enum 1 2 1 3 1 4)))))
  (testing "equality still works if implementation details are exploited"
    (is (= (update (s/enum 1 2 3) :vs conj 4)
           (update (s/enum 1 2 3 4 5) :vs disj 5))))
  (testing "still prints correctly (albeit unordered) if implementation details are exploited"
    (dotimes [_ 100]
      (let [[a b c] (repeatedly #(rand-nth
                                   [(gensym)
                                    (str (gensym))
                                    (keyword (gensym))]))
            _ (assert (distinct? a b c))
            e (s/enum a b)
            _ (testing "prints in order"
                (is (= (list 'enum a b) (s/explain e))))
            e (update e :vs conj c)
            _ (testing "adding an extra entry using implementation details just prints using the set's order"
                (is (= (cons 'enum (:vs e)) (s/explain e))))
            e (update e :vs disj c)
            _ (testing "resetting :vs preserves the original printing order"
                (is (= (list 'enum a b) (s/explain (update e :vs disj c)))))]))))

(deftest pred-test
  (let [schema (s/pred odd? 'odd?)]
    (valid! schema 1)
    (invalid! schema 2 "(not (odd? 2))")
    (invalid! schema :foo "(throws? (odd? :foo))")
    (is (= '(pred odd?) (s/explain schema)))
    (invalid! (s/pred odd?) 2 "(not (odd? 2))")))

(defprotocol ATestMarkerProtocol)

(s/defn ^:always-validate a-test-marker-protocol-fn
  "Compile the schema before extending, make sure it works as expected"
  [x :- (s/protocol ATestMarkerProtocol)]
  x)

(defrecord DirectTestMarkerProtocolSatisfier [] ATestMarkerProtocol)
(defrecord IndirectTestMarkerProtocolSatisfier []) (extend-type IndirectTestMarkerProtocolSatisfier ATestMarkerProtocol)
(defrecord NonTestMarkerProtocolSatisfier [])

(deftest marker-protocol-test
 (let [schema (s/protocol ATestMarkerProtocol)]
   (valid! schema (DirectTestMarkerProtocolSatisfier.))
   (valid! schema (IndirectTestMarkerProtocolSatisfier.))
   (invalid! schema (NonTestMarkerProtocolSatisfier.))
   (invalid! schema nil)
   (invalid! schema 117 "(not (satisfies? ATestMarkerProtocol 117))")
   (is (a-test-marker-protocol-fn (DirectTestMarkerProtocolSatisfier.)))
   (is (a-test-marker-protocol-fn (IndirectTestMarkerProtocolSatisfier.)))
   (invalid-call! a-test-marker-protocol-fn (NonTestMarkerProtocolSatisfier.))
   (is (= '(protocol ATestMarkerProtocol) (s/explain schema)))))

(defprotocol ATestProtocol
  (not-marker-protocol [this]))

(s/defn ^:always-validate a-test-non-marker-protocol-fn
  "Compile the schema before extending, make sure it works as expected"
  [x :- (s/protocol ATestProtocol)]
  x)

(defrecord DirectTestProtocolSatisfier [] ATestProtocol (not-marker-protocol [this]))
(defrecord IndirectTestProtocolSatisfier []) (extend-type IndirectTestProtocolSatisfier ATestProtocol (not-marker-protocol [this]))
(defrecord NonTestProtocolSatisfier [])

(deftest non-marker-protocol-test
  (let [schema (s/protocol ATestProtocol)]
    (valid! schema (DirectTestProtocolSatisfier.))
    (valid! schema (IndirectTestProtocolSatisfier.))
    (invalid! schema (NonTestProtocolSatisfier.))
    (invalid! schema nil)
    (invalid! schema 117 "(not (satisfies? ATestProtocol 117))")
    (is (a-test-non-marker-protocol-fn (DirectTestProtocolSatisfier.)))
    (is (a-test-non-marker-protocol-fn (IndirectTestProtocolSatisfier.)))
    (invalid-call! a-test-non-marker-protocol-fn (NonTestProtocolSatisfier.))
    (is (= '(protocol ATestProtocol) (s/explain schema)))))

(deftest regex-test
  (valid! #"lex" "Alex B")
  (valid! #"lex" "lex")
  (invalid! #"lex" nil "(not (string? nil))")
  (invalid! #"lex" "Ale" "(not (re-find #\"lex\" \"Ale\"))")
  (is (= (symbol "#\"lex\"") (s/explain #"lex"))))

(deftest leaf-bool-test
  (valid! s/Bool true)
  (invalid! s/Bool nil "(not (instance? java.lang.Boolean nil))")
  (is (= 'Bool (s/explain s/Bool))))

(deftest leaf-string-test
  (valid! s/Str "asdf")
  (invalid! s/Str nil "(not (instance? java.lang.String nil))")
  (invalid! s/Str :a "(not (instance? java.lang.String :a))")
  (is (= 'Str (s/explain s/Str))))

(deftest leaf-number-test
  (valid! s/Num 1)
  (valid! s/Num 1.2)
  (valid! s/Num (/ 1 2))
  (invalid! s/Num nil "(not (instance? java.lang.Number nil))")
  (invalid! s/Num "1" "(not (instance? java.lang.Number \"1\"))")
  (is (= 'Num (s/explain s/Num))))

(deftest leaf-int-test
  (valid! s/Int 1)
  (invalid! s/Int 1.2 "(not (integer? 1.2))")
  #?(:clj (invalid! s/Int 1.0 "(not (integer? 1.0))"))
  (invalid! s/Int nil "(not (integer? nil))")
  (is (= 'Int (s/explain s/Int))))

(deftest leaf-keyword-test
  (valid! s/Keyword :a)
  (valid! s/Keyword ::a)
  (invalid! s/Keyword nil "(not (keyword? nil))")
  (invalid! s/Keyword ":a" "(not (keyword? \":a\"))")
  (is (= 'Keyword (s/explain s/Keyword))))

(deftest leaf-symbol-test
  (valid! s/Symbol 'foo)
  (invalid! s/Symbol nil "(not (symbol? nil))")
  (invalid! s/Symbol "'a" "(not (symbol? \"'a\"))")
  (is (= 'Symbol (s/explain s/Symbol))))

(deftest leaf-regex-test
  (valid! s/Regex #".*")
  (invalid! s/Regex ".*")
  (is (= 'Regex (s/explain s/Regex))))

(deftest leaf-inst-test
  (valid! s/Inst #inst "2013-01-01T01:15:01.840-00:00")
  (invalid! s/Inst "2013-01-01T01:15:01.840-00:00")
  (is (= 'Inst (s/explain s/Inst))))

(deftest leaf-uuid-test
  (valid! s/Uuid #uuid "0e98ce5b-9aca-4bf7-b5fd-d90576c80fdf")
  (invalid! s/Uuid "0e98ce5b-9aca-4bf7-b5fd-d90576c80fdf")
  (is (= 'Uuid (s/explain s/Uuid))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple composite Schemas


(deftest maybe-test
  (let [schema (s/maybe s/Int)]
    (valid! schema nil)
    (valid! schema 1)
    (invalid! schema 1.1 "(not (integer? 1.1))")
    (is (= '(maybe Int) (s/explain schema)))))

(deftest named-test
  (let [schema (s/named s/Int :score)]
    (valid! schema 12)
    (invalid! schema :a "(named (not (integer? :a)) :score)")
    (is (= '(named Int :score) (s/explain schema)))))

(deftest either-test
  (let [schema (s/either
                {:num s/Int}
                {:str s/Str})]
    (valid! schema {:num 1})
    (valid! schema {:str "hello"})
    (invalid! schema {:num "bad!"})
    (invalid! schema {:str 1})
    (is (= '(either {:a Int} Int) (s/explain (s/either {:a s/Int} s/Int))))
    (is (s/explain schema))))

(deftest both-test
  (let [schema (s/both
                (s/pred (fn equal-keys? [m] (every? (fn [[k v]] (= k v)) m)) 'equal-keys?)
                {s/Keyword s/Keyword})]
    (valid! schema {})
    (valid! schema {:foo :foo :bar :bar})
    (invalid! schema {"foo" "foo"})
    (invalid! schema {:foo :bar} "(not (equal-keys? {:foo :bar}))")
    (invalid! schema {:foo 1} "(not (equal-keys? {:foo 1}))")
    (is (= '(both (pred vector?) [Int])
           (s/explain (s/both (s/pred vector? 'vector?) [s/Int]))))))

(deftest conditional-test
  (let [schema (s/conditional #(= (:type %) :foo) {:type (s/eq :foo) :baz s/Num}
                              #(= (:type %) :bar) {:type (s/eq :bar) :baz s/Str})]
    (valid! schema {:type :foo :baz 10})
    (valid! schema {:type :bar :baz "10"})
    (invalid! schema {:type :foo :baz "10"})
    (invalid! schema {:type :bar :baz 10} "{:baz (not (instance? java.lang.String 10))}")
    (invalid! schema {:type :zzz :baz 10}
              "(not (some-matching-condition? a-clojure.lang.PersistentArrayMap))")
    (is (s/explain schema)))
  (testing "as simple constraint"
    (let [schema (s/conditional
                  (fn [m] (every? (fn [[k v]] (= k v)) m))
                  {s/Keyword s/Keyword}
                  'equal-keys?)]
      (valid! schema {})
      (valid! schema {:foo :foo :bar :bar})
      (invalid! schema {"foo" "foo"})
      (invalid! schema {:foo :bar} "(not (equal-keys? {:foo :bar}))")
      (invalid! schema {:foo 1} "(not (equal-keys? {:foo 1}))")
      (invalid! (s/conditional odd? s/Int) 2 "(not (odd? 2))")
      (invalid! (s/conditional odd? s/Int) "1" "(throws? (odd? \"1\"))")
      (is (= '(conditional odd? Int)
             (s/explain (s/conditional odd? s/Int))))
      (is (= '(conditional odd? Int weird?)
             (s/explain (s/conditional odd? s/Int 'weird?))))))
  (testing "nonfatal exceptions are caught"
    (let [schema (s/conditional (fn [x] (throw (ex-info "non-fatal error" {})))
                                s/Int)]
      (invalid! schema 99)))
  #?(:clj
  (testing "fatal exceptions are not caught"
    (let [schema (s/conditional (fn [x] (throw (InterruptedException.)))
                                s/Int)]
      (is (thrown? InterruptedException (s/validate schema 42)))))))

(deftest cond-pre-test
  (let [s (s/cond-pre
           s/Int
           (s/maybe s/Str)
           (s/cond-pre s/Keyword {:x s/Int})
           (s/both [s/Num] (s/pred (fn [xs] (even? (count xs))) 'even-len?))
           [s/Str])]
    (valid! s 1)
    (valid! s "hello")
    (valid! s nil)
    (valid! s :hello)
    (valid! s {:x 3})
    (valid! s [1 2])
    (valid! s ["hello"])
    (invalid! s 3.14)
    (invalid! s [1 2 3])
    (invalid! s {:x 3.14})
    (invalid! s [1 2 3])))

(deftest constrained-test
  (let [s (s/constrained s/Int odd?)]
    (is (= '(constrained Int odd?) (s/explain s)))
    (valid! s 1)
    (valid! s 5)
    (invalid! s 2 "(not (odd? 2))")
    (invalid! s "2" "(not (integer? \"2\"))")
    (invalid! (s/constrained s/Str odd?) "2" "(throws? (odd? \"2\"))" ))
  (let [s (s/constrained {:a s/Int} #(odd? (:a %)))]
    (valid! s {:a 1})
    (invalid! s {:b 1})
    (invalid! s {:a 2})))

(deftest if-test
  (let [schema (s/if #(= (:type %) :foo)
                 {:type (s/eq :foo) :baz s/Num}
                 {:type (s/eq :bar) :baz s/Str})]
    (valid! schema {:type :foo :baz 10})
    (valid! schema {:type :bar :baz "10"})
    (invalid! schema {:type :foo :baz "10"})
    (invalid! schema {:type :bar :baz 10})
    (invalid! schema {:type :zzz :baz 10})))


(def NestedVecs
  [(s/one s/Num "Node ID")
   (s/recursive #'NestedVecs)])

(def NestedMaps
  {:node-id s/Num
   (s/optional-key :children) [(s/recursive #'NestedMaps)]})

(declare TestBlackNode)
(def TestRedNode {(s/optional-key :red) (s/recursive #'TestBlackNode)})
(def TestBlackNode {:black TestRedNode})

(deftest recursive-test
  (valid! NestedVecs [1 [2 [3 [4]]]])
  (invalid! NestedVecs [1 [2 ["invalid-id" [4]]]])
  (invalid! NestedVecs [1 [2 [3 "invalid-content"]]])

  (valid! NestedMaps
          {:node-id 1
           :children [{:node-id 1
                       :children [{:node-id 4}]}
                      {:node-id 3}]})
  (invalid! NestedMaps
            {:node-id 1
             :children [{:invalid-node-id 1
                         :children [{:node-id 4}]}
                        {:node-id 3}]})
  (invalid! NestedMaps
            {:node-id 1
             :children [{:node-id 1
                         :children [{:node-id 4}]}
                        {:node-id "invalid-id"}]})

  (valid! TestBlackNode {:black {}})
  (valid! TestBlackNode {:black {:red {:black {}}}})
  (invalid! TestBlackNode {:black {:black {}}})
  (invalid! TestBlackNode {:black {:red {}}})

  (let [rec (atom nil)
        schema {(s/optional-key :x) (s/recursive rec)}]
    (reset! rec schema)
    (valid! schema {})
    (valid! schema {:x {:x {:x {}}}})
    (invalid! schema {:x {:x {:y {}}}})
    (let [explanation (first (s/explain schema))]
      (is (= '(optional-key :x) (key explanation)))
      #?(:clj (is (= 'recursive (first (val explanation)))))
      #?(:clj (is (re-matches #"clojure.lang.Atom.*" (second (val explanation)))))
      #?(:cljs (is (= '(recursive ...) (val explanation))))))

  (is (= '{:black {(optional-key :red) (recursive (var schema.core-test/TestBlackNode))}}
         (s/explain TestBlackNode)))
  (is-assert! (s/recursive nil) #"Not an IDeref: null"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Atom schemas

(deftest atom-test
  (let [s (s/atom s/Str)]
    (is (not (s/check s (atom "asdf")))) ;; don't expect identity after walking
    (invalid! s (delay "asdf") "(not (atom? a-clojure.lang.Delay))")
    (invalid! s (atom 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Map Schemas

(deftest uniform-map-test
  (let [schema {s/Keyword s/Int}]
    (valid! schema {})
    (valid! schema {:a 1 :b 2})
    (invalid! schema {'a 1 :b 2} "{(not (keyword? a)) invalid-key}")
    (invalid! schema {:a :b} "{:a (not (integer? :b))}")
    (is (= '{Keyword Int} (s/explain {s/Keyword s/Int})))))

(deftest simple-specific-key-map-test
  (let [schema-args [:foo s/Keyword :bar s/Int]]
    (doseq [[t schema] {"hash-map" (apply hash-map schema-args)
                        "array-map" (apply array-map schema-args)}]
      (testing t
        (valid! schema {:foo :a :bar 2})
        (invalid! schema [[:foo :a] [:bar 2]] "(not (map? a-clojure.lang.PersistentVector))")
        (invalid! schema {:foo :a} "{:bar missing-required-key}")
        (invalid! schema {:foo :a :bar 2 :baz 1} "{:baz disallowed-key}")
        (invalid! schema {:foo :a :bar 1.5} "{:bar (not (integer? 1.5))}")
        (is (= '{:foo Keyword, :bar Int} (s/explain schema)))))))

(deftest fancier-map-schema-test
  (let [schema {:foo s/Int
                s/Str s/Num}]
    (valid! schema {:foo 1})
    (valid! schema {:foo 1 "bar" 2.0})
    (valid! schema {:foo 1 "bar" 2.0 "baz" 10.0})
    (invalid! schema {:foo 1 :bar 2.0})
    (invalid! schema {:foo 1 :bar 2.0})
    (invalid! schema {:foo 1 :bar 2.0 "baz" 2.0})
    (invalid! schema {:foo 1 "bar" "a"})))

(deftest another-fancy-map-schema-test
  (let [schema {:foo (s/maybe s/Int)
                (s/optional-key :bar) s/Num
                :baz {:b1 (s/pred odd?)}
                s/Keyword s/Any}]
    (valid! schema {:foo 1 :bar 1.0 :baz {:b1 3}})
    (valid! schema {:foo 1 :baz {:b1 3}})
    (valid! schema {:foo nil :baz {:b1 3}})
    (valid! schema {:foo nil :baz {:b1 3} :whatever "whatever"})
    (invalid! schema {:foo 1 :bar 1.0 :baz [[:b1 3]]})
    (invalid! schema {:foo 1 :bar 1.0 :baz {:b2 3}})
    (invalid! schema {:foo 1 :bar 1.0 :baz {:b1 4}})
    (invalid! schema {:bar 1.0 :baz {:b1 3}})
    (invalid! schema {:foo 1 :bar nil :baz {:b1 3}})
    (invalid! schema {:foo 1 :bar "z" :baz {:b1 3}})))

(deftest throw-on-multiple-key-variants-test
  (is (thrown? Exception (s/checker {:foo s/Str (s/optional-key :foo) s/Str})))
  (is (thrown? Exception (s/checker {(s/required-key "A") s/Str (s/optional-key "A") s/Str}))))

(defprotocol SomeProtocol
  (stuff [this]))

(defrecord SomeRecord [x y z]
  SomeProtocol
  (stuff [_] x))

(deftest keys-and-protocol-test
  (let [field-subset {:x s/Keyword :y s/Num s/Keyword s/Any}
        schema (s/conditional #(satisfies? SomeProtocol %) field-subset)]
    (is (not (s/check schema (->SomeRecord :foo 42 "extra")))) ;; comes out as map
    (invalid! schema {:x :foo :y 42})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handle Struct

#?(:bb nil
:clj
(do (defstruct ts1 :num :str :map :vec)
    (defstruct ts2 :num :str)

    (deftest struct-tests
      (let [schema {(s/required-key :num) s/Num
                    (s/required-key :str) s/Str
                    (s/required-key :map) {s/Keyword s/Str}
                    (s/required-key :vec) [s/Num]
                    (s/optional-key :opt) s/Num}]
        (valid! schema (struct ts1 1 "str" {:key "str"} [1]))
        (valid! schema {:num 1 :str "str" :map {:key "str"} :vec [1]})
        (valid! schema (struct ts1 1 "str" (struct ts1 "a" "b" "c" "d") [1]))
        (valid! schema (assoc (struct ts1 1 "str" {:key "str"} [1])
                         :opt 1))
        (valid! schema (assoc (struct ts2 1 "str")
                         :map {}
                         :vec []))

        (invalid! schema (struct ts1 "str" "str" {:key "str"} [1]))
        (invalid! schema (struct ts1 1 1 {:key "str"} [1]))
        (invalid! schema (struct ts1 1 "str" {"str" "str"} [1]))
        (invalid! schema (struct ts1 1 "str" {:key 1} [1]))
        (invalid! schema (struct ts1 1 "str" {:key "str"} 1))
        (invalid! schema (struct ts1 1 "str" {:key "str"} ["str"]))
        (invalid! schema (assoc (struct ts1 1 "str" {:key "str"} [1])
                           :opt "str"))
        (invalid! schema (assoc (struct ts1 1 "str" {:key "str"} [1])
                           :extra-key 1))
        (invalid! schema (struct ts2 1 "str"))
        (invalid! schema (assoc (struct ts2 1 "str")
                           :map {:key 1}
                           :vec []))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set Schemas

(deftest simple-set-test
  (testing "set schemas must have exactly one entry"
    (is (thrown? Exception (s/check #{s/Int s/Num} #{})))
    (is (thrown? Exception (s/check #{} #{}))))

  (testing "basic set identification"
    (let [schema #{s/Keyword}]
      (valid! schema #{:a :b :c})
      (invalid! schema [:a :b :c] "(not (set? [:a :b :c]))")
      (invalid! schema {:a :a :b :b})
      (is (= '#{Keyword} (s/explain schema)))))

  (testing "enforces matching with single simple entry"
    (let [schema #{s/Int}]
      (valid! schema #{})
      (valid! schema #{1 2 3})
      (invalid! schema #{1 :a} "#{(not (integer? :a))}")
      (invalid! schema #{:a "c" {}})))

  (testing "more complex element schema"
    (let [schema #{[s/Int]}]
      (valid! schema #{})
      (valid! schema #{[2 4] [3 6]})
      (invalid! schema #{2})
      (invalid! schema #{[[2 3]]}))))

(deftest mixed-set-test
  (let [schema #{(s/either [s/Int] #{s/Int})}]
    (valid! schema #{})
    (valid! schema #{[3 4] [56 1] [-11 3]})
    (valid! schema #{#{3 4} #{56 1} #{-11 3}})
    (valid! schema #{[3 4] #{56 1} #{-11 3}})
    (invalid! schema #{#{[3 4]}})
    (invalid! schema #{[[3 4]]})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Queue Schemas

(deftest queue-test
  (testing "queues of simple values"
    (let [schema (s/queue s/Int)]
      (valid! schema (s/as-queue []))
      (valid! schema (s/as-queue [1]))
      (valid! schema (s/as-queue [1 2 3 4]))
      (invalid! schema [1 2 3])
      (invalid! schema (s/as-queue [1 :a 3])))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sequence Schemas

(deftest simple-repeated-seq-test
  (let [schema [s/Int]]
    (valid! schema [])
    (valid! schema [1 2 3])
    (invalid! schema {})
    #?(:clj (invalid! schema [1 2 1.0]))
    (invalid! schema [1 2 1.1])))

(deftest simple-one-seq-test
  (let [schema [(s/one s/Int "int") (s/one s/Str "str")]]
    (valid! schema [1 "a"])
    (invalid! schema [1])
    (invalid! schema [1 1.0 2])
    (invalid! schema [1 1])
    (invalid! schema [1.0 1.0])))

(deftest optional-seq-test
  (let [schema [(s/one s/Int "int")
                (s/optional s/Str "str")
                (s/optional s/Int "int2")]]
    (valid! schema [1])
    (valid! schema [1 "a"])
    (valid! schema [1 "a" 2])
    (invalid! schema [])
    (invalid! schema [1 "a" 2 3])
    (invalid! schema [1 1])))

(deftest combo-seq-test
  (let [schema [(s/one (s/maybe s/Int) :maybe-long)
                (s/optional s/Keyword :key)
                s/Int]]
    (valid! schema [1])
    (valid! schema [1 :a])
    (valid! schema [1 :a 1 2 3])
    (valid! schema [nil :b 1 2 3])
    (invalid! schema {} "(not (sequential? {}))")
    (invalid! schema "asdf" "(not (sequential? \"asdf\"))")
    (invalid! schema [nil 1 1 2 3] "[nil (named (not (keyword? 1)) :key) nil nil nil]")
    (invalid! schema [1.4 :A 2 3] "[(named (not (integer? 1.4)) :maybe-long) nil nil nil]")
    (invalid! schema [] "[(not (present? :maybe-long))]")
    (is (= '[(one (maybe Int) :maybe-long) (optional Keyword :key) Int] (s/explain schema)))))

(deftest pair-test
  (let [schema (s/pair s/Str "user-name" s/Int "count")]
    (valid! schema ["user1" 42])
    (invalid! schema ["user2" 42.1])
    (invalid! schema [42 "user1"])
    (invalid! schema ["user1" 42 42])
    (valid! schema ["user2" 41]) ))

#?(:clj
(deftest java-list-test
  (let [schema [s/Str]]
    (valid! schema (java.util.ArrayList. ^java.util.Collection (identity ["hi" "bye"])))
    (invalid! schema (java.util.ArrayList. ^java.util.Collection (identity [1 2])))
    #?(:bb nil :default (valid! schema (java.util.LinkedList. ^java.util.Collection (identity ["hi" "bye"]))))
    #?(:bb nil :default (invalid! schema (java.util.LinkedList. ^java.util.Collection (identity [1 2]))))
    (valid! schema java.util.Collections/EMPTY_LIST)
    (invalid! schema java.util.Collections/EMPTY_MAP)
    (invalid! schema #{"hi" "bye"}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Record Schemas

(defrecord Foo [x y])

(deftest record-test
  (let [schema (s/record Foo {:x s/Any (s/optional-key :y) s/Int})]
    (valid! schema (Foo. :foo 1))
    (invalid! schema {:x :foo :y 1})
    (invalid! schema (assoc (Foo. :foo 1) :bar 2))
    #?(:clj (is (= '(record schema.core_test.Foo {:x Any, (optional-key :y) Int})
                   (s/explain schema))))))

(deftest record-with-extra-keys-test
  (let [schema (s/record Foo {:x s/Any
                              :y s/Int
                              s/Keyword s/Any})]
    (valid! schema (Foo. :foo 1))
    (valid! schema (assoc (Foo. :foo 1) :bar 2))
    (invalid! schema {:x :foo :y 1})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function Schemas

(deftest single-arity-fn-schema-test
  (let [schema (s/=> s/Keyword s/Int s/Int)]
    (valid! schema (fn [x y] (keyword (str (+ x y)))))
    (valid! schema (fn [])) ;; we don't actually validate what the function does
    (valid! schema {})
    (is (= '(=> Keyword Int Int) (s/explain schema)))))

(deftest single-arity-and-more-fn-schema-test
  (let [schema (s/=> s/Keyword s/Int s/Int & [s/Keyword])]
    (valid! schema (fn [])) ;; we don't actually validate what the function does
    (valid! schema {})
    (is (= '(=> Keyword Int Int & [Keyword]) (s/explain schema)))))

(deftest multi-arity-fn-schema-test
  (let [schema (s/=>* s/Keyword [s/Int] [s/Int & [s/Keyword]])]
    (valid! schema (fn [])) ;; we don't actually validate what the function does
    (valid! schema {})
    (is (= '(=>* Keyword [Int] [Int & [Keyword]]) (s/explain schema)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schematized defrecord

#?(:clj
(defmacro test-normalized-meta [symbol ex-schema desired-meta]
  (let [normalized (macros/normalized-metadata &env symbol ex-schema)]
    `(do (is (= '~symbol '~normalized))
         (is (= ~(select-keys desired-meta [:schema :tag])
                ~(select-keys (meta normalized) [:schema :tag])))))))

#?(:clj
(do
  (def ASchema [long])

  (deftest normalized-metadata-test
    (testing "empty" (test-normalized-meta 'foo nil {:schema s/Any}))
    (testing "primitive" (test-normalized-meta ^long foo nil {:tag long :schema long}))
    (testing "class" (test-normalized-meta ^String foo nil {:tag String :schema String}))
    (testing "non-tag" (test-normalized-meta ^ASchema foo nil {:schema ASchema}))
    (testing "explicit" (test-normalized-meta ^Object foo String {:tag Object :schema String}))
    (testing "xplatform" (test-normalized-meta ^s/Str foo nil {:tag String :schema String})))

  (defmacro test-meta-extraction [meta-form arrow-form]
    (let [meta-ized (macros/process-arrow-schematized-args {} arrow-form)]
      `(do (is (= '~meta-form '~meta-ized))
           (is (= ~(mapv #(select-keys (meta (macros/normalized-metadata {} % nil)) [:schema :tag]) meta-form)
                  ~(mapv #(select-keys (meta %) [:schema :tag]) meta-ized))))))

  (deftest extract-arrow-schematized-args-test
    (testing "empty" (test-meta-extraction [] []))
    (testing "no-tag" (test-meta-extraction [x] [x]))
    (testing "old-tags" (test-meta-extraction [^String x] [^String x]))
    (testing "new-vs-old-tag" (test-meta-extraction [^String x] [x :- String]))
    (testing "xplatform-new-vs-xplatform-old-tag" (test-meta-extraction [^s/Str x] [x :- s/Str]))
    (testing "multi vars" (test-meta-extraction [x ^String y z] [x y :- String z])))))

(defprotocol PProtocol
  (do-something [this]))

;; exercies some different arities

(s/defrecord Bar
    [^s/Int foo ^s/Str bar]
  {(s/optional-key :baz) s/Keyword})

(s/defrecord Bar2
    [^s/Int foo ^s/Str bar]
  {(s/optional-key :baz) s/Keyword}
  PProtocol
  (do-something [this] 2))

(s/defrecord Bar3
    [^s/Int foo ^s/Str bar]
  PProtocol
  (do-something [this] 3))

(s/defrecord Bar4
    [foo :- [s/Int]
     bar :- (s/maybe {s/Str s/Str})]
  PProtocol
  (do-something [this] 4))

(deftest defrecord-schema-test
  (is (= (utils/class-schema Bar)
         (s/record Bar {:foo s/Int
                        :bar s/Str
                        (s/optional-key :baz) s/Keyword})))
  (is (identity (Bar. 1 :foo)))
  (is (= #{:foo :bar} (set (keys (map->Bar {:foo 1})))))
  ;; (is (thrown? Exception (map->Bar {}))) ;; check for primitive long
  (valid! Bar (Bar. 1 "test"))
  (invalid! Bar (Bar. 1 :foo))
  (valid! Bar (assoc (Bar. 1 "test") :baz :foo))
  (invalid! Bar (assoc (Bar. 1 "test") :baaaz :foo))
  (invalid! Bar (assoc (Bar. 1 "test") :baz "foo"))

  (valid! Bar2 (assoc (Bar2. 1 "test") :baz :foo))
  (invalid! Bar2 (assoc (Bar2. 1 "test") :baaaaz :foo))
  (is (= 2 (do-something (Bar2. 1 "test"))))

  (valid! Bar3 (Bar3. 1 "test"))
  (invalid! Bar3 (assoc (Bar3. 1 "test") :foo :bar))
  (is (= 3 (do-something (Bar3. 1 "test"))))

  (valid! Bar4 (Bar4. [1] {"test" "test"}))
  (valid! Bar4 (Bar4. [1] nil))
  (invalid! Bar4 (Bar4. ["a"] {"test" "test"}))
  (is (= 4 (do-something (Bar4. 1 "test")))))

(s/defrecord BarNewStyle
    [foo :- s/Int
     bar :- s/Str
     zoo]
  {(s/optional-key :baz) s/Keyword})

(deftest defrecord-new-style-schema-test
  (is (= (utils/class-schema BarNewStyle)
         (s/record BarNewStyle {:foo s/Int
                                :bar s/Str
                                :zoo s/Any
                                (s/optional-key :baz) s/Keyword})))
  (is (identity (BarNewStyle. 1 :foo "a")))
  (is (= #{:foo :bar :zoo} (set (keys (map->BarNewStyle {:foo 1})))))
  ;; (is (thrown? Exception (map->BarNewStyle {}))) ;; check for primitive long
  (valid! BarNewStyle (BarNewStyle. 1 "test" "a"))
  (invalid! BarNewStyle (BarNewStyle. 1 :foo "a"))
  (valid! BarNewStyle (assoc (BarNewStyle. 1 "test" "a") :baz :foo))
  (invalid! BarNewStyle (assoc (BarNewStyle. 1 "test" "a") :baaaz :foo))
  (invalid! BarNewStyle (assoc (BarNewStyle. 1 "test" "a") :baz "foo")))


;; Now test that schemata and protocols work as type hints.
;; (auto-detecting protocols only works in clj currently)

(def LongOrString (s/either s/Int s/Str))

#?(:clj (s/defrecord Nested [^Bar4 b ^LongOrString c p :- (s/protocol PProtocol)]))
(s/defrecord NestedExplicit [b :- Bar4 c :- LongOrString p :- (s/protocol PProtocol)])

(defn test-fancier-defrecord-schema [klass konstructor]
  (let [bar1 (Bar. 1 "a")
        bar2 (Bar2. 1 "a")]
    (is (= (utils/class-schema klass)
           (s/record
            klass
            {:b Bar4
             :c LongOrString
             :p (s/protocol PProtocol)}
            konstructor)))
    (valid! klass (konstructor {:b (Bar4. [1] {}) :c 1 :p bar2}))
    (valid! klass (konstructor {:b (Bar4. [1] {}) :c "hi" :p bar2}))
    (invalid! klass (konstructor {:b (Bar4. [1] {}) :c "hi" :p bar1}))
    (invalid! klass (konstructor {:b (Bar4. [1] {:foo :bar}) :c 1 :p bar2}))
    (invalid! klass (konstructor {:b nil :c "hi" :p bar2}))))

(deftest fancier-defrecord-schema-test
  #?(:clj (test-fancier-defrecord-schema Nested map->Nested))
  (test-fancier-defrecord-schema NestedExplicit map->NestedExplicit))


(s/defrecord OddSum
    [a b]
  {}
  #(odd? (+ (:a %) (:b %))))

(deftest defrecord-extra-validation-test
  (valid! OddSum (OddSum. 1 2))
  (invalid! OddSum (OddSum. 1 3)))

#?(:clj
(do (s/defrecord RecordWithPrimitive [x :- long])
    (deftest record-with-primitive-test
      (valid! RecordWithPrimitive (RecordWithPrimitive. 1))
      #?(:bb (do (is (invalid! RecordWithPrimitive (RecordWithPrimitive. "a")))
                 (is (invalid! RecordWithPrimitive (RecordWithPrimitive. nil))))
         :default (do (is (thrown? Exception (RecordWithPrimitive. "a")))
                      (is (thrown? Exception (RecordWithPrimitive. nil))))))))

(deftest map->record-test
  (let [subset {:foo 1 :bar "a"}
        exact (assoc subset :zoo :zoo)
        superset (assoc exact :baz :baz)]
    (testing "map->record"
      (is (= (assoc subset :zoo nil)
             (into {} (map->BarNewStyle subset))))
      (is (= exact
             (into {} (map->BarNewStyle exact))))
      (is (= superset
             (into {} (map->BarNewStyle superset)))))

    (testing "strict-map->record"
      (is (thrown? Exception (strict-map->BarNewStyle subset)))
      (is (= exact (into {} (strict-map->BarNewStyle exact))))
      (is (= exact (into {} (strict-map->BarNewStyle exact true))))
      (is (thrown? Exception (strict-map->BarNewStyle superset)))
      (is (= exact (into {} (strict-map->BarNewStyle superset true)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schematized functions

#?(:clj
(deftest split-rest-arg-test
  (is (= (macros/split-rest-arg {} ['a '& 'b])
         '[[a] b]))
  (is (= (macros/split-rest-arg {} ['a 'b])
         '[[a b] nil]))))

;;; fn

(def OddLong (s/both (s/pred odd?) #?(:cljs s/Int :clj long)))

(def +test-fn-schema+
  "Schema for (s/fn ^String [^OddLong x y])"
  (s/=> s/Str OddLong s/Any))

(deftest simple-validated-meta-test
  (let [f (s/fn ^s/Str foo [^OddLong arg0 arg1])]
    (is (= +test-fn-schema+ (s/fn-schema f)))))

#?(:bb nil :clj
(deftest no-wrapper-fn-test
  (let [f (s/fn this [] this)]
    (is (identical? f (f))))))

(deftest no-schema-fn-test
  (let [f (s/fn [arg0 arg1] (+ arg0 arg1))]
    (is (= (s/=> s/Any s/Any s/Any) (s/fn-schema f)))
    (s/with-fn-validation
      (is (= 4 (f 1 3))))
    (is (= 4 (f 1 3)))))

(deftest simple-validated-fn-test
  (let [fthiss (atom [])
        f (s/fn test-fn :- (s/pred even?)
            [^s/Int x y :- {:foo (s/both s/Int (s/pred odd?))}]
            (swap! fthiss conj test-fn)
            (+ x (:foo y -100)))]
    (s/with-fn-validation
      (is (= 4 (f 1 {:foo 3})))
      ;; Primitive Interface Test
      #?(:bb nil
         :clj (is (thrown? Exception (.invokePrim ^clojure.lang.IFn$LOL f 1 {:foo 3})))) ;; primitive type hints don't work on fns
      (invalid-call! f 1 {:foo 4})  ;; foo not odd?
      (invalid-call! f 2 {:foo 3})) ;; return not even?

    (is (= 5 (f 1 {:foo 4})))     ;; foo not odd?
    (is (= 4.0 (f 1.0 {:foo 3}))) ;; first arg not long
    (is (= 5 (f 2 {:foo 3})))     ;; return not even?
    (let [fthiss @fthiss]
      (is (seq fthiss))
      #?(:bb nil
         :clj (is (every? #(identical? % f) fthiss)))))
  (testing
    "Tests that the anonymous function schema macro can handle a
    name, a schema without a name and no return schema."
    (let [named-square (s/fn square :- s/Int [x :- s/Int]
                         (* x x))
          anon-square (s/fn :- s/Int [x :- s/Int]
                        (* x x))
          arg-only-square (s/fn [x :- s/Int] (* x x))]
      (is (= 100
             (named-square 10)
             (anon-square 10)
             (arg-only-square 10))))))



(deftest always-validated-fn-test
  (let [f (s/fn ^:always-validate test-fn :- (s/pred even?)
            [x :- (s/pred pos?)]
            (inc x))]
    (is (= 2 (f 1)))
    (invalid-call! f 2)
    (invalid-call! f -1)))


(s/defn ^:never-validate never-validated-test-fn :- (s/pred even?)
  [x :- (s/pred pos?)]
  (inc x))

(deftest never-validated-fn-test
  (doseq [f [never-validated-test-fn
             (s/fn ^:never-validate test-fn :- (s/pred even?)
               [x :- (s/pred pos?)]
               (inc x))]]
    (s/with-fn-validation
      (is (= 2 (f 1)))
      (is (= 3 (f 2)))
      (is (= 0 (f -1))))))

(s/defn ^:never-validate never-validated-rest-test-fn :- (s/pred even?)
  [arg0 & [rest0 :- (s/pred pos?)]]
  (+ arg0 (or rest0 2)))

(deftest never-validated-rest-test
  (doseq [f [never-validated-rest-test-fn
             (s/fn ^:never-validate rest-test-fn :- (s/pred even?)
               [arg0 & [rest0 :- (s/pred pos?)]]
               (+ arg0 (or rest0 2)))]]
    (s/with-fn-validation
      (is (= 2 (f 0)))
      (is (= 4 (f 2 2)))
      (is (= 1 (f 2 -1))))))

(s/set-compile-fn-validation! false)

(s/defn elided-validation-test-fn :- (s/pred even?)
  [x :- (s/pred pos?)]
  (inc x))

(s/defn ^:always-validate elided-validation-always-test-fn :- (s/pred even?)
  [x :- (s/pred pos?)]
  (inc x))

(s/set-compile-fn-validation! true)

(deftest elided-validation-test
  (doseq [f [elided-validation-test-fn
             elided-validation-always-test-fn]]
    (s/with-fn-validation
      (is (= 2 (f 1)))
      (is (= 3 (f 2)))
      (is (= 0 (f -1))))))

(defn parse-long [x]
  #?(:clj (Long/parseLong x)
     :cljs (js/parseInt x)))

(deftest destructured-validated-fn-test
  (let [LongPair [(s/one s/Int 'x) (s/one s/Int 'y)]
        f (s/fn foo :- s/Int
            [^LongPair [x y] ^s/Int arg1]
            (+ x y arg1))]
    (is (= (s/=> s/Int LongPair s/Int)
           (s/fn-schema f)))
    (s/with-fn-validation
      (is (= 6 (f [1 2] 3)))
      (invalid-call! f ["a" 2] 3))))

(deftest two-arity-fn-test
  (let [f (s/fn foo :- s/Int
            ([^s/Str arg0 ^s/Int arg1] (+ arg1 (foo arg0)))
            ([^s/Str arg0] (parse-long arg0)))]
    (is (= (s/=>* s/Int [s/Str] [s/Str s/Int])
           (s/fn-schema f)))
    (is (= 3 (f "3")))
    (is (= 10 (f "3" 7)))))

(deftest infinite-arity-fn-test
  (let [f (s/fn foo :- s/Int
            ([^s/Int arg0] (inc arg0))
            ([^s/Int arg0 & strs :- [s/Str]]
               (reduce + (foo arg0) (map count strs))))]
    (is (= (s/=>* s/Int [s/Int] [s/Int & [s/Str]])
           (s/fn-schema f)))
    (s/with-fn-validation
      (is (= 5 (f 4)))
      (is (= 16 (f 4 "55555" "666666")))
      (invalid-call! f 4 [3 3 3]))))

(deftest rest-arg-destructuring-test
  (testing "no schema"
    (let [fthiss (atom [])
          f (s/fn foo :- s/Int
              [^s/Int arg0 & [rest0]]
              (swap! fthiss conj foo)
              (+ arg0 (or rest0 2)))]
      (is (= (s/=>* s/Int [s/Int & [(s/optional s/Any 'rest0)]])
             (s/fn-schema f)))
      (s/with-fn-validation
        (is (= 6 (f 4)))
        (is (= 9 (f 4 5)))
        (invalid-call! f 4 9 2))
      (let [fthiss @fthiss]
        (is (seq fthiss))
        #?(:bb nil
           :clj (is (every? #(identical? % f) fthiss))))))
  (testing "arg schema"
    (let [f (s/fn foo :- s/Int
              [^s/Int arg0 & [rest0 :- s/Int]] (+ arg0 (or rest0 2)))]
      (is (= (s/=>* s/Int [s/Int & [(s/optional s/Int 'rest0)]])
             (s/fn-schema f)))
      (s/with-fn-validation
        (is (= 6 (f 4)))
        (is (= 9 (f 4 5)))
        (invalid-call! f 4 9 2)
        (invalid-call! f 4 1.5))))
  (testing "list schema"
    (let [f (s/fn foo :- s/Int
              [^s/Int arg0 & [rest0] :- [s/Int]] (+ arg0 (or rest0 2)))]
      (is (= (s/=>* s/Int [s/Int & [s/Int]])
             (s/fn-schema f)))
      (s/with-fn-validation
        (is (= 6 (f 4)))
        (is (= 9 (f 4 5)))
        (is (= 9 (f 4 5 9)))
        (invalid-call! f 4 1.5)))))

(deftest fn-recursion-test
  (testing "non-tail recursion"
    (let [f (s/fn fib :- s/Int [n :- s/Int]
              (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))]
      (is (= 8 (f 6)))
      (s/with-fn-validation
        (is (= 8 (f 6))))))
  (testing "tail recursion"
    (let [f (s/fn fact :- s/Int [n :- s/Int ret :- s/Int]
              (if (<= n 1) ret (recur (dec n) (* ret n))))]
      (is (= 120 (f 5 1)))
      (s/with-fn-validation
        (is (= 120 (f 5 1)))))))

(deftest fn-metadata-test
  (let [->mkeys #(set (keys (meta %)))]
    (is (= (into (->mkeys (s/fn [])) [:blah])
           (->mkeys ^:blah (s/fn []))))))

;;; defn

(def OddLongString
  (s/both s/Str (s/pred #(odd? (parse-long %)) 'odd-str?)))

(s/defn ^{:tag String} simple-validated-defn :- OddLongString
  "I am a simple schema fn"
  {:metadata :bla}
  [arg0 :- OddLong]
  (str arg0))

(s/defn validated-pre-post-defn :- OddLong
  "I have pre/post conditions"
  [arg0 :- s/Num]
  {:pre  [(odd? arg0) (> 10 arg0)]
   :post [(odd? %)    (<  5 %)]}
  arg0)

(def +simple-validated-defn-schema+
  (s/=> OddLongString OddLong))

(def ^String +bad-input-str+ "Input to simple-validated-defn does not match schema")

;; Test that s/defn returns var
#?(:clj
(with-test
  (s/defn with-test-fn [a b] (+ a b))
  (is (= 3 (with-test-fn 1 2)))
  (is (= 0 (with-test-fn 10 -10)))))

#?(:cljs
(deftest simple-validated-defn-test
  (s/with-fn-validation
    (is (= "3" (simple-validated-defn 3)))
    (invalid-call! simple-validated-defn 4)
    (invalid-call! simple-validated-defn "a"))
  (s/with-fn-validation
    (is (= 7 (validated-pre-post-defn 7)))
    (invalid-call! validated-pre-post-defn 0)
    (invalid-call! validated-pre-post-defn 11)
    (invalid-call! validated-pre-post-defn 1)
    (invalid-call! validated-pre-post-defn "a"))
  (let [e (try (s/with-fn-validation (simple-validated-defn 2)) nil
               (catch js/Error e e))]
    (when e ;; validation can be disabled at compile time, and exception not thrown
      (is (>= (.indexOf (str e) +bad-input-str+) 0))))
  (is (= +simple-validated-defn-schema+ (s/fn-schema simple-validated-defn)))))

#?(:clj
(s/defn ^String multi-arglist-validated-defn :- OddLongString
  "I am a multi-arglist schema fn"
  {:metadata :bla}
  ([arg0 :- OddLong]
     (str arg0))
  ([arg0 :- OddLong arg1 :- Long]
     (str (+ arg0 arg1)))))

#?(:clj
(deftest simple-validated-defn-test
  (is (= "Inputs: [arg0 :- OddLong]\n  Returns: OddLongString\n\n  I am a simple schema fn"
         (:doc (meta #'simple-validated-defn))))
  #?(:bb nil
     :default (is (= '([arg0]) (:arglists (meta #'simple-validated-defn)))))
  (is (= "Inputs: ([arg0 :- OddLong] [arg0 :- OddLong arg1 :- Long])\n  Returns: OddLongString\n\n  I am a multi-arglist schema fn"
         (:doc (meta #'multi-arglist-validated-defn))))
  #?(:bb nil
     :default (is (= '([arg0] [arg0 arg1]) (:arglists (meta #'multi-arglist-validated-defn)))))
  (s/with-fn-validation
    (testing "pre/post"
      (is (= 7 (validated-pre-post-defn 7)))
      (is (thrown-with-msg? AssertionError #"Assert failed: \(odd\? arg0\)"
                            (validated-pre-post-defn 0)))
      (is (thrown-with-msg? AssertionError #"Assert failed: \(> 10 arg0\)"
                            (validated-pre-post-defn 11)))
      (is (thrown-with-msg? AssertionError #"Assert failed: \(< 5 %\)"
                            (validated-pre-post-defn 1)))
      (invalid-call! validated-pre-post-defn "a")))
  (let [{:keys [tag schema metadata]} (meta #'simple-validated-defn)]
    (is (= tag s/Str))
    (is (= +simple-validated-defn-schema+ schema))
    (is (= metadata :bla)))
  (is (= +simple-validated-defn-schema+ (s/fn-schema simple-validated-defn)))

  (s/with-fn-validation
    (is (= "3" (simple-validated-defn 3)))
    (invalid-call! simple-validated-defn 4)
    (invalid-call! simple-validated-defn "a"))

  (is (= "4" (simple-validated-defn 4)))
  (let [^Exception e (try (s/with-fn-validation (simple-validated-defn 2)) nil (catch Exception e e))]
    (is (.contains (.getMessage e) +bad-input-str+))
    #?(:bb nil
       :default (do (is (.contains (.getClassName ^StackTraceElement (first (.getStackTrace e))) "simple_validated_defn"))
                    (is (.startsWith (.getFileName ^StackTraceElement (first (.getStackTrace e))) "core_test.clj")))))))

(s/defn ^:always-validate always-validated-defn :- (s/pred even?)
  [x :- (s/pred pos?)]
  (inc x))

(deftest always-validated-defn-test
  (is (= 2 (always-validated-defn 1)))
  (invalid-call! always-validated-defn 2)
  (invalid-call! always-validated-defn -1))

(s/defn fib :- s/Int [n :- s/Int]
  (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))

(s/defn fact :- s/Int [n :- s/Int ret :- s/Int]
  (if (<= n 1) ret (recur (dec n) (* ret n))))


(deftest defn-recursion-test
  (testing "non-tail recursion"
    (is (= 8 (fib 6)))
    (s/with-fn-validation
      (is (= 8 (fib 6)))))
  (testing "tail recursion"
    (is (= 120 (fact 5 1)))
    (s/with-fn-validation
      (is (= 120 (fact 5 1))))))

;; letfn

(deftest minimal-letfn-test
  (is (= "1"
         (s/letfn
             []
           "1"))))

(deftest simple-letfn-test
  (is (= "1"
         (s/with-fn-validation
           (s/letfn
               [(x :- s/Num [] 1)
                (y :- s/Str [m :- s/Num] (str m))]
             (y (x)))))))

(deftest unannotated-letfn-test
  (is (= "1"
         (s/with-fn-validation
           (s/letfn
               [(x [] 1)
                (y [m] (str m))]
             (y (x)))))))

(deftest no-validation-letfn-test
  (is (= "1"
         (s/letfn
             [(x :- s/Num [] 1)
              (y :- s/Str [m :- s/Num] (str m))]
           (y (x))))))

(deftest mutual-letfn-test
  (is (= [true false]
         (s/letfn
           [(even [x] (if (zero? x) true (odd (dec x))))
            (odd [x] (if (zero? x) false (even (dec x))))]
           [(even 10)
            (odd 10)])))
  (is (= [true false]
         (s/letfn
           [(even :- s/Int [x :- s/Int] (if (zero? x) true (odd (dec x))))
            (odd :- s/Int [x :- s/Int] (if (zero? x) false (even (dec x))))]
           [(even 10)
            (odd 10)])))
  (is (every? s/fn-schema
              (s/letfn
                [(even :- s/Int [x :- s/Int] (if (zero? x) true (odd (dec x))))
                 (odd :- s/Int [x :- s/Int] (if (zero? x) false (even (dec x))))]
                [even odd]))))

(deftest error-letfn-test
  (s/with-fn-validation
    (s/letfn
        [(x :- s/Num [] "1")
         (y :- s/Str [m :- s/Num] (str m))]
      (invalid-call! y (x))))
  (testing "without-fn-validation"
    (s/without-fn-validation
      (s/letfn
        [(f :- s/Num [] "1")]
        (is (= "1" (f))))
      (testing "^:always-validate"
        (s/letfn
          [(^:always-validate f :- s/Num [] "1")]
          (invalid-call! f)))))
  (testing "with-fn-validation"
    (testing "^:always-validate"
      (s/letfn
        [(^:always-validate f :- s/Num [] "1")]
        (invalid-call! f)))
    (testing "^:never-validate"
      (s/with-fn-validation
        (s/letfn
          [(^:never-validate f :- s/Num [] "1")]
          (is (= "1" (f)))))))
  (testing "self recursion"
    (s/with-fn-validation
      (s/letfn
        [(f :- s/Num [] "1")]
        (invalid-call! f))
      (s/letfn
        [(f [x :- s/Num])]
        (invalid-call! f "1"))
      (s/letfn
        [(f
           ([] (f "1"))
           ([x :- s/Int]))]
        (invalid-call! f))))
  (testing "mutual recursion"
    (s/with-fn-validation
      (s/letfn
        [(f [] (g "1"))
         (g [x :- s/Num])]
        (invalid-call! f))
      (s/letfn
        [(f [] (g "1"))
         (g :- s/Num [x] x)]
        (invalid-call! f)))))

;; Primitive validation testing for JVM
#?(:clj
(do

  (def +primitive-validated-defn-schema+
    (s/=> long OddLong))

  (s/defn primitive-validated-defn :- long
    [^long arg0 :- OddLong]
    (inc arg0))


  (deftest simple-primitive-validated-defn-test
    (is (= +primitive-validated-defn-schema+ (s/fn-schema primitive-validated-defn)))

    #?(:bb nil
       :default (is ((ancestors (class primitive-validated-defn)) clojure.lang.IFn$LL)))
    (s/with-fn-validation
      (is (= 4 (primitive-validated-defn 3)))
       #?(:bb nil
          :default (is (= 4 (.invokePrim ^clojure.lang.IFn$LL primitive-validated-defn 3))))
      (is (thrown? Exception (primitive-validated-defn 4))))

    (is (= 5 (primitive-validated-defn 4))))

  (s/defn another-primitive-fn :- double
    [^long arg0]
    1.0)

  (deftest another-primitive-fn-test
    #?(:bb nil
       :default (is ((ancestors (class another-primitive-fn)) clojure.lang.IFn$LD)))
    (is (= 1.0 (another-primitive-fn 10))))))


(deftest with-fn-validation-error-test
  (is (thrown? #?(:clj RuntimeException :cljs js/Error)
               (s/with-fn-validation (throw #?(:clj (RuntimeException.) :cljs (js/Error. "error"))))))
  (is (false? (s/fn-validation?))))


;; def

(deftest def-test ;; heh
  (s/def v 1)
  (is (= 1 v))
  (s/def v "doc" 2)
  (is (= 2 v))
  #?(:clj (is (= "doc" (:doc (meta #'v)))))
  (s/def v :- s/Int "doc" 3)
  (is (= 3 v))
  #?(:clj (is (= "doc" (:doc (meta #'v)))))
  (s/def v :- s/Int 3)
  #?(:clj (is (= String (:tag (meta (s/def v :- String "a"))))))
  #?(:clj (is (thrown? Exception (s/def v :- s/Int "doc" 1.0))))
  #?(:clj (is (thrown? Exception (s/def v :- s/Int 1.0))))
  #?(:clj (is (thrown? Exception (s/def ^s/Int v 1.0)))))


;; defmethod

(defmulti m #(:k (first %&)))

(deftest defmethod-unannotated-test
  (s/defmethod m :v [m x y] (+ x y))
  (is (= 3 (m {:k :v} 1 2))))

(deftest defmethod-input-annotated
  (s/defmethod m :v [m :- {:k s/Keyword} x :- s/Num y :- s/Num] (+ x y))
  (is (= 3
         (s/with-fn-validation (m {:k :v} 1 2)))))

(deftest defmethod-output-annotated
  (s/defmethod m :v :- s/Num [m x y] (+ x y))
  (is (= 3
         (s/with-fn-validation (m {:k :v} 1 2)))))

(deftest defmethod-all-annotated
  (s/defmethod m :v :- s/Num [m :- {:k s/Keyword} x :- s/Num y :- s/Num] (+ x y))
  (is (= 3
         (s/with-fn-validation (m {:k :v} 1 2)))))

(deftest defmethod-input-error-test
  (s/defmethod m :v :- s/Num [m :- {:k s/Keyword} x :- s/Num y :- s/Num] (+ x y))
  (s/with-fn-validation (invalid-call! m {:k :v} 1 "2")))

(deftest defmethod-output-error-test
  (s/defmethod m :v :- s/Num [m :- {:k s/Keyword} x :- s/Num y :- s/Num] "wrong")
  (s/with-fn-validation (invalid-call! m {:k :v} 1 2)))

(deftest defmethod-metadata-test
  (s/defmethod ^:always-validate m :v :- s/Num [m :- {:k s/Keyword} x :- s/Num y :- s/Num] "wrong")
  (is (thrown? #?(:clj RuntimeException :cljs js/Error)
               (m {:k :v} 1 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Composite Schemas (test a few combinations of above)


(deftest nice-error-test
  (let [schema {:a #{[s/Int]}
                :b [(s/one s/Keyword :k) s/Int]
                :c s/Any}]
    (valid! schema {:a #{[1 2 3 4] [] [1 2]}
                    :b [:k 1 2 3]
                    :c :whatever})
    (invalid! schema {:a #{[1 2 3 4] [] [1 2] [:a :b]}
                      :b [:k]
                      :c nil}
              "{:a #{[(not (integer? :a)) (not (integer? :b))]}}")
    (invalid! schema {:a #{}
                      :b [1 :a]
                      :c nil}
              "{:b [(named (not (keyword? 1)) :k) (not (integer? :a))]}")
    (invalid! schema {:a #{}
                      :b [:k]}
              "{:c missing-required-key}")))

(s/defrecord Explainer
    [^s/Int foo ^s/Keyword bar]
  {(s/optional-key :baz) s/Keyword})

#?(:clj ;; clojurescript.test hangs on this test in phantom.js, so marking clj-only
(deftest fancy-explain-test
  (is (= (s/explain {(s/required-key 'x) s/Int
                     s/Keyword [(s/one s/Int "foo") (s/maybe Explainer)]})
         `{~'(required-key x) ~'Int
           ~'Keyword [(~'one ~'Int "foo")
                      (~'maybe
                       (~'record
                        #?(:clj Explainer
                           :cljs schema.core-test/Explainer)
                        {:foo ~'Int
                         :bar ~'Keyword
                         (~'optional-key :baz) ~'Keyword}))]}))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Regression tests

;; map implementation takes precedence in cljs
#?(:clj
   (deftest pprint-test
     (is (= "(maybe Int)" (str/trim (with-out-str (pprint/pprint (s/maybe s/Int))))))))

(deftest print-test
  (is (= "(maybe Int)" (pr-str (s/maybe s/Int)))))

(defrecord ItemTest [first second])

(macros/defrecord-schema CacheTest [schema]
  s/Schema
  (spec [this]
    (collection/collection-spec
     (let [p (spec/precondition this #(instance? ItemTest %) #(list 'instance? ItemTest %))]
       (if-let [evf (:extra-validator-fn this)]
         (some-fn p (spec/precondition this evf #(list 'passes-extra-validation? %)))
         p))
     (fn [x] x)
     [{:schema     s/Int
       :parser     (fn [item-col m]
                     (item-col (:first m))
                     m)
       :error-wrap (fn [err] [:first (utils/error-val err)])}
      {:schema      schema
       :parser      (fn [item-col m]
                      (item-col (:second m))
                      m)
       :error-wrap  (fn [err] [:second (utils/error-val err)])}
      {:schema s/Any
       :parser (fn [_ _] nil)}]
     (fn [_ elts _] (map utils/error-val elts))))
  (explain [_]
    (list 'cache-test)))

(deftest issue-310-error-wrap-cache
  (are [schema value expected]
    (= expected (pr-str (s/check schema value)))
    (->CacheTest s/Int) (->ItemTest :a nil)
    "([:first (not (integer? :a))] [:second (not (integer? nil))])"

    (->CacheTest [s/Int]) (->ItemTest :a nil)
    "([:first (not (integer? :a))] nil)"

    (->CacheTest [s/Int]) (->ItemTest :a [nil])
    "([:first (not (integer? :a))] [:second [(not (integer? nil))]])"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Helpers for defining schemas (used in in-progress work, explanation coming soon)

(s/defschema TestFoo {:bar s/Str})

(deftest test-defschema
  (is (= 'TestFoo (:name (meta TestFoo))))
  (is (= 'schema.core-test (:ns (meta TestFoo)))))

(deftest schema-with-name-test
  (let [schema (s/schema-with-name {:baz s/Num} 'Baz)]
    (valid! schema {:baz 123})
    (invalid! schema {:baz "abc"})
    (is (= 'Baz (s/schema-name schema)))
    (is (= nil (s/schema-ns schema))))
  #?(:clj (is-assert! (s/schema-with-name s/Str 'Baz)
                      #"Named schema \(such as the right-most `s/defalias` arg\) must support metadata: class java.lang.Class")
     :cljs (is-assert! (s/schema-with-name nil 'Baz)
                       #"Named schema \(such as the right-most `s/defalias` arg\) must support metadata: object")))

(deftest schema-name-test
  (is (= 'TestFoo (s/schema-name TestFoo))))

(deftest schema-ns-test
  (is (= 'schema.core-test (s/schema-ns TestFoo))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Testing the ability to redefine schema.macros/fn-validator

(deftest soft-validation-test
  (let [the-log (atom [])
        log #(swap! the-log conj %&)]
    (with-redefs [s/fn-validator
                  (fn [dir fn-name schema checker value]
                    (when-let [err (checker value)]
                      (log dir fn-name value)))]
      (s/with-fn-validation
        (simple-validated-defn 12)
        (simple-validated-defn 13)))
    (is (= [[:input 'simple-validated-defn [12]]
            [:output 'simple-validated-defn "12"]]
           @the-log))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Testing that error messages on multimethods include the name

(defmulti ffbe878f identity)
(s/defmethod ^:always-validate ffbe878f 42
  [x :- s/Str]
  :unreachable)

(s/defmethod ^:always-validate other-namespace/ef408750 42
  [x :- s/Str]
  :also-unreachable)

(deftest multimethod-error-messages
  (testing "multimethods in the same namespace"
    (try
      (ffbe878f 42)
      (is false "unreachable")
      (catch Exception e
        (is (re-find #"ffbe878f"
                     (#?(:cljs .-message :clj .getMessage) e))))))
  (testing "multimethods in a different namespace"
    (try
      (other-namespace/ef408750 42)
      (is false "unreachable")
      (catch Exception e
        (is (re-find #"ef408750"
                     (#?(:cljs .-message :clj .getMessage) e)))))))


;; s/defprotocol

(defprotocol ProtAssumptions
  (prot-assumptions [this a] [this a b] "foo"))

(s/defprotocol PDefProtocolTest1
  "Doc"
  (defprotocoltest1-method1
    :- s/Str
    ;; IMPORTANT don't remove arities, specifically tests 2 arities
    [this a :- s/Int]
    [this a :- s/Int, b :- s/Any]
    "doc 1")
  (defprotocoltest1-method2
    :- s/Str
    ;; IMPORTANT don't add arities, specifically tests 1 arity
    [this a :- s/Int, b :- s/Any]
    "doc 2"))

(defrecord ImplementsPDefProtocolTest1 []
  PDefProtocolTest1
  (defprotocoltest1-method1
    [this a]
    :a))

#?(:bb nil ;;https://github.com/babashka/babashka/issues/1339
   :clj
   (deftest protocol-in-another-ns
     (binding [*ns* *ns*]
       (eval `(ns ~(gensym)))
       (is (= :a (eval `(defprotocoltest1-method1 (ImplementsPDefProtocolTest1.) 1)))))))

(deftest protocol-assumptions-test
  #?(:clj
     (testing "methods never have :inline meta by default"
       (is (= {}
              (-> (var prot-assumptions)
                  meta
                  (select-keys [:inline :inline-arities]))))))
  #?(:cljs
     (testing ":protocol meta on method vars is the protocol name"
       (testing "cc/defprotocol"
         (is (= `ProtAssumptions
                (-> (var prot-assumptions)
                    meta
                    :protocol))))
       (testing "s/defprotocol"
         (is (= `PDefProtocolTest1
                (-> (var defprotocoltest1-method1)
                    meta
                    :protocol))))))
  #?(:bb nil ;;https://github.com/babashka/babashka/issues/1340
     :default (testing ":doc meta on method vars"
                (testing "cc/defprotocol"
                  (is (= "foo"
                         (-> (var prot-assumptions)
                             meta
                             :doc))))
                (testing "cc/defprotocol"
                  (is (str/ends-with?
                        (-> (var defprotocoltest1-method1)
                            meta
                            :doc)
                        "doc 1"))))))

(deftype TDefProtocolTest1 []
  PDefProtocolTest1
  (defprotocoltest1-method1 [this a] (str a))
  (defprotocoltest1-method1 [this a b] b)
  (defprotocoltest1-method2 [this a b] b))

(s/defprotocol PDefProtocolTestDefault
  ;; test two arities
  (pdefprotocol-test-default1 :- s/Str
    [this a :- s/Int]
    [this a :- s/Int, b :- s/Any])
  ;; test single arity
  (pdefprotocol-test-default2 :- s/Str
    [this a :- s/Int, b :- s/Any]))

(extend-protocol PDefProtocolTestDefault
  #?(:clj Object
     ;; default stored in "_" field of protocol method
     :cljs default)
  (pdefprotocol-test-default1
    ([this a] (str a))
    ([this a b] b))
  (pdefprotocol-test-default2 [this a b] b))

(deftest sdefprotocol-test
  (do-template
    [WRAP] (testing (pr-str 'WRAP)
             (WRAP
               (is (= "1" (defprotocoltest1-method1 (->TDefProtocolTest1) 1)))
               (testing "default dispatch"
                 (is (= "1" (pdefprotocol-test-default1 :foo 1)))
                 (is (= "1" (pdefprotocol-test-default1 :foo 1 "1")))
                 (is (= "2" (pdefprotocol-test-default1 "str" 2)))
                 (is (= "2" (pdefprotocol-test-default1 "str" 2 "2")))
                 (is (= "3" (pdefprotocol-test-default1 'a 3)))
                 (is (= "3" (pdefprotocol-test-default1 'a 3 "3"))))))
    do
    s/with-fn-validation
    s/without-fn-validation)
  (testing "metadata"
    (is (= "Doc" (-> #'PDefProtocolTest1 meta :doc)))
    #?(:bb nil ;;https://github.com/babashka/babashka/issues/1340
       :clj (is (= "doc 1" (-> #'defprotocoltest1-method1 meta :doc))))
    #?(:bb nil ;;https://github.com/babashka/babashka/issues/1340
       :clj (is (= "doc 2" (-> #'defprotocoltest1-method2 meta :doc))))
    (is (= (s/=>* s/Str [s/Any s/Int] [s/Any s/Int s/Any])
           (s/fn-schema defprotocoltest1-method1)))
    #?(:clj (is (= (s/=>* s/Str [s/Any s/Int] [s/Any s/Int s/Any])
                   (-> #'defprotocoltest1-method1 meta :schema)))))
  #_ ;; :inline metadata on methods we add to prevent inlining thwarts this compile-time error
  #?(:clj
     (is (thrown-with-msg?
           Exception #"No single method"
           (eval `#(defprotocoltest1-method1 (->TDefProtocolTest1))))))
  #?(:bb nil
     :default (testing "default method errors"
                (s/with-fn-validation
                  (invalid-call! pdefprotocol-test-default1 :foo nil) ;;input
                  (invalid-call! pdefprotocol-test-default1 :foo nil "a") ;;input
                  (invalid-call! pdefprotocol-test-default1 :foo 1 :a) ;;output
                  (invalid-call! pdefprotocol-test-default1 "str" :foo) ;;input
                  (invalid-call! pdefprotocol-test-default1 "str" :foo :a) ;;input
                  (invalid-call! pdefprotocol-test-default1 "str" 1 :a)))) ;;output
  (testing "inlinable positions"
    (s/with-fn-validation
      (is (= "1" (defprotocoltest1-method1 (->TDefProtocolTest1) 1)))
      (is (= "a" (defprotocoltest1-method1 (->TDefProtocolTest1) 1 "a")))
      (is (= "a" (defprotocoltest1-method2 (->TDefProtocolTest1) 1 "a"))))
    #?(:bb nil
       :default (s/with-fn-validation
                  (invalid-call! defprotocoltest1-method1 (->TDefProtocolTest1) :a)
                  (testing "input"
                    (invalid-call! defprotocoltest1-method1 (->TDefProtocolTest1) ::foo "a"))
                  (testing "output"
                    (invalid-call! defprotocoltest1-method1 (->TDefProtocolTest1) 1 ::foo))
                  (invalid-call! defprotocoltest1-method2 (->TDefProtocolTest1) 1 ::foo)))
    ;; try a bunch of contexts and nestings to make sure inlining is defeated
    #?(:bb nil
       :clj 
       (do
         (s/with-fn-validation
           (invalid-call! (eval `(defprotocoltest1-method1 (->TDefProtocolTest1) :a))))
         (s/with-fn-validation
           (invalid-call! (eval `(let [] (defprotocoltest1-method1 (->TDefProtocolTest1) :a)))))))))

#?(:clj
   (s/defprotocol ProtocolCache
     (protocol-cache [this])))

#?(:clj
   (deftest clj-protocol-cache-test
     ;; make test repeatable
     (alter-var-root #'ProtocolCache dissoc :impls)
     (let [x 1
           ;; use partial to hold onto method reference. this acts differently
           ;; with cc/defprotocol because of CLJ-1796 (cache is never invalidated
           ;; on old references).
           call (partial protocol-cache x)]
       (extend-protocol ProtocolCache
         Number
         (protocol-cache [_] :number))
       (is (= :number (protocol-cache x) (call)))
       (extend-protocol ProtocolCache
         Long
         (protocol-cache [_] :long))
       (testing "invalidates .__methodImplCache"
         (is (= :long (protocol-cache x) (call)))))))
