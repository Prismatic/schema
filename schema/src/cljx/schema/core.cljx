(ns schema.core
  "A library for data structure schema definition and validation.

   For example,

   (check {:foo long :bar [double]} {:foo 1 :bar [1.0 2.0 3.0]})

   returns nil (for successful validation) but the following all return
   truthy objects that look like the bad portions of the input object,
   with leaf values replaced by descriptions of the validation failure:

   (check {:foo long :bar [double]} {:bar [1.0 2.0 3.0]})
   ==> {:foo missing-required-key}

   (check {:foo long :bar [double]} {:foo \"1\" :bar [1.0 2.0 3.0]})
   ==> {:foo (not (instance? java.lang.Long \"1\"))}

   (check {:foo long :bar [double]} {:foo 1 :bar [1.0 2.0 3.0] :baz 1})
   ==> {:baz disallowed-key}

   Schemas are also supported as field/argument metadata in special
   defrecord/fn/defn replacements, using standard ^long ^Class ^Record
   syntax for classes and primitives as usual.  For more complex
   schemata, you must use a map like:

   ^{:schema +a-schema+} or ^{:s +a-schema+} for short, or

   ^{:s? +a-schema+} as shorthand for ^{:s (s/maybe +a-schema+)}.

   This metadata is bakwards compatible, and is ignored by usual
   Clojure forms.

   The new forms are also able to directly accept hints of the form
   ^+a-schema+ where +a-schema+ is a symbol referencing a schema,
   and ^AProtocol where AProtocol is a protocol but these hints are
   not backwards compatible with ordinary
   defrecord/ defn/etc.

   As an alternative, you can also provide schemas in s/defrecord
    and s/defn using the following syntax:

   (s/defn foo :- return-schema
     [a :- a-schema
      b :- b-schema] ...)

   These forms are all compatible and can be mixed and matched
   within a single s/defn (although we wouldn't recommend that for
   readability's sake)."
  (:refer-clojure :exclude [defrecord defn])
  (:require
   [clojure.data :as data]
   [clojure.string :as str]
   [plumbing.core :as plumbing]
   potemkin))

(set! *warn-on-reflection* true)

;; TODO: better error messages for fn schema validation
;; TODO: sequences have to support optional args before final to handle
;; (defn foo [x & [y]]) type things.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schema protocol

(defmacro assert-iae
  "Like assert, but throws an IllegalArgumentException and takes args to format"
  [form & format-args]
  `(when-not ~form (throw (IllegalArgumentException. (format ~@format-args)))))

(defprotocol Schema
  (check [this x]
    "Validate that x satisfies this schema, returning a description of the validation
     failure(s) or nil for success.")
  (explain [this]
    "Expand this schema to a human-readable format suitable for pprinting,
     also expanding classes schematas at the leaves"))

(deftype ValidationError [schema value expectation-delay])

(defmethod print-method ValidationError [^ValidationError err writer]
  (print-method (list 'not @(.expectation-delay err)) writer))

(defmacro validation-error [schema value expectation]
  `(ValidationError. ~schema ~value (delay ~expectation)))

(defn- value-name
  "Provide a descriptive short name for a value."
  [value]
  (if (< (count (str value)) 20) value (symbol (str "a-" (.getName (class value))))))

;; TODO(JW): some sugar macro for simple validations that just takes an expression and does the
;; check and produces the validation-error automatically somehow.

(clojure.core/defn validate [schema value]
  (let [error (check schema value)]
    (when error
      (throw (IllegalArgumentException. (format "Value does not match schema: %s" error))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Leaf values

;; TODO(jw): unfortunately (java.util.Collections/synchronizedMap (java.util.WeakHashMap.))
;; is too slow in practice, so for now we leak classes.  Figure out a concurrent, fast,
;; weak alternative.
(def ^java.util.Map +class-schemata+
  (java.util.concurrent.ConcurrentHashMap.))

;; Can we do this in a way that respects hierarchy?
;; can do it with defmethods,
(clojure.core/defn declare-class-schema!
  "Globally set the schema for a class (above and beyond a simple instance? check).
   Use with care, i.e., only on classes that you control.  Also note that this
   schema only applies to instances of the concrete type passed, i.e.,
   (= (class x) klass), not (instance? klass x)."
  [klass schema]
  (assert-iae (class? klass) "Cannot declare class schema for non-class %s" (class klass))
  (.put +class-schemata+ klass schema))

(clojure.core/defn class-schema
  "The last schema for a class set by declare-class-schema!, or nil."
  [klass]
  (.get +class-schemata+ klass))


(defn- check-class [schema class value]
  (when-not (instance? class value)
    (validation-error schema value (list 'instance? class (value-name value)))))

(extend-protocol Schema
  Class
  (check [this x]
    (or (check-class this this x)
        (when-let [more-schema (class-schema this)]
          (check more-schema x))))
  (explain [this]
    (if-let [more-schema (class-schema this)]
      (explain more-schema)
      (symbol (.getName ^Class this))))

  clojure.lang.AFn
  (check [this x]
    (try (when-not (this x)
           (validation-error this x (list this (value-name x))))
         (catch Throwable t
           (validation-error this x (list 'thrown? t (list this (value-name x)))))))
  (explain [this] this))

;; prevent coersion, so you have to be exactly the given type.
(defmacro extend-primitive [cast-sym class-sym]
  `(extend-protocol Schema
     ~cast-sym
     (check [this# x#]
       (check-class ~cast-sym ~class-sym x#))
     (explain [this#] '~(symbol (last (.split (name cast-sym) "\\$"))))))

(extend-primitive clojure.core$double Double)
(extend-primitive clojure.core$float Float)
(extend-primitive clojure.core$long Long)
(extend-primitive clojure.core$int Integer)
(extend-primitive clojure.core$short Short)
(extend-primitive clojure.core$char Character)
(extend-primitive clojure.core$byte Byte)
(extend-primitive clojure.core$boolean Boolean)

;; TODO: abstract these into predicates?
;; single required value

(clojure.core/defrecord EqSchema [v]
  Schema
  (check [this x]
         (when-not (= v x)
           (validation-error this x (list '= v (value-name x)))))
  (explain [this] (cons '= v)))

(clojure.core/defn eq
  "A value that must be = to one element of v."
  [v]
  (EqSchema. v))

;; enum

(clojure.core/defrecord EnumSchema [vs]
  Schema
  (check [this x]
         (when-not (contains? vs x)
           (validation-error this x (list vs (value-name x)))))
  (explain [this] (cons 'enum vs)))

(clojure.core/defn enum
  "A value that must be = to one element of vs."
  [& vs]
  (EnumSchema. (set vs)))

;; protocol

(clojure.core/defrecord Protocol [p]
  Schema
  (check [this x]
         (when-not (satisfies? p x)
           (validation-error this x (list 'satisfies? p (value-name x)))))
  (explain [this] (cons 'protocol (plumbing/safe-get p :var))))

(clojure.core/defn protocol [p]
  (assert-iae (:on p) "Cannot make protocol schema for non-protocol %s" p)
  (Protocol. p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple helpers / wrappers

;; anything

;; _ is to work around bug in Clojure where eval-ing defrecord with no fields
;; loses type info, which makes this unusable in schema-fn.
;; http://dev.clojure.org/jira/browse/CLJ-1196
(clojure.core/defrecord Anything [_]
  Schema
  (check [this x])
  (explain [this] 'anything))

(def +anything+ (Anything. nil))
(def Top "in case you like type theory" +anything+)


;; either

(clojure.core/defrecord Either [schemas]
  Schema
  (check [this x]
         (when (every? #(check % x) schemas)
           (validation-error this x (list 'every? (list 'check '% (value-name x)) 'schemas))))
  (explain [this] (cons 'either (map explain schemas))))

(clojure.core/defn either
  "The disjunction of multiple schemas."
  [& schemas]
  (Either. schemas))


;; both

(clojure.core/defrecord Both [schemas]
  Schema
  (check [this x]
         (when-let [errors (seq (keep #(check % x) schemas))]
           (validation-error this x (cons 'empty? [errors]))))
  (explain [this] (cons 'both (map explain schemas))))

(clojure.core/defn both
  "The intersection of multiple schemas.  Useful, e.g., to combine a special-
   purpose function validator with a normal map schema."
  [& schemas]
  (Both. schemas))


;; maybe

(clojure.core/defrecord Maybe [schema]
  Schema
  (check [this x]
         (when-not (nil? x)
           (check schema x)))
  (explain [this] (list 'maybe (explain schema))))

(clojure.core/defn maybe
  "Value can be nil or must satisfy schema"
  [schema]
  (Maybe. schema))

(def ? maybe)


;; named

(clojure.core/defrecord NamedSchema [name schema]
  Schema
  (check [this x] (check schema x)) ;; TODO: something more?
  (explain [this] (list 'named name (explain schema))))

(clojure.core/defn named
  "Provide an explicit name for this schema element, useful for seqs."
  [schema name]
  (NamedSchema. name schema))


;; conditional

(clojure.core/defrecord ConditionalSchema [preds-and-schemas]
  Schema
  (check [this x]
         (if-let [[_ match] (first (filter (fn [[pred]] (pred x)) preds-and-schemas))]
           (check match x)
           (validation-error this x (list 'not-any? (list 'matches-pred? (value-name x))
                                          (map first preds-and-schemas)))))
  (explain [this]
           (list 'conditional (for [[pred schema] preds-and-schemas]
                                [pred (explain schema)]))))

(clojure.core/defn conditional
  "Define a conditional schema.  Takes args like cond,
   (conditional pred1 schema1 pred2 schema2 ...),
   and checks the first schema where pred is true on the value.
   Unlike cond, throws if the value does not match any condition.
   :else may be used as a final condition in the place of (constantly true)."
  [& preds-and-schemas]
  (assert-iae (and (seq preds-and-schemas) (even? (count preds-and-schemas)))
              "Expected even, nonzero number of args; got %s" (count preds-and-schemas))
  (ConditionalSchema. (for [[pred schema] (partition 2 preds-and-schemas)]
                        [(if (= pred :else) (constantly true) pred) schema])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Map schemata

(clojure.core/defrecord RequiredKey [k])

(clojure.core/defn required-key
  "A required key in a map"
  [k]
  (RequiredKey. k))

(clojure.core/defn required-key? [ks]
  (or (keyword? ks)
      (instance? RequiredKey ks)))

(clojure.core/defrecord OptionalKey [k])

(clojure.core/defn optional-key
  "An optional key in a map"
  [k]
  (OptionalKey. k))

(defn- explicit-schema-key [ks]
  (cond (keyword? ks) ks
        (instance? RequiredKey ks) (.k ^RequiredKey ks)
        (instance? OptionalKey ks) (.k ^OptionalKey ks)
        :else (throw (RuntimeException. (format "Bad explicit key: %s" ks)))))

(defn- specific-key? [ks]
  (or (required-key? ks)
      (instance? OptionalKey ks)))

(defn- find-extra-keys-schema [map-schema]
  (let [key-schemata (remove specific-key? (keys map-schema))]
    (assert-iae (< (count key-schemata) 2)
                "More than one non-optional/required key schemata: %s"
                (vec key-schemata))
    (first key-schemata)))

(defn- check-explicit-key
  "Validate a single schema key and dissoc the value from m"
  [value [key-schema val-schema]]
  (let [optional? (instance? OptionalKey key-schema)
        k (explicit-schema-key key-schema)
        present? (contains? value k)]
    (cond (and (not optional?) (not present?))
          [k 'missing-required-key]

          present?
          (when-let [error (check val-schema (get value k))]
            [k error]))))

(defn- check-extra-key
  "Validate a single schema key and dissoc the value from m"
  [key-schema val-schema [value-k value-v]]
  (if-not key-schema
    [value-k 'disallowed-key]
    (if-let [error (check key-schema value-k)]
      [error 'invalid-key]
      (when-let [error (check val-schema value-v)]
        [value-k error]))))

(defn- check-map [map-schema value]
  (let [extra-keys-schema (find-extra-keys-schema map-schema)
        extra-vals-schema (get map-schema extra-keys-schema)
        explicit-schema (dissoc map-schema extra-keys-schema)
        errors (concat
                (keep #(check-explicit-key value %)
                      explicit-schema)
                (keep #(check-extra-key extra-keys-schema extra-vals-schema %)
                      (apply dissoc value (map explicit-schema-key (keys explicit-schema)))))]
    (when (seq errors)
      (into {} errors))))

(extend-protocol Schema
  clojure.lang.APersistentMap
  (check [this x]
    (if-not (map? x)
      (validation-error this x (list 'instance? 'clojure.lang.APersistentMap (value-name x)))
      (check-map this x)))
  (explain [this]
    (plumbing/for-map [[k v] this]
      (if (specific-key? k)
        (if (keyword? k)
          k
          (list (cond (instance? RequiredKey k) 'required-key
                      (instance? OptionalKey k) 'optional-key)
                (plumbing/safe-get k :k)))
        (explain k))
      (explain v))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sequence schemata

;; default for seqs is repeated schema.
;; to do destructuring style, can use any number of 'single' elements
;; followed by an optional (implicit) repeated.

(clojure.core/defrecord One [schema name])
(clojure.core/defn one
  "A single element of a sequence (not repeated, the implicit default)"
  [schema name]
  (One. schema name))

(defn- split-singles [this]
  (if (instance? One (last this))
    [this nil]
    [(butlast this) (last this)]))

(extend-protocol Schema
  clojure.lang.APersistentVector
  (check [this x]
    (or (when (instance? java.util.Map x)
          (validation-error this x (list 'not (list 'instance? 'java.util.Map (value-name x)))))
        (when (try (seq x) true
                   (catch Exception e
                     (validation-error this x (list 'throws (list 'seq (value-name x)))))))
        (let [[singles multi] (split-singles this)]
          (loop [singles singles x x out []]
            (if-let [[^One first-single & more-singles] (seq singles)]
              (if (empty? x)
                (conj out
                      (validation-error (vec singles) nil (list 'has-enough-elts? (count singles))))
                (recur more-singles
                       (rest x)
                       (conj out (check (.schema first-single) (first x)))))
              (let [out (cond multi
                              (into out (map #(check multi %) x))

                              (seq x)
                              (conj out (validation-error nil x (list 'has-extra-elts? (count x))))

                              :else
                              out)]
                (when (some identity out)
                  out)))))))
  (explain [this]
    (let [[singles multi] (split-singles this)]
      (vec
       (concat
        (for [^One s singles]
          (list (.name s) (explain (.schema s))))
        (when multi
          ['& (explain multi)]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set schemas

;; Set schemas should look like the key half of map schemas
;; with the exception that required entires don't really make sense
;; as a result, they can have at most *one* schema for elements
;; which roughly corresponds to the 'more-keys' part of map schemas

(extend-protocol Schema
  clojure.lang.APersistentSet
  (check [this x]
    (assert-iae (= (count this) 1) "Set schema must have exactly one element")
    (or (when-not (set? x)
          (validation-error this x (list 'set? (value-name x))))
        (when-let [out (seq (keep #(check (first this) %) x))]
          (validation-error this x (set out)))))

  (explain [this]
    (set (map explain this))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Record schemata

(clojure.core/defrecord Record [klass schema]
  Schema
  (check [this r]
         (or (when-not (instance? klass r)
               (validation-error this r (list 'instance? klass (value-name r))))
             (check-map schema r)
             (when-let [f (:extra-validator-fn this)]
               (check f r))))
  (explain [this]
           (list (symbol (.getName ^Class klass)) (explain schema))))

(clojure.core/defn record
  "A schema for record with class klass and map schema schema"
  [klass schema]
  (assert-iae (class? klass) "Expected record class, got %s" (class klass))
  (assert-iae (map? schema) "Expected map, got %s" (class schema))
  (Record. klass schema))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function schemata

;; These are purely descriptive at this point, and carry no validation.
;; We make the assumption that for sanity, a function can only have a single output schema,
;; over all arities.

(def +infinite-arity+ Long/MAX_VALUE)

(clojure.core/defrecord Fn [output-schema input-schemas] ;; input-schemas sorted by arity
  Schema
  (check [this x] nil) ;; TODO?
  (explain [this]
           (if (> (count input-schemas) 1)
             (list* '=>* (explain output-schema) (map explain input-schemas))
             (list* '=> (explain output-schema) (explain (first input-schemas))))))

(clojure.core/defn arity [input-schema]
  (if (seq input-schema)
    (if (instance? One (last input-schema))
      (count input-schema)
      +infinite-arity+)
    0))

(clojure.core/defn make-fn-schema [output-schema input-schemas]
  (assert-iae (seq input-schemas) "Function must have at least one input schema")
  (assert-iae (every? vector? input-schemas) "Each arity must be a vector.")
  (assert-iae (apply distinct? (map arity input-schemas)) "Arities must be distinct")
  (Fn. output-schema (sort-by arity input-schemas)))

(defn- parse-arity-spec [spec]
  (assert-iae (vector? spec) "An arity spec must be a vector")
  (let [[init more] ((juxt take-while drop-while) #(not= '& %) spec)
        fixed (mapv (fn [i s] `(one ~s ~(str "arg" i))) (range) init)]
    (if (empty? more)
      fixed
      (do (assert-iae (and (= (count more) 2) (vector? (second more)))
                      "An arity with & must be followed by a single sequence schema")
          (into fixed (second more))))))

(clojure.core/defmacro =>*
  "Produce a function schema from an output schema and a list of arity input schema specs,
   each of which is a vector of argument schemas, ending with an optional '& more-schema'
   specification where more-schema must be a sequence schema.

   Currently function schemas are purely descriptive; there is no validation except for
   functions defined directly by s/fn or s/defn"
  [output-schema & arity-schema-specs]
  `(make-fn-schema ~output-schema ~(mapv parse-arity-spec arity-schema-specs)))

(clojure.core/defmacro =>
  "Convenience function for defining functions with a single arity; like =>*, but
   there is no vector around the argument schemas for this arity."
  [output-schema & arg-schemas]
  `(=>* ~output-schema ~(vec arg-schemas)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schematized defrecord

(def primitive-sym? '#{float double boolean byte char short int long
                       floats doubles booleans bytes chars shorts ints longs objects})

(defn- looks-like-a-protocol-var?
  "There is no 'protocol?'in Clojure, so here's a half-assed attempt."
  [v]
  (and (var? v)
       (map? @v)
       (= (:var @v) v)
       (:on @v)))

(defn- fix-protocol-tag [env tag]
  (or (when (symbol? tag)
        (when-let [v (resolve env tag)]
          (when (looks-like-a-protocol-var? v)
            `(protocol (deref ~v)))))
      tag))

(defn- valid-tag? [env tag]
  (and (symbol? tag) (or (primitive-sym? tag) (class? (resolve env tag)))))

(defn- normalized-metadata
  "Take an object with optional metadata, which may include a :tag and/or explicit
   :schema/:s/:s?/:tag data, plus an optional explicit schema, and normalize the
   object to have a valid Clojure :tag plus a :schema field."
  [env imeta explicit-schema]
  (let [{:keys [tag s s? schema]} (meta imeta)]
    (assert-iae (< (count (remove nil? [s s? schema explicit-schema])) 2)
                "Expected single schema, got meta %s, explicit %s" (meta symbol) explicit-schema)
    (let [schema (fix-protocol-tag
                  env
                  (or s schema (when s? `(maybe ~s?)) explicit-schema tag Top))]
      (with-meta imeta
        (-> (or (meta imeta) {})
            (dissoc :tag :s :s? :schema)
            (plumbing/assoc-when :schema schema
                                 :tag (let [t (or tag schema)]
                                        (when (valid-tag? env t)
                                          t))))))))

(defn- extract-arrow-schematized-element
  "Take a nonempty seq, which may start like [a ...] or [a :- schema ...], and return
   a list of [first-element-with-schema-attached rest-elements]"
  [env s]
  (assert (seq s))
  (let [[f & more] s]
    (if (= :- (first more))
      [(normalized-metadata env f (second more)) (drop 2 more)]
      [(normalized-metadata env f nil) more])))

(defn- process-arrow-schematized-args
  "Take an arg vector, in which each argument is followed by an optional :- schema,
   and transform into an ordinary arg vector where the schemas are metadata on the args."
  [env args]
  (loop [in args out []]
    (if (empty? in)
      out
      (let [[arg more] (extract-arrow-schematized-element env in)]
        (recur more (conj out arg))))))

(clojure.core/defn extract-schema-form
  "Pull out the schema stored on a thing.  Public only because of its use in a public macro."
  [symbol]
  (let [s (:schema (meta symbol))]
    (assert-iae s "%s is missing a schema" symbol)
    s))

(defn- maybe-split-first [pred s]
  (if (pred (first s))
    [(first s) (next s)]
    [nil s]))


(defmacro defrecord
  "Define a defrecord 'name' using a modified map schema format.

   field-schema looks just like an ordinary defrecord field binding, except that you
   can use ^{:s/:schema +schema+} forms to give non-primitive, non-class schema hints
   to fields.
   e.g., [^long foo  ^{:schema {:a double}} bar]
   defines a record with two base keys foo and bar.
   You can also use ^{:s? schema} as shorthand for {:s (maybe schema)},
   or ^+schema+ to refer to a var/local defining a schema (note that this form
   is not legal on an ordinary defrecord, however, unlike all the others).

   extra-key-schema? is an optional map schema that defines additional optional
   keys (and/or a key-schemas) -- without it, the schema specifies that extra
   keys are not allowed in the record.

   extra-validator-fn? is an optional additional function that validates the record
   value.

   and opts+specs is passed through to defrecord, i.e., protocol/interface
   definitions, etc."
  {:arglists '([name field-schema extra-key-schema? extra-validator-fn? & opts+specs])}
  [name field-schema & more-args]
  (let [[extra-key-schema? more-args] (maybe-split-first map? more-args)
        [extra-validator-fn? more-args] (maybe-split-first (complement symbol?) more-args)
        field-schema (process-arrow-schematized-args &env field-schema)]
    `(do
       (when-let [bad-keys# (seq (filter #(required-key? %)
                                         (keys ~extra-key-schema?)))]
         (throw (RuntimeException. (str "extra-key-schema? can not contain required keys: "
                                        (vec bad-keys#)))))
       (when ~extra-validator-fn?
         (assert-iae (fn? ~extra-validator-fn?) "Extra-validator-fn? not a fn: %s"
                     (class ~extra-validator-fn?)))
       (potemkin/defrecord+ ~name ~field-schema ~@more-args)
       (declare-class-schema!
        ~name
        (plumbing/assoc-when
         (record ~name (merge ~(plumbing/for-map [k field-schema]
                                 (keyword (clojure.core/name k))
                                 (do (assert-iae (symbol? k)
                                                 "Non-symbol in record binding form: %s" k)
                                     (extract-schema-form k)))
                              ~extra-key-schema?))
         :extra-validator-fn ~extra-validator-fn?))
       ~(let [map-sym (gensym "m")]
          `(clojure.core/defn ~(symbol (str 'map-> name))
             ~(str "Factory function for class " name ", taking a map of keywords to field values, but not 400x"
                   " slower than ->x like the clojure.core version")
             [~map-sym]
             (let [base# (new ~(symbol (str name))
                              ~@(map (fn [s] `(get ~map-sym ~(keyword s))) field-schema))
                   remaining# (dissoc ~map-sym ~@(map keyword field-schema))]
               (if (seq remaining#)
                 (merge base# remaining#)
                 base#))))
       ~(let [map-sym (gensym "m")]
          `(clojure.core/defn ~(symbol (str 'strict-map-> name))
             ~(str "Factory function for class " name ", taking a map of keywords to field values.  All"
                   " keys are required, and no extra keys are allowed.  Even faster than map->")
             [~map-sym]
             (when-not (= (count ~map-sym) ~(count field-schema))
               (throw (RuntimeException. (format "Record has wrong set of keys: %s"
                                                 (data/diff (set (keys ~map-sym))
                                                            ~(set (map keyword field-schema)))))))
             (new ~(symbol (str name))
                  ~@(map (fn [s] `(plumbing/safe-get ~map-sym ~(keyword s))) field-schema)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schematized functions

;; Metadata syntax is the same as for schema/defrecord.

;; Currently, there is zero overhead with compile-fn-validation off,
;; since we're sneaky and apply the schema metadata to the fn class
;; rather than using metadata (which seems to yield wrapping in a
;; non-primitive AFn wrapper of some sort, giving 2x slowdown).

;; For fns we're stuck with this 2x slowdown for now, and
;; no primitives, unless we can figure out how to pull a similar trick

;; The overhead for checking if run-time validation should be used
;; is very small -- about 5% of a very small fn call.  On top of that,
;; actual validation costs what it costs.


;; Clojure has a bug that makes it impossible to extend a protocol and define
;; your own fn in the same namespace [1], so we have to be sneaky about
;; defining fn -- we can't :exclude it above, but we can unmap and then def
;; it at the last minute down here, once we've already done our extending
;; [1] http://dev.clojure.org/jira/browse/CLJ-1195


(ns-unmap *ns* 'fn)

(clojure.core/defn ^Fn fn-schema
  "Produce the schema for a fn.  Since storing metadata on fns currently
   destroys their primitive-ness, and also adds an extra layer of fn call
   overhead, we store the schema on the class when we can (for defns)
   and on metadata otherwise (for fns)."
  [f]
  (assert-iae (fn? f) "Non-function %s" (class f))
  (or (class-schema (class f))
      (plumbing/safe-get (meta f) :schema)))

(clojure.core/defn input-schema
  "Convenience method for fns with single arity"
  [f]
  (let [input-schemas (.input-schemas (fn-schema f))]
    (assert-iae (= 1 (count input-schemas))
                "Expected single arity fn, got %s" (count input-schemas))
    (first input-schemas)))

(clojure.core/defn output-schema
  "Convenience method for fns with single arity"
  [f]
  (.output-schema (fn-schema f)))

(definterface PSimpleCell
  (get_cell ^boolean [])
  (set_cell [^boolean x]))

;; adds ~5% overhead compared to no check
(deftype SimpleVCell [^:volatile-mutable ^boolean q]
  PSimpleCell
  (get-cell [this] q)
  (set-cell [this x] (set! q x)))

(def ^schema.core.PSimpleCell use-fn-validation
  "Turn on run-time function validation for functions compiled when
   *compile-function-validation* was true -- has no effect for functions compiled
   when it is false."
  (SimpleVCell. false))

(defmacro with-fn-validation [& body]
  `(do (.set_cell use-fn-validation true)
       ~@body
       (.set_cell use-fn-validation false)))

;; Helpers for the macro magic

(defn- single-arg-schema-form [[index arg]]
  `(one
    ~(extract-schema-form arg)
    ~(if (symbol? arg)
       (name arg)
       (str "arg" index))))

(defn- rest-arg-schema-form [arg]
  (let [s (extract-schema-form arg)]
    (if (= s Top)
      [Top]
      (do (assert-iae (vector? s) "Expected seq schema for rest args, got %s" s)
          s))))

(defn- input-schema-form [regular-args rest-arg]
  (let [base (mapv single-arg-schema-form (plumbing/indexed regular-args))]
    (if rest-arg
      (vec (concat base (rest-arg-schema-form rest-arg)))
      base)))

(defn- split-rest-arg [bind]
  (let [[pre-& post-&] (split-with #(not= % '&) bind)]
    (if (seq post-&)
      (do (assert-iae (= (count post-&) 2) "Got more than 1 symbol after &: %s" (vec post-&))
          (assert-iae (symbol? (second post-&)) "Got non-symbol after & (currently unsupported): %s" (vec post-&))
          [(vec pre-&) (last post-&)])
      [bind nil])))

(defn- process-fn-arity
  "Process a single (bind & body) form, producing an output tag, schema-form,
   and arity-form which has asserts for validation purposes added that are
   executed when turned on, and have very low overhead otherwise.
   tag? is a prospective tag for the fn symbol based on the output schema.
   schema-bindings are bindings to lift eval outwards, so we don't build the schema
   every time we do the validation."
  [env output-schema-sym bind-meta [bind & body]]
  (assert-iae (vector? bind) "Got non-vector binding form %s" bind)
  (when-let [bad-meta (seq (filter (or (meta bind) {}) [:tag :s? :s :schema]))]
    (throw (RuntimeException. (str "Meta not supported on bindings, put on fn name" (vec bad-meta)))))
  (let [bind (with-meta (process-arrow-schematized-args env bind) bind-meta)
        [regular-args rest-arg] (split-rest-arg bind)
        input-schema-sym (gensym "input-schema")]
    {:schema-binding [input-schema-sym (input-schema-form regular-args rest-arg)]
     :arity-form (if true
                   (let [bind-syms (vec (repeatedly (count regular-args) gensym))
                         metad-bind-syms (with-meta (mapv #(with-meta %1 (meta %2)) bind-syms bind) bind-meta)]
                     (list
                      (if rest-arg
                        (-> metad-bind-syms (conj '&) (conj rest-arg))
                        metad-bind-syms)
                      `(let ~(vec (interleave (map #(with-meta % {}) bind) bind-syms))
                         (let [validate# (.get_cell ~'ufv)]
                           (when validate#
                             (validate
                              ~input-schema-sym
                              ~(if rest-arg
                                 `(list* ~@bind-syms ~rest-arg)
                                 bind-syms)))
                           (let [o# (do ~@body)]
                             (when validate# (validate ~output-schema-sym o#))
                             o#)))))
                   (cons bind body))}))

(defn- process-fn-
  "Process the fn args into a final tag proposal, schema form, schema bindings, and fn form"
  [env name fn-body]
  (let [output-schema (extract-schema-form name)
        output-schema-sym (gensym "output-schema")
        bind-meta (or (when-let [t (:tag (meta name))]
                        (when (primitive-sym? t)
                          {:tag t}))
                      {})
        processed-arities (map (partial process-fn-arity env output-schema-sym bind-meta)
                               (if (vector? (first fn-body))
                                 [fn-body]
                                 fn-body))
        schema-bindings (map :schema-binding processed-arities)
        fn-forms (map :arity-form processed-arities)]
    {:schema-bindings (vec (apply concat [output-schema-sym output-schema] schema-bindings))
     :schema-form `(make-fn-schema ~output-schema-sym ~(mapv first schema-bindings))
     :fn-form `(let [^schema.core.PSimpleCell ~'ufv use-fn-validation]
                 (clojure.core/fn ~name
                   ~@fn-forms))}))

;; Finally we get to the prize

(defmacro fn
  "Like clojure.core/fn, except that schema-style typehints can be given on the argument
   symbols and on the arguemnt vector (for the return value), and (for now)
   schema metadata is only processed at the top-level.  i.e., you can use destructuring,
   but you must put schema metadata on the top level arguments and not on the destructured
   shit.  The only unsupported form is the '& {}' map destructuring.

   This produces a fn that you can call fn-schema on to get a schema back.
   This is currently done using metadata for fns, which currently causes
   clojure to wrap the fn in an outer non-primitive layer, so you may pay double
   function call cost and lose the benefits of primitive type hints.

   When compile-fn-validation is true (at compile-time), also automatically
   generates pre- and post-conditions on each arity that validate the input and output
   schemata whenever *use-fn-validation* is true (at run-time)."
  [& fn-args]
  (let [[name more-fn-args] (if (symbol? (first fn-args))
                              (extract-arrow-schematized-element &env fn-args)
                              ["fn" fn-args])
        {:keys [schema-bindings schema-form fn-form]} (process-fn- &env name more-fn-args)]
    `(let ~schema-bindings
       (with-meta ~fn-form ~{:schema schema-form}))))

(defmacro defn
  "defn : clojure.core/defn :: fn : clojure.core/fn.

   Things of note:
    - Unlike clojure.core/defn, we don't support a final attr-map on multi-arity functions
    - The '& {}' map destructing form is not supported
    - fn-schema works on the class of the fn, so primitive hints are supported and there
      is no overhead, unlike with 'fn' above
    - Output metadata always goes on the argument vector.  If you use the same bare
      class on every arity, this will automatically propagate to the tag on the name."
  [& defn-args]
  (let [[name more-defn-args] (extract-arrow-schematized-element &env defn-args)
        [doc-string? more-defn-args] (maybe-split-first string? more-defn-args)
        [attr-map? more-defn-args] (maybe-split-first map? more-defn-args)
        {:keys [schema-bindings schema-form fn-form]} (process-fn- &env name more-defn-args)]
    `(let ~schema-bindings
       (def ~(with-meta name
               (plumbing/assoc-when (or attr-map? {})
                                    :doc doc-string?
                                    :schema schema-form
                                    :tag (let [t (:tag (meta name))]
                                           (when-not (primitive-sym? t)
                                             t))))
         ~fn-form)
       (declare-class-schema! (class ~name) ~schema-form))))




(set! *warn-on-reflection* false)
