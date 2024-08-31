## NEXT
 * [#453](https://github.com/plumatic/schema/issues/453): Preserve `s/enum` order during printing

## 1.4.1 (`2022-09-29`)
 * [#449](https://github.com/plumatic/schema/issues/449): Fix bad jsdoc

## 1.4.0 (`2022-08-16`)
 * Add `s/defprotocol`

## 1.3.5 (`2022-07-28`)
 * [#391](https://github.com/plumatic/schema/issues/391): Improve `s/defalias` error message when passed a schema that doesn't support metadata
 * [#442](https://github.com/plumatic/schema/pull/442): Add support for cross-platform JVM type hints (another attempt at fixing [#174](https://github.com/plumatic/schema/issues/174))
 * [#434](https://github.com/plumatic/schema/pull/434): Fix [API doc generation](http://plumatic.github.io/schema)
 * Fix `view source` in API docs

## 1.3.1 - 1.3.4 (`2022-07-28`)
 * Prefer 1.3.4 (used to test the release process for schema)

## 1.3.0 (`2022-06-10`)
 * [Babashka support](https://github.com/plumatic/schema/pull/440)
 * CLJS: Add pretty `pr-str` for schemas

## 1.2.1 (`2022-04-28`)
 * Fix mutually recursive `s/letfn` bindings
 * Fix `s/fn` perf caveats in Clojure by avoiding wrappers
 * [Fix](https://github.com/plumatic/schema/pull/438) conflict with key named `constructor` in recent ClojureScript versions.

## 1.2.0 (`2021-11-03`)
 * **BREAKING** use `cljc` instead of `cljx`, which requires Clojure 1.7 or later. (#425)

## 1.1.12 (`2019-08-10`)
 * Fixes a "wrong number of arguments" warning in clojurescript (#418)
 * Fixes an issue with function name inference introduced in the previous
   release (#416)
 * Improved the names of built-in predicate schemas in clojurescript production
   environments (#417)

## 1.1.11 (`2019-06-12`)
 * No longer swallows fatal exceptions on the JVM ([#413](https://github.com/plumatic/schema/pull/413))
 * Wildcard keys in map schemas are internally amended
   in a way that fixes [a bug in schema-generators](https://github.com/plumatic/schema-generators/issues/16)
 * Minor fixes for more recent ClojureScript versions

## 1.1.10
 * Fix warnings in newer versions of ClojureScript around `aget`, `aset`, and `uuid`.

## 1.1.9
 * Fix bug introduced in 1.1.8 ([#402](https://github.com/plumatic/schema/pull/402)).

## 1.1.8
 * Improved errors for multimethods.

## 1.1.7
 * Remove unnecessarily relative marker in `::schema.spec.collection/` keywords to fix compatibility with latest Clojure 1.9 alpha.

## 1.1.6
 * Add exclusions to workaround `->MapEntry` warnings with latest cljs.

## 1.1.5
 * Add float type to JVM coercers.

## 1.1.4
 * Highlights schema validation errors
 * Fix an issue with `isa?` and the global hierarchy
 * Fix an issue with coercion and map entries

## 1.1.3
 * Make fn validation customizable by addition of `fn-validator` function

## 1.1.2
 * Exclude `clojure.core/Inst` to avoid warnings in Clojure 1.9 alphas

## 1.1.1
 * Fix (at least some) AOT issues around PSimpleCell/SimpleVCell, by replacing with atom/AtomicReference.

## 1.1.0
 * **Deprecate* schema.experimental.generators and schema.experimental.complete.  Find them in their new home in the separate schema-generators project.
 * **BREAKING** change the internal details of collection specs (should only be an issue for custom collection schemas that don't rely on helpers `one-element` or `all-elements`).
 * Fix generation for sequence schemas containing a non-trailing `s/optional` element.

## 1.0.6
 * Install a pprint method that uses the explain, in addition to an ordinary print-method.  Should fix large prints and stack overflows while pprinting schemas, or with plugins such as `pretty`.

## 1.0.5
 * Fix completion through non-map collections

## 1.0.4
 * Attempt to resolve issues with AOT compilation by moving typehint on `use-fn-validation` var to callsites.
 * Update generators, bump minimum version of test.check to 0.9.0.  Generators will only work under Clojure 1.7.0+.
 * Better performance for anonymous schematized functions, via lazy checker creation

## 1.0.3
 * Fix warning about overriding `atom` under Clojure 1.7
 * Fix behavior of `constrained` with some schemas (e.g. maps).

## 1.0.2
 * Extend keyword `enum` coercion to keyword `eq` coercion
 * Add `s/atom` schema for atoms
 * Add `coercer!` which throws on error
 * Add leaf generators for UUIDs
 * Make `s/defn` compatible with `with-test`
 * Add `constrained` schema for postconditions (replaces `(both x (s/pred ...))`)

## 1.0.1
 * Catch and report exceptions in guards the same as preconditions, rather than allowing them to propagate out.

## 1.0.0
 * New schema backend, which is faster, simpler, and more declarative, enabling more applications and simplifying tooling.  Users of built-in schema types should experience very little or no breakage, but tooling or custom schema types will need to be updated.   As a concrete example of an application that's enabled, schema now experimentally supports test-check style generation from schemas, as well as completion of partial inputs.
 * **BREAKING** Changes to the core Schema protocol will break existing third-party schema tooling and schema types.
 * **BREAKING** Records coerced to an ordinary (non-record) map schema are now converted to maps, rather than retaining their record type.
 * **Deprecate** `s/either` in favor of `s/cond-pre`, `s/conditional`, or `schema.experimental.abstract-map-schema`.  As of this release, `either` no longer works with coercion.
 * **Deprecate** `s/both` in favor of improved `s/conditional`.
 * **Deprecate** `schema.core/defrecord+`; moved to new `schema.potemkin` namespace.
 * `s/pred` can more intelligently guess the predicate name
 * `record` schemas can now coerce values to corresponding record types.
 * New experimental `abstract-map-schema` that models super/subclasses as maps.
 * Improved explains explains for leaf schemas, especially in Clojurescript.

## 0.4.4
 * Fix ClojureScript warnings about `map->Record` constructors being redefined.
 * Add queue schemas
 * Configurable maximum length for values in error messages
 * Fix potential memory leaks after many redefinitions of `s/defn` or `s/defrecord`.

## 0.4.3
 * Fix longstanding AOT compilation issue when used with Clojure 1.7.0-RC1 and later.

## 0.4.2
 * Add recursive schema support for ClojureScript
 * Add ns metadata to defschema

## 0.4.1
 * Fix some harmless warnings when using Schema with the latest version of ClojureScript (due to the addition of positional constructors for `deftype`).

## 0.4.0
 * **BREAKING** Remove support for old `^{:schema ..}` style annotations. `:- schema` is the preferred way, but metadata-style schemas are still allowed for valid Clojure typehints.
 * **BREAKING** Remove support for bare `:- Protocol` annotations (use `:- (s/protocol Protocol)` instead).
 * **BREAKING** Remove deprecated macros (`defn`, `defrecord`, etc) from schema.macros.  The identical versions in schema.core remain.
 * **BREAKING** Remove potemkin as a dependency, and the `*use-potemkin*` flag.  To get the old behavior of potemkin defrecords, you can still bring your own potemkin and use `schema.core/defrecord+` in place of `schema.core/defrecord`.

## 0.3.7
 * Add coercion handler for s/Uuid from string input

## 0.3.6
 * Support java.util.List instances as valid data for sequence schemas

## 0.3.5
 * Make primitive schemas work better in some cases under partial AOT compilation

## 0.3.3
 * Fix bug in `defschema` which clobbered metadata, breaking `s/protocol` in Clojure in 0.3.2.

## 0.3.2
 * Fix `s/protocol` in Clojure (didn't work properly with extends created later)
 * Fix ClojureScript (Closure) warning about reference to global RegExp object.
 * Add `set-compile-fn-validation!` function to turn off emission of validation globally, and turn off emission of validation code for non- ^:always-validate functions when *assert* is false.

## 0.3.1
 * Fix Clojurescript compilation warnings/errors from accidental references to `clojure.data/diff` and `class` inside error messages.

## 0.3.0
 * **BREAKING** increase minimum clojurescript version 2120 to support :include-macros
 * **Deprecate** direct use of `schema.macros` in client code -- prefer canonical versions in `schema.core`
   in both Clojure and ClojureScript, using `:include-macros true` in cljs.
 * **Deprecate** old `^{:s schema}` syntax for providing schemas.
 * **Deprecate** `*use-potemkin*` flag and behavior to default to potemkin s/defrecords in Clojure;
   in future releases, you will have to provide your own potemkin and explicitly opt-in to this behavior.
 * (Hopefully) fix issues with AOT compilation, by removing dependence on potemkin/import-vars.
 * Add `isa` schema for Clojure hierarchies.
 * Preserve the types of maps (including Records) when coercing with map schemas.
 * Smarter code generation in s/defrecord to avoid dead code warnings
 * Fix printed form of s/Str in ClojureScript
 * Make some internal fns public to simplify third-part schema extensions
 * Walking records with map schemas preserves the record type
 * Proper explain for s/Str

## 0.2.6
 * Memoize walker computation, providing much faster checker compilation for graph-structured schemas

## 0.2.5
 * Add `normalized-defn-args` helper fn for defining `s/defn`-like macros.
 * Map schemas correctly validate against struct-maps

## 0.2.4
 * Fixed an issue that could cause ClojureScript compilation to fail
 * Generalize `s/recursive` to work on artibrary refs
 * Add `s/Symbol` as a cross-platfor primitive

## 0.2.3
 * Improved explains for primitives & primitive arrays
 * More robust double coercions
 * Fix cljs warning about extending js/Function
 * Import schema.macros/defmulti in schema.core

## 0.2.2
 * Add validated `s/def`.
 * Add validated `s/defmethod`.
 * Add `Bool` coercions.

## 0.2.1
 * Add `Bool` to cross-platform primitives
 * Fix several minor bugs
 * Replace cljs-test with headless clojurescript.test.

## 0.2.0
 * **breaking change:** Cross-platform leaves String and Number are now Str and Num (the former caused warnings and broke AOT).
 * Replaced core Schema protocol method `check` with `walker`, for increased speed and versatility
 * Support for schema-driven transformations/coercion
 * Schemas for primitive arrays (`longs`, etc)
 * Schematized `letfn`

## 0.1.10
 * Remove non-dev dependency on cljx

## 0.1.9
 * Support for pre/postcondition maps in `s/defn`
 * Support for recursive schemas in Clojure
 * Fixes for sm/defn and sm/defrecord with cljs advanced compilation

## 0.1.8
 * Works with advanced compilation in cljs (at least sometimes)

## 0.1.7
 * More small bugfixes
 * Better validation error messages in cljs

## 0.1.6
 * Minor bugfixes (thanks various contributors)
 * Extend schema protocol to regex (thanks [AlexBaranosky](https://github.com/AlexBaranosky)).
 * Add `:never-validate` meta option

## 0.1.5
 * Fix regression in primitive handling introduced in 0.1.4

## 0.1.4
 * Added Regex, Inst, and Uuid as primitive schema types (thanks [jwhitlark](https://github.com/jwhitlark))
 * Add annotated arglists to functions defined with `s/defn` (thanks [danielneal](https://github.com/danielneal))
 * Add `set-fn-validation!` to schema.core, to globally turn validation on or off.
 * Add `:always-validate` metadata on fn/defn name to unconditionally use validation.

## 0.1.3
 * Fix compatibility with Clojurescript 1889 (removal of format)

## 0.1.2
 * Validate returns the value on success
 * Sequence schemas only match sequential? things, to match map and set
 * Implementation of `defschema` puts name in metadata, rather than generating named schema
 * Improved error messages and stack traces for `s/defn`

## 0.1.1
 * Bugfix: with-fn-validation persisting after Exception

## 0.1.0
 * Initial release
