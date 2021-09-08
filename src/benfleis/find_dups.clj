(ns benfleis.find-dups
  "CLI tool to detect and act upon duplicate files"
  (:gen-class)
  (:require [clojure.data.json :as json])
  (:require [clojure.java.io :as io])
  (:require [clojure.pprint :as pprint])
  (:require [clojure.spec.alpha :as s])
  (:require [clojure.spec.test.alpha :as stest])
  (:require [digest]))

;;;;
;; input: list of directories to scan
;; output: list of {keep}:{skip} sets for removal, [optional]{skipped} set
;;
;; transformation sequence:
;; [dir] -> [file] -> [{file_info:...}]
;;  {:name "}
;;

;; {:to-check [[{:file "f"} {:file "g"}] [{:file "h"}]]}
;; seq of candidate groups of files -> seq of seq of maps w :file
(s/def ::state-file-rec (s/keys :req [::file]))
(s/def ::state-file-group (s/coll-of ::state-file-rec))
(s/def ::state-group-seq (s/coll-of ::state-file-group))
(s/def ::state (s/and
                (s/map-of keyword? ::state-group-seq)
                (s/keys :req [::to-check])))

(assert (not (s/valid? ::state {})))
(assert (s/valid? ::state {::to-check []}))
(assert (not (s/valid? ::state {::to-check 1})))
(assert (s/valid? ::state {::to-check [[] []]}))
(assert (not (s/valid? ::state {::to-check [{::file 2}]})))
(assert (s/valid? ::state {::to-check [[{::file 2 ::sth-else 4}]]}))


(defn dirs->files [dirs]
  (->> dirs
    (map io/file)
    (map file-seq)
    (flatten)
    (filter #(.isFile %))
    (map #(hash-map ::file %, ::size (.length %)))))

(defn decorate-digest-4k [file-rec]
  (assoc file-rec
         ::digest-4k
         (with-open [in (io/input-stream (::file file-rec))]
           (let [buf (byte-array 4096)
                 n (.read in buf)]
             (digest/sha-256 buf)))))

;; (decorate-digest-4k {::file (io/file "/etc/bashrc_Apple_Terminal")})
;; confirmed w manual sha256

(defn decorate-digest-full [file-rec]
  (assoc file-rec ::digest-full (-> file-rec ::file digest/sha-256)))

;; (decorate-digest-full {::file (io/file "/etc/bashrc_Apple_Terminal")})
;; confirmed w manual sha256

;;;;
;; -> coll (decorate size) (group-by size) (separate count (assoc))

(defn separate [pred coll]
  (reduce
   (fn [[to-keep to-skip] item]
      (if (pred item)
        [(conj to-keep item) to-skip]
        [to-keep (conj to-skip item)]))
   [nil nil]
   coll))

(assert
 (=
  (separate even? (range 0 10))
  [[8 6 4 2 0] [9 7 5 3 1]]))

(assert
 (=
  (separate zero? (range 0 10))
  [[0] [9 8 7 6 5 4 3 2 1]]))


(defn separate-singletons [coll]
  (separate #(= 1 (count %)) coll))

(separate-singletons [[] [1] [2 3]])

(assert
 (=
  (separate-singletons [[] [1] [2 3]])
  [
   [[1]]
   [[2 3] []]
   ]))


(defn sieve [decorate-fn group-fn coll]
  (let [res (->> coll
                 (map decorate-fn)
                 (group-by group-fn)
                 (map second)
                 separate-singletons)]
    {::skip (-> res first flatten list) ;; discard group-by singletons for rejects
     ::keep (second res)}))

(s/fdef sieve
  :args (s/cat :decorate-fn ifn? :group-fn ifn? :file-group ::state-file-group)
  :ret ::state-group-seq)

(stest/instrument `sieve)


#_
(assert
 (=
  (sieve #(list (count %) %) first '("a" "ab" "b" "bcd"))
  [[[[3 "bcd"]] [[2 "ab"]]] [[[1 "a"] [1 "b"]]]]
  ))


(defn sieve-by-size [{::keys [to-check] :as state}]
  (let [match-groups (map #(sieve identity ::size %) to-check)
        to-skip (mapcat ::skip match-groups)
        to-keep (mapcat ::keep match-groups)]
    (assoc state ::skip-by-size to-skip ::to-check to-keep)))


(defn sieve-by
  "Sieve remaining possible matches in `(::to-check state)`."
  [decorate-fn group-fn skip-key {::keys [to-check] :as state}]
  (let [match-groups (map #(sieve decorate-fn group-fn %) to-check)
        to-skip (mapcat ::skip match-groups)
        to-keep (mapcat ::keep match-groups)]
    (assoc state skip-key to-skip ::to-check to-keep)))

(def sieve-by-size (partial sieve-by identity ::size ::skip-by-size))
(def sieve-by-digest-4k (partial sieve-by decorate-digest-4k ::digest-4k ::skip-by-digest-4k))
(def sieve-by-digest-file (partial sieve-by decorate-digest-full ::digest-full ::skip-by-digest-full))

#_(let [files-group [{::file :two-sy, ::size 2}
                   {::file :one-o, ::size 1}
                   {::file :two-two, ::size 2}
                   {::file :three-o, ::size 3}
                   {::file :four-1, ::size 4}
                   {::file :four-2, ::size 4}]
      state {::to-check [files-group]}]
  (assert (s/valid? ::state state))
  (let [s2 (sieve-by-size state)]
    (s/explain ::state s2)
    s2)
  )

#_
(let [state {::to-check [(->> ["." "/tmp" "/tmp"] dirs->files)]}
      sieved (-> state sieve-by-size sieve-by-digest-4k sieve-by-digest-file)
      final (-> sieved (assoc ::duplicate (::to-check sieved)) (dissoc ::to-check))]
    final)


(defn init [dirs]
  {::to-check [(dirs->files dirs)]})

(defn finalize [{::keys [to-check duplicate] :as state}]
  (assoc state ::to-check nil ::duplicate (merge to-check duplicate)))

(defn find-in [dirs]
  (-> dirs init sieve-by-size sieve-by-digest-4k sieve-by-digest-file finalize ::duplicate))

(defn -main [& args]
  (clojure.pprint/pprint (find-in ["/Users/ben/Documents" "/Users/ben/backup"])))
