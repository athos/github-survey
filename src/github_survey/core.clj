(ns github-survey.core
  (require [tentacles.core :as github]
           [clj-http.client :as http])
  (import [java.io File]))

(def basic-auth-credential "username:password")

;; GitHub Search API now only provides up to 1,000 results for each search.
;; So, to carry out a complete survey, we have to carefully construct
;; each search query so that their results won't exceed the limit.

(defn fetch-repository-data [dest]
  (doseq [[min max] (concat (map #(list % (+ % 4)) (range 0 500 5))
                            [[500 749] [750 999] [1000 4999] [5000]])]
    (println "searching repositories whose size is between" min "and" max "...")
    (let [search-query (str "language:clojure size:"
                            (if max (str min ".." max) (str ">=" min)))
          query {"q" search-query, "auth" basic-auth-credential, "per_page" 100,
                 "all_pages" true}
          res (partition 2 (github/api-call :get "search/repositories" nil query))]
      (doseq [[_ [_ repos]] res]
        (doseq [repo repos
                :let [filename (str dest "/" (:name repo) "_" (hash repo))]]
          (spit filename repo))
        (Thread/sleep 10000)))))

(defn download-project-files [src dst & {:keys [resume-from]}]
  (doseq [filename (drop-while (if resume-from
                                 #(not= resume-from %)
                                 (constantly false))
                               (.list (File. src)))
          :let [repo (load-file (str src "/" filename))]]
    (println "downloading project.clj for" (:name repo) "...")
    (let [url (str "https://raw.github.com/" (:full_name repo) "/master/project.clj")
          res (http/get url {:throw-exceptions false})]
      (case (:status res)
        200 (spit (str dst "/" filename) (:body res))
        404 nil
        (let [msg (str "received status code " (:status res)
                       " when requesting project.clj for " (:full_name repo))]
          (throw (RuntimeException. msg))))
      (Thread/sleep 10000))))

(defn project-definitions [dir]
  (let [f (fn f [[file & files]]
            (lazy-seq
              (when-not (nil? file)
                (if-let [def (try
                               (read-string (slurp (str dir "/" file)))
                               (catch Exception e))]
                  (cons def (f files))
                  (f files)))))]
    (f (.list (File. dir)))))

(defn dependency-stats [dir]
  (letfn [; convert [[lib1 ver1] [lib2 ver2] ...] to #{lib1 lib2 ...}
          (deps-vec->set [v]
            (try
              (set (map first v))
              (catch Exception e)))
          (extract-deps [def]
            (let [m (apply assoc {} (nthnext def 3))
                  deps (deps-vec->set (:dependencies m))
                  profile-deps (set (for [profile (:profiles m)
                                          deps (deps-vec->set (:dependencies profile))]
                                      deps))]
              (-> #{} (into deps) (into profile-deps))))]
    (-> (for [def (project-definitions dir)
              :when (and (odd? (count def))
                         (> (count def) 3)
                         (= (first def) 'defproject))
              dep (extract-deps def)]
          dep)
        frequencies)))
