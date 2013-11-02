(ns github-survey.core
  (require [tentacles.core :as github]))

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
