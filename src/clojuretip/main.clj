(ns clojuretip.main
  (:require [ring.adapter.jetty :as jetty]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [html5]]
            [garden.core :refer [css]]
            [compojure.core :refer :all]
            [ring.middleware.session :refer [wrap-session]]))

(def style
  (css
   [:#container {:margin "30px"}]
   [:#primary {:font-weight "bold"}]
   [:ul {:list-style-type "none"
         :padding-left "0px"}]
   [:body {:color "#555"
           :font-family "Menlo, monospace"}]
   [:hr {:height "1px"
         :border "0px"
         :background-color "#aaa"
         :width "375px"}]
   [:footer {:margin-top "70px"
             :font-size "75%"}]))

(defn enque [nm]
  (let [nm (str nm)]
    (cond
      (.endsWith nm "?") (str (subs nm 0 (- (count nm) 1)) "_q")
      (= nm "/") "_"
      ;; (= nm ".") "_dot"
      (= nm "..") "_dotdot"
      :else nm)))

(defn deque [nm]
  (cond
    (.endsWith nm "_q") (str (subs nm 0 (- (count nm) 2)) "?")
    (= nm "_") "/"
    ;; (= nm "_dot") "."
    (= nm "_dotdot") ".."
    :else nm))

(defn show-name [nm old-names]
  (let [name-meta (meta nm)
        fq-enqued-name (str (ns-name (:ns name-meta)) "/" (enque (:name name-meta)))
        fq-dequed-name (str (ns-name (:ns name-meta)) "/" (:name name-meta))
        body (html
              (html5
               [:head [:style {:type "text/css"} style]]
               [:body
                [:div#container
                 [:ul#primary
                  (if (empty? (:arglists name-meta))
                    [:li (:name name-meta)]
                    (for [args (:arglists name-meta)]
                      [:li "(" (interpose " " (cons (:name name-meta) args)) ")"]))]
                 [:pre "  " (:doc name-meta)]

                 [:footer
                  [:p
                   [:a {:href "/"} "random"]
                   ", "
                   [:a {:href (str "/" (enque (:name name-meta)))}
                    "permalink"]
                   ", "
                   [:a {:href (str "http://clojuredocs.org/clojure_core/" fq-enqued-name)}
                    "clojuredocs"]
                   ", "
                   [:a {:href (str "http://clojure.github.io/clojure/clojure.core-api.html#" fq-dequed-name)}
                    "official docs"]]
                  (if-not (empty? old-names)
                    [:p
                     "Recent: "
                     (interpose ", "
                                (for [[idx nm] (map list (iterate inc 0) (take 3 old-names))]
                                  [:a {:href (str "/recent/" idx)} nm]))])
                  [:hr {:align "left"}]
                  [:p "Idea by "
                   [:a {:href "https://github.com/TimMc"} "TimMc"]
                   ". Webified by "
                   [:a {:href "https://github.com/sdegutis"} "sdegutis"]
                   ". Source "
                   [:a {:href "https://github.com/sdegutis/clojuretip"} "on github"]
                   "."]]]]))]
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body body}))

(defn shuffled-names []
  (->> clojure.core
       (quote)
       (the-ns)
       (ns-publics)
       (map first)
       (shuffle)))

(defn var-for [nm]
  (find-var (symbol "clojure.core" (str nm))))

(defn get-stored-names [names-str]
  (if names-str
    (let [names (read-string names-str)]
      (if-not (empty? names)
        names))))

(defroutes app
  (GET "/" [:as req]
    (let [names (or (get-stored-names (:names (:session req)))
                    (shuffled-names))
          [nm & remainder] names
          old-names (cons nm
                          (or (get-stored-names (:old-names (:session req)))
                              []))]
      (assoc (show-name (var-for nm) old-names)
        :session {:names (pr-str remainder)
                  :old-names (pr-str (take 20 old-names))})))
  (GET "/:nm" [nm :as req]
    (let [old-names (or (get-stored-names (:old-names (:session req)))
                        [])]
      (show-name (var-for (deque nm)) old-names)))
  (GET "/recent/:n" [n :as req]
    (let [old-names (or (get-stored-names (:old-names (:session req)))
                        [])
          n (Integer/parseInt n)]
      (if (< -1 n (count old-names))
        (if-let [nm (nth old-names n)]
          (show-name (var-for nm) old-names))))))

(defn -main [port]
  (jetty/run-jetty (-> app
                       (wrap-session))
                   {:port (Integer. port)
                    :join? false}))
