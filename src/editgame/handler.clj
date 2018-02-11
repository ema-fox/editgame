(ns editgame.handler
  (:import [java.util UUID])
  (:require [clojure.tools.nrepl.server :refer [start-server]]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.coercions :refer [as-int]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.util.response :refer [redirect set-cookie]]
            [hiccup.page :refer [html5]]
            [hiccup.element :refer [link-to]]
            [hiccup.def :refer [defelem]]
            [hiccup.form :refer [form-to text-field submit-button]]
            [editgame.util :refer :all]))

(defonce server (start-server :port 7888))

(def callbacks (ref {}))

(defn make-callback [f]
  (let [id (str (UUID/randomUUID))]
    (dosync (alter callbacks assoc id f))
    (str "/callback/" id)))

(defn make-callback-link [name f]
  (link-to (make-callback f) name))

(defmacro cblink [name & forms]
  `(make-callback-link ~name (fn [] ~@forms)))

(defmacro formcb [keys & forms]
  `(make-callback (fn [{~(apply hash-map (mapcat (fn [key]
                                                  [key (str key)])
                                                keys)) :form-params}]
                    ~@forms)))

(def state (ref (try
                  (read-string (slurp "save.edn"))
                  (catch Exception err
                    {:map {[0 0] {:title "spawn point"
                                  :owner "admin"}}
                     :players {"admin" {:inventar {:gold 1000}}}}))))

(def saved-state (agent nil))

(add-watch state :save
           (fn [_ _ _ _]
             (future
               (. Thread (sleep 1000))
               (send-off saved-state
                         (fn [old new]
                           (when-not (= old new)
                             (spit "save.edn" new))
                           new)
                         @state))))

(def neighbors [[1 0] [0 1] [-1 0] [0 -1]])

(defn ensure-player [name]
  (dosync
   (when-not (get-in @state [:players name]) 
     (alter state assoc-in [:players name] {:pos [0 0]
                                            :inventar {:gold 10}}))))
  

(defn foo [req]
  (if-let [name (get-in req [:cookies "name" :value])]
    (redirect "/show")
    (html5
      (form-to [:post "/login"]
        "name:"
        (text-field "name")
        (submit-button "login")))))

(defn login [req]
  (let [name (get-in req [:form-params "name"])]
    (set-cookie (redirect "/") "name" name)))

(defn show-players [players]
  (for [player (sort-by #(:gold (:inventar (second %))) > players)]
    [:div (first player) ": " (str (second player))]))

(defmacro action [name & forms]
  `(cblink ~name ~@forms (redirect "/show")))

(defn update-inventar [st name stuff]
  (update-in st [:players name :inventar] (partial merge-with +) stuff))

(defn show-place [pos offset player place name]
  (list
   [:h4 (:title place)]
   (when (some #{offset} neighbors)
     (action "go"
       (dosync
        (alter state assoc-in [:players name :pos] pos)
        (when-let [owner (get-in @state [:map pos :owner])]
          (alter state update-inventar owner {:gold 1})))))
   (if-let [tree (:tree place)]
     [:div
      (let [ready-in (- (+ tree 600) (now))]
        (if (> ready-in 0)
          (str "tree ready in " (int (/ ready-in 60))" minutes")
          (if (= offset [0 0])
            (action "harvest tree"
              (dosync
               (alter state #(-> (update-inventar % name {:wood 1})
                                 (assoc-in [:map pos :tree] (now))))))
            "tree")))])
   (when (= offset [0 0])
     (if (:owner place)
       (list
        (form-to [:post (formcb [title]
                          (dosync
                           (alter state assoc-in [:map pos :title] title))
                          (redirect "/show"))]
          (text-field "title")
          (submit-button "set-title"))
        (if (and (>= (:gold (:inventar player)) 5) (not (:tree place)))
          (action "place tree"
            (dosync
             (alter state #(-> (update-inventar % name {:gold -5})
                               (assoc-in [:map pos :tree] (now))))))))
       (when (>= (:gold (:inventar player)) 10)
         (action "buy"
           (dosync
            (alter state #(-> (update-inventar % name {:gold -10})
                              (assoc-in [:map pos :owner] name))))))))
   [:div [:sub (str pos) "  " (:owner place)]]))

(defelem table [& rows]
  [:table
   (for [row rows]
     [:tr
      (for [cell row]
        [:td cell])])])

(defn show [name]
  (ensure-player name)
  (let [player (get-in @state [:players name])
        [x y] (:pos player)]
    (html5
      (show-players (:players @state))
      (apply table {:style "border-spacing: 2em"}
             (for [xo (range -2 3)]
               (for [yo (range -2 3)]
                 (let [xp (+ x xo)
                       yp (+ y yo)
                       place (get-in @state [:map [xp yp]])]
                   (show-place [xp yp] [xo yo] player place name))))))))

(defn callback [id & args]
  (if-let [cb (@callbacks id)]
    (apply cb args)
    (html5
      [:div "unknow or expired link"]
      (link-to "/show" "go back"))))
       
(defroutes app-routes
  (GET "/" [] foo)
  (POST "/login" [] login)
  (GET "/show" [:as {:keys [cookies]}] (show (get-in cookies ["name" :value])))
  (GET "/callback/:id" [id] (callback id))
  (POST "/callback/:id" [id :as req] (callback id req))
  (route/not-found "Not Found"))

(def app
  (wrap-defaults app-routes
                 (assoc-in site-defaults
                           [:security :anti-forgery] false)))
