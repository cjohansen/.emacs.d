(ns __project-name__.core
  (:use compojure.core)
  (:use hiccup.core)
  (:use hiccup.page)
  (:require [compojure.route :as route]))

(defn render-index []
  (html5
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:name "viewport"
            :content "width=device-width, initial-scale=1.0, maximum-scale=1"}]]))

(defroutes app
  (GET "/" [] (render-index))
  (route/resources "/")
  (route/not-found "<h1>Page not found</h1>"))
