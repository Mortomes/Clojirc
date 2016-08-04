(ns clojirc.network
  (:require [clojirc.parsing :refer :all])
  (:import (java.net Socket)
           (java.io PrintWriter InputStreamReader BufferedReader))
  (:gen-class))


(def server {:name "irc.freenode.net" :port 6667})
(def user {:name "Clojirc Bot" :nick "ClojoBot"})

(declare conn-handler)

(defn connect [{name :name port :port :as server} line-handler]
  (let [socket (Socket. name port)
        in (BufferedReader. (InputStreamReader. (.getInputStream socket)))
        out (PrintWriter. (.getOutputStream socket))
        conn (ref {:in in :out out})]
    (future (conn-handler conn line-handler))
    conn))

(defn write [conn msg]
  (println "--->" msg) 
  (doto (:out @conn)
    (.println (str msg "\r"))
    (.flush)))

(defn login [conn {name :name nick :nick :as user}]
  (write conn (str "NICK " nick))
  (write conn (str "USER " nick " 0 * :" name)))

(defn conn-handler [conn msg-handler]
  (while (nil? (:exit @conn))
    (let [line (.readLine (:in @conn))
          msg (message line)]
      (println "<---" line)
      (println "as msg:" msg)
      (future (msg-handler conn msg))
      (comment (cond
                (re-find #"^ERROR :Closing Link:" line)
                (dosync (alter conn merge {:exit true}))
                (re-find #"^PING" line)
                (write conn (str "PONG " (re-find #":.*" line)))))
      )))

(defn dummy-handler [conn msg]
  (cond
   (= (:command msg) :ping) (write conn (str "PONG :" ((first :params msg))))
   (= (:command msg) :error) (dosync (alter conn merge {:exit true}))
   (= (:command msg) :privmsg) (println "PRIVMSG:" (:params msg))))
