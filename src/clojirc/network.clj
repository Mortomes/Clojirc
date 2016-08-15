(ns clojirc.network
  (:require [clojirc.messages :refer :all]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout go-loop]])
  (:import (java.net Socket)
           (java.io PrintWriter InputStreamReader BufferedReader))
  (:gen-class))


(def server {:name "irc.freenode.net" :port 6667})
(def user {:name "Clojirc Bot" :nick "ClojoBot"})

(declare conn-handler)

(defn shut-down [in out]
  (close! in)
  (close! out))

(defn read-loop [reader]
  (let [out (chan)]
    (go
      (loop []
        (let [line (.readLine reader)]
          (>! out line)
          (recur))))
    out
    ))

(defn write-loop [writer]
  (let [in (chan)]
    (go
      (loop []
        (if-let [line (<! in)]
          (do
            (doto writer
              (.println (str line "\r"))
              (.flush))
            (recur)))))
    in))

(defn connect [{name :name port :port :as server}]
  (let [socket (Socket. name port)
        in (-> socket .getInputStream InputStreamReader. BufferedReader. read-loop)
        out (-> socket .getOutputStream PrintWriter. write-loop )]
    {:in in :out out})
  )

(defn write [out msg]
  (println "--->" msg)
  (go (>! out msg)))

(defn read-handler [in]
  (let [out (chan)]
    (go-loop []
      (if-let [line (<! in)]
        (do (println "<---" line)
            (go (>! out (message line)))
            (recur))
        (close! out)))
    out))

(defn message-handler [in out]
  (go-loop []
    (if-let [msg (<! in)]
      (do
        (cond
         (= (:command msg) :ping) (write out (str "PONG :" (first (:params msg))))
         (= (:command msg) :error) (shut-down in out))
         (= (:command msg) :privmsg) (println "Got a privmsg :D")
        (recur))
      (shut-down in out)
      )
    ))



(defn login [out {name :name nick :nick :as user}]
  (write out (str "NICK " nick))
  (write out (str "USER " nick " 0 * :" name)))

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
  (println "In dummy-handler")
  (cond
   (= (:command msg) :ping) (write conn (str "PONG :" (first (:params msg))))
   (= (:command msg) :error) (dosync (alter conn merge {:exit true}))
   (= (:command msg) :privmsg) (println "PRIVMSG:" (:params msg))))

(defn start-up [server user]
  (let [[in :in out :out :as chans] (connect server)
        reader (read-handler in)
        m-handler (message-handler reader out)]
    (log-in out user)))
