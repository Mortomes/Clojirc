(ns clojirc.messages-spec
  (:require [clojirc.messages :refer :all]
            [clojure.test :refer [deftest, is, run-tests]]))

(def msg-notice (message ":sinisalo.freenode.net NOTICE * :*** Looking up your hostname..."))

(def msg-mode (message ":Clojirc MODE Clojirc :+i"))

(def msg-privmsg (message ":Mortomes|Work!~Mortomes_@unaffiliated/mortomes PRIVMSG Clojirc :Hi to you!"))

(def msg-ping (message "PING :adams.freenode.net"))

(deftest message-type
  (is (= (msg-type msg-notice ) :notice ))
  (is (= (msg-type msg-mode   ) :mode   ))
  (is (= (msg-type msg-privmsg) :privmsg))
  (is (= (msg-type msg-ping) :ping)))

(deftest message-prefix
  (is (= (msg-prefix msg-notice) "sinisalo.freenode.net"))
  (is (= (msg-prefix msg-mode)   "Clojirc"))
  (is (= (msg-prefix msg-privmsg) "Mortomes|Work!~Mortomes_@unaffiliated/mortomes"))
  (is (= (msg-prefix msg-ping) nil )))

(deftest message-params
  (is (= (msg-params msg-notice) ["*" "*** Looking up your hostname..."]))
  (is (= (msg-params msg-mode) ["Clojirc" "+i"]))
  (is (= (msg-params msg-privmsg) ["Clojirc" "Hi to you!"]))
  (is (= (msg-params msg-ping) ["adams.freenode.net"])))
