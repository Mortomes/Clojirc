(ns clojirc.parsing
  (require [clojure.string :as str]))

(def notice-example ":sinisalo.freenode.net NOTICE * :*** Looking up your hostname...")


(def not-empty? (comp not empty?))

(defn next-space [x] (str/index-of x \ ))

(defn chain-parse [input initial & functions]
  (loop [remainder input
         [f & fs] functions
         message initial]
    (if (and (not-empty? remainder) (not-empty? functions))
      (let [[next-remainder next-message] (f remainder message)]
        (recur next-remainder fs next-message))
      [remainder message])))

(defn parse-prefix [input m]
  (if (= (first input) \:)
    (let [prefix-end (next-space input)]
      [(subs input (inc prefix-end))
       (assoc m :prefix (subs input 1 prefix-end))])
    [input (assoc m :prefix nil)]))

(defn parse-command [input m]
  (let [command-end (next-space input)]
    [(subs input (inc command-end))
     (assoc m :command (keyword (str/lower-case (subs input 0 command-end))))]))

(defn parse-param [input m]
  (if (= (first input) \:)
    ["" (conj m (subs input 1))]
    (let [param-end (next-space input)]
      (if param-end
        [(str/trim (subs input (inc param-end)))
         (conj m (subs input 0 param-end))]
        ["" (conj m input)]))))

(defn parse-params [input m]
  (let [[remainder result] (apply chain-parse input [] (repeat parse-param))
        my-result (assoc m :params result)]
    [remainder my-result]))

(defn parse-params-old [input m]
  (loop [remainder input
         parsed-params []]
    (if (= (first remainder) \:)
      ["" (assoc m :params (conj parsed-params (subs remainder 1)))] 
      (let [param-end (str/index-of remainder \ )]
        (recur (subs remainder (inc param-end))
               (conj parsed-params (subs remainder 0 param-end)))))))





(defn message [x]
  (let [[remainder parsed-message] (chain-parse x {}
                                                parse-prefix
                                                parse-command
                                                parse-params)]
       parsed-message))

(def msg-type :command)
(def msg-prefix :prefix)
(def msg-params :params)


(defn parse-message [x])
