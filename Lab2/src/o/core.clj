(ns о.core)
(require '[clojure.string :as string])
(require '[org.httpkit.client :as http])
(require '[net.cgrand.enlive-html :as html])

(defn write-result
  [result url message]  
   (dosync (alter result assoc url message)))

(def redirect-status 
  (list 301 302 303))

(defn parse-object
  [object pattern]
  (string/split object pattern))

(defn get-url-status
  [url]
  (let [options {:timeout 20000 :follow-redirects false}]
    (let [{:keys [status headers body error] :as resp} @(http/get url options)]    
       {:status status :headers headers})))

(defn success-url
  [url urls depth result passed-links]
  (write-result result url (count urls))
  (when (and (not-empty urls) (> depth 1))
    (let [sub-result (ref {})]
      (write-result result url sub-result)
      (start-process nil urls (- depth 1) sub-result passed-links))))

(defn check-status
  [status url result depth passed-links]
  (if (or (nil? status) (= (:status status) 404))
    (do
      (write-result result url "bad")
      false)    
    (if (= (:status status) 200)
      true   
        (if(not-empty (remove #(not (= (:status status) %)) redirect-status))
          (do
            (redirect url result status depth passed-links)
             false)
          false
          ))
    )
)

(defn validate-url 
  [url]
  (and (> (count url) 4) (= "http" (subs url 0 4))))

(defn get-urls
  [url]
  (try
    (let [urls (html/select (html/html-resource (java.net.URL. url))  [:a])]
       (filter validate-url (map #(:href (:attrs %)) urls)))
  (catch Exception e)))

(defn redirect
  [url result status depth passed-links]
  (let [location (:location (:headers status))]
    (let [urls (get-urls location)]
      (write-result result url (string/join " " [(count urls) "redirect" location]))      
          (start-process nil urls (- depth 1) result passed-links)
      )))

(defn print-result [result depth]
    (let [pref (reduce str (repeat depth "    "))]
         (loop [[key & keys](keys (deref result))]
           (print (string/join "" (list pref key)))
           (let [value (get (deref result) key)]
             (if (instance? clojure.lang.Ref value)
               (do
                 (println "" (count (deref value)))
                 (print-result value (+ depth 1)))              
               (println "" value)))
           (when (not (nil? keys ))
          (recur keys)))))


(defn start-process
  [agen urls depth result passed-links] 
   (loop [[url & rest] urls]   
     ;(println (dosync(not-empty (remove #(not(= url %)) (ensure passed-links)))))
     (when (and (not (nil? url)) (dosync (nil? (not-empty (remove #(not(= url %)) (ensure passed-links)))))) 
      ; (println url)
       (dosync (alter passed-links conj url))       
       (let [url-status (get-url-status url)]
        ; (println url)
          (let [status (check-status url-status url result depth passed-links)]  
            ; (println status url-status)
            (when status              
               (let [new-urls (get-urls url)]
             ;    (println new-urls)
                (success-url url new-urls depth result passed-links)))))
        (recur rest))))



(def result (ref {}))
(def passed-links (ref []))
(defn -main 
 [path depth]
 (let [urls (parse-object (slurp path) #"\r\n")
       agent (agent {})]
   (loop [[url & rest] urls]
     (when (not (nil? url)) 
         (send agent start-process urls depth result passed-links)
         (recur rest)
       ))
   (await agent)
   (print-result result 0)
 ))
  

  

  
