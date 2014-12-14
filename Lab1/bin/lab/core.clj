(ns lab.core)
(require '[clojure.string :as string])

(defn parse-object
  [object pattern]
  (string/split object pattern))

(defn remove-last-item
  [list]
  (map drop-last list))

(defn parse-lines
  [lines-source]
  (loop [lines lines-source 
         lines-destination []]
    (if (empty? lines) 
      lines-destination
      (let [[item & rest] lines]           
        (recur rest (conj lines-destination (parse-object item #",")))))))


(def ra 3)
(def rb (* 1.5 ra))

(defn multiplication
  ([value]
    (* value value))
  ([first-value second-value]
    (* first-value second-value)))

(def alpha (/ 4 (multiplication ra)))
(def beta (/ 4 (multiplication rb)))

(defn squared-difference
  [first-value second-value]
 ; (println first-value " " second-value) 
  (loop [[first & first-rest] first-value
         [second & second-rest] second-value
         result 0]      
    (if (empty? first-rest)
      (+ result (multiplication (- (Double/parseDouble first) (Double/parseDouble second))))               
      (recur first-rest second-rest (+ result (multiplication (- (Double/parseDouble first) (Double/parseDouble second))))))))

(defn hamming-distance
  [first-value second-value]
  (count (filter true? (map not= first-value second-value))))

(defn invert-multiplication
  [first-value second-value]
  (* -1 first-value second-value))

(defn exp 
  [value]
  (Math/exp value))

(defn potential-value
  [first-value second-value factor method]    
  (exp (invert-multiplication factor (method first-value second-value))))

(defn start-potential
  [first-value list-values factor method]
  (loop [[value & values] list-values
         result 0]
   ; (println result)
    (if (empty? values)
      (+ result (potential-value first-value value factor method))
      (recur values (+ result (potential-value first-value value factor method))))))

(defn new-potential
  [first-value value factor method]
  (def potential (- (:potential value) (multiplication (:potential first-value) (potential-value (:value first-value) (:value value) factor method))))
    {:index (:index value) :potential potential :value (:value value)})

(defn get-start-potentials
  [values factor method]
  (loop [[value & rest] values
         result []]
  
    (if (empty? rest)
      (conj result (start-potential value values factor method))
      (recur rest (conj result (start-potential value values factor method))))))

(defn shortest-distance 
  [value values]
 ; (println "shortest-distance ")
  (java.lang.Math/pow (apply min (map (fn [item] (squared-difference value (:value item))) values)) 0.5))

(defn formula [new-potential potantials greatest-potential]
 ; (println "formula")
  (def right-part (/ (shortest-distance (:value new-potential) potantials) ra))
 ; (println "right-part" right-part)
  (def left-part (/ (:potential new-potential) (:potential greatest-potential)))
 ; (println "left-part" left-part)
  (+ right-part left-part))

(defn remove-element 
  [list value]
 ; (println value)
  (remove (fn [x] (= (:index x) (:index value))) list))


(defn calculate-clusters
  [values method]
 ; (println (get-start-potentials values alpha))
  (def start-potentials (map-indexed (fn [index potential] {:index index
                                                            :potential potential
                                                            :value (nth values index)}) (get-start-potentials values alpha method)))
  (def greatest-start-potential (apply max-key :potential start-potentials))
   ;(println start-potentials)
  (def high-potential-border (multiplication 0.5 (:potential greatest-start-potential)))
  (def low-potential-border (multiplication 0.15 (:potential greatest-start-potential))) 
  ;(println greatest-start-potential)
  (loop [potentials  (map (fn [potential] (new-potential greatest-start-potential potential beta method)) 
                          (remove-element start-potentials greatest-start-potential))
        result (list greatest-start-potential)]
        ; (def new-potentials (map (fn [potential] (new-potential (#(nth result 0)) potential beta)) potentials))       
         (def new-greatest-potential (apply max-key :potential potentials))
      ;   (println "1:" (> (:potential new-greatest-potential) high-potential-border))
         (if (> (:potential new-greatest-potential) high-potential-border)        
           (recur (map (fn [potential] (new-potential (first result) potential beta method)) 
                          (remove-element potentials new-greatest-potential)) (conj result new-greatest-potential))
           (do
          ;  (println "2:" (< (:potential new-greatest-potential) low-potential-border))
            (if (< (:potential new-greatest-potential) low-potential-border)
             result
             (do
            ;   (println "3:" (>= (formula new-greatest-potential result greatest-start-potential) 1) )
             (if (>= (formula new-greatest-potential result greatest-start-potential) 1)
               (recur (map (fn [potential] (new-potential (first result) potential beta method)) 
                          (remove-element potentials new-greatest-potential)) (conj result new-greatest-potential))
               (recur (map (fn [value] (if (= (:index value ) (:index new-greatest-potential)) 
                                         {:index (:index value) :value (:value value) :potential 0} value) ) potentials), result)
               ))
             )))
           )
        )
           
(defn -main 
  [path method]
  (let [func (if (= method "h") hamming-distance squared-difference)]
    (let [points (slurp path)]
      (let [lines (parse-object points #"\r\n")]
        (println (calculate-clusters (remove-last-item (parse-lines lines)) func)))))
)

