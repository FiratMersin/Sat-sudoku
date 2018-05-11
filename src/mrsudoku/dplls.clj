(ns mrsudoku.dplls
  (:use midje.sweet))


(declare find-1-literal)

(defn find-1-literal [phi]
  (some #(when (= 1 (count %))
           (first %)) phi))

(fact
  (find-1-literal '#{#{a}})
  => 'a
  (find-1-literal '#{#{a b}})
  => nil
  (find-1-literal '#{#{a b} #{c}})
  => 'c
  (find-1-literal '#{#{a b} #{(not c)}})
  => '(not c))

(declare make-true-1)

(defn make-true-1 [clause x]
  (let [not-x (list 'not x)]
    (cond
      (contains? clause x) true
      (contains? clause not-x)
        (let [clause' (disj clause not-x)]
          (if (empty? clause')
            nil
            clause'))
      :else clause)))

(fact
  (make-true-1 '#{a} 'a)
  => true
  (make-true-1 '#{(not a)} 'a)
  => nil
  (make-true-1 '#{b} 'a)
  => '#{b}
  (make-true-1 '#{(not a) b} 'a)
  => '#{b})

(declare make-var-true)

(defn make-var-true [phi x]
  (loop [phi phi, res #{}]
    (if (seq phi)
      (let [clause (make-true-1 (first phi) x)]
        (if (nil? clause)
          nil
          (if (= true clause)
            (recur (rest phi) res)
            (recur (rest phi) (conj res clause)))))
      res)))

(fact
  (make-var-true '#{#{(not c) a (not a)} #{a (not a)} #{c} #{c (not a)} #{c a}} 'a)
  => '#{#{c}}
   (make-var-true '#{#{(not c) a (not a)} #{(not a)}  #{a}} 'a)
  => nil
  (make-var-true '#{#{(not c) a (not a)}  #{a}} 'a)
  => '#{})


(declare make-false-1)

(defn make-false-1 [clause x]
  (let [not-x (list 'not x)]
    (cond
      (contains? clause not-x) true
      (contains? clause x)
        (let [clause' (disj clause x)]
          (if (empty? clause')
            nil
            clause'))
      :else clause)))

(fact
  (make-false-1 '#{a} 'a)
  => nil
  (make-false-1 '#{(not a)} 'a)
  => true
  (make-false-1 '#{b} 'a)
  => '#{b}
  (make-false-1 '#{(not a) b} 'a)
  => true)


(declare make-var-false)

(defn make-var-false [phi x]
  (loop [phi phi, res #{}]
    (if (seq phi)
      (let [clause (make-false-1 (first phi) x)]
        (if (nil? clause)
          nil
          (if (= true clause)
            (recur (rest phi) res)
            (recur (rest phi) (conj res clause)))))
      res)))

(fact
  (make-var-false '#{#{(not c) a (not a)} #{a (not a)} #{c} #{c (not a)} #{c b a}} 'a)
  => '#{#{c b} #{c}}
   (make-var-false '#{#{(not c) a (not a)} #{a} #{c} #{c (not a)} #{c b a}} 'a)
  => nil)



(defn rule-1-literal [phi]
  (if-let [litt (find-1-literal phi)]
    (if-let [phi' (if (symbol? litt)
                    (make-var-true phi litt)
                    (make-var-false phi (second litt)))]
      [phi', (if (symbol? litt) litt (second litt)), (symbol? litt)]
      ;;else formule insatisfiable
      [nil, nil, nil])
    ;;else on n'a pas de trouvé de 1-literal
    nil))

(fact
  (rule-1-literal '#{#{(not c) a (not a)} #{a (not a)} #{(not c)} #{(not c) (not a)} #{(not c) a}})
  => '[#{#{a (not a)}} c false]
  (rule-1-literal '#{#{(not c) a (not a)} #{a (not a)}  #{c (not a)} #{(not c) a}})
  => nil
  (rule-1-literal '#{#{(not c) a (not a)} #{a (not a)} #{(not c)} #{c}  #{c (not a)} #{(not c) a}})
  => [nil,nil,nil])

(declare find-neg-pos-1)

(defn find-neg-pos-1 [m clause]
  (loop [clause clause m m]
    (if (seq clause)
      (let [ [x, xsigne] (if (symbol? (first clause))
                           [(first clause) :positif]
                           [(second (first clause)) :negatif])
             signe (get m x)]
        (case signe
          nil (recur (rest clause) (assoc m x xsigne))
          :positif (recur (rest clause) (if (= xsigne :positif)
                                          m
                                          (assoc m x :suppr)))
          :negatif (recur (rest clause) (if (= xsigne :negatif)
                                          m
                                          (assoc m x :suppr)))
          :suppr (recur (rest clause) m)))
      m)))


(fact
  (find-neg-pos-1 {} '#{a})
  => '{a :positif}
  (find-neg-pos-1 '{a :positif} '#{(not a)})
  => '{a :suppr})

(declare find-neg-pos)

(defn find-neg-pos [phi]
  (some (fn [[x, signe]]
          (if (not= signe :suppr)
            [x signe]
            false))
        (reduce find-neg-pos-1 {} phi)))

(fact
  (find-neg-pos '#{#{(not a)} #{a (not a)} #{c} #{c (not a)} #{c a}})
  => '[c :positif]
   (find-neg-pos '#{#{(not c) a (not a)} #{a (not a)} #{c} #{c (not a)} #{c a}})
  => nil
  (find-neg-pos '#{#{(not c) a (not a)} #{a (not a)} #{(not c)} #{(not c) (not a)} #{(not c) a}})
  => '[c :negatif])



(declare rule-affirmative-negative)

(defn rule-affirmative-negative [phi]
  (if-let [[x, signe] (find-neg-pos phi)]
    (case signe
      :positif (make-var-true phi x)
      :negatif (make-var-false phi x))
    nil))

(fact
  (rule-affirmative-negative '#{#{(not c) a (not a)} #{a (not a)} #{(not c)} #{(not c) (not a)} #{(not c) a}})
  => '#{#{a (not a)}})

(declare get-var)

(defn get-var [litt]
  (if (symbol? litt)
    litt
    (second litt)))


;;premier splitter
(declare trivial-splitter)

(defn trivial-splitter [phi]
  (get-var (first (seq (first phi)))))


;;deuxième splitter
(declare varfreq1)

(defn varfreq1 [m clause]
  (reduce (fn [m litt]
            (let [x (get-var litt)]
              (update m x (fn [xnb]
                            (if xnb
                              (inc xnb)
                              1))))) m clause))

(fact
  (varfreq1 {} '#{a})
  => '{a 1}
  (varfreq1 '{a 1} '#{a})
  => '{a 2}
  (varfreq1 '{a 1} '#{b})
  => '{a 1 b 1}
   (varfreq1 {} '#{(not a)})
  => '{a 1})



(declare varfreqs)

(defn varfreqs [phi]
  (loop [phi phi, m {}]
    (if (seq phi)
      (recur (rest phi) (varfreq1 m (first phi)))
      m)))

(fact
  (varfreqs '#{#{a b c} #{a b} #{c (not b)}})
  => '{a 2, b 3, c 2})


(declare max-val)

(defn max-val [m]
  (loop [m m, x nil, maxnb 0]
    (if (seq m)
      (let [[y, ynb] (first m)]
        (if (> ynb maxnb)
          (recur (rest m) y ynb)
          (recur (rest m) x maxnb)))
      x)))

(fact
  (max-val '{a 2, b 3, c 2})
  => 'b
   (max-val '{a 2, b 2, c 2})
  => 'a)

(declare max-splitter)

(defn max-splitter [phi]
  (max-val (varfreqs phi)))

(fact
  (max-splitter '#{#{a b c} #{a b} #{c (not b)}})
  => 'b)

(declare dpll)

(defn dpll
  ([phi] (dpll phi {} max-splitter))
  ([phi splitter] (dpll phi {} splitter))
  ([phi sat splitter]
     (loop [phi phi, sat sat]
       (if (empty? phi)
         sat;;solution trouvée
         (if-let [[phi', x, xval] (rule-1-literal phi)]
           (if (nil? phi')
             false
             (recur phi' (assoc sat x xval)))
           (if-let [[phi', x, xval] (rule-affirmative-negative phi)]
             (recur phi' (assoc sat x xval))
             (let [x (splitter phi)]
               (or (let [phi-true (make-var-true phi x)]
                     (and phi-true (dpll phi-true (assoc sat x true) splitter)))
                   (let [phi-false (make-var-false phi x)]
                     (and phi-false (dpll phi-false (assoc sat x false) splitter)))
                   nil))))))))


(fact
  (dpll '#{#{a} #{(not a)}})
  => false
   (dpll '#{#{a (not a)}})
  => '{a true}
   (dpll '#{#{a (not a)} #{b}})
  => '{a true b true})
