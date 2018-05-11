(ns mrsudoku.normal
   (:require [clojure.core.match :refer [match]])
  (:use midje.sweet)
  (:require [clojure.set :as set]))

(defn simplify-one [f]
  (match f
    ;; *** simplification du not ***
    ;; (not true) -> false
    (['not true] :seq) false
    ;; (not false) -> true
    (['not false] :seq) true
    ;; (not (not a)) -> a
    (['not (['not a] :seq)] :seq) a
    ;; *** simplification du or ***
    ;; (or true a) -> true
    (['or true a] :seq) true
    ;; (or a true) -> true
    (['or a true] :seq) true
    ;; (or false a) -> a
    (['or false a] :seq) a
    ;; (or a false) -> a
    (['or a false] :seq) a
    (['or false false] :seq) false
    (['or true true] :seq) true
    ;; *** simplification du and ***
    ;; (and true a) -> a
    (['and true true] :seq) true
    (['and true a] :seq) a
    ;; (and a true) -> a
    (['and a true] :seq) a
    ;; (and false a) -> false
    (['and false a] :seq) false
    ;; (and a false) -> false
    (['and a false] :seq) false
    ;; *** simplification de l'implication ==> ***
    (['==> true a] :seq) a
    (['==> a true] :seq) true
    (['==> false a] :seq) true
    (['==> a false] :seq) (list 'not a)
    ;; *** simplification de l'équivalence <=> ***
    ;;(<=> true true) -> true
    (['<=> true true] :seq) true
    ;;(<=> true false) -> false
    (['<=> true false] :seq) false
    ;;(<=> false true) -> false
    (['<=> false true] :seq) false
    ;;(<=> false false) -> true
    (['<=> false false] :seq) true
    :else f))


(fact
  (simplify-one '(not true)) => false
  (simplify-one '(not false)) => true
  (simplify-one '(not (or true (not false)))) => '(not (or true (not false)))
  (simplify-one '(not (not true))) => true
  (simplify-one '(==> true false)) => false
  (simplify-one '(and false :a)) => false
  (simplify-one '(or false :a)) => :a
  (simplify-one '(and true true)) => true)

(defn simplify [f]
  (match f
    ([op a] :seq) (simplify-one (list op (simplify a)))
    ([op a b] :seq) (simplify-one (list op (simplify a)
                                          (simplify b)))
    :else f))

(fact
  (simplify '(or (not (or (not true)
                        (and (or (not x) false)
                             (or (not false) x))))
               (not (or y (and false z)))))
  => '(or  x (not y))

  (simplify '(not (or true (not false))))
  => false

  (simplify '(==> (or true (not false)) false))
  => false

  (simplify '(<=> (not (or true (not false))) false))
  => true
  )

(defn nnf' [f]
  (match f
    ;; not .. and
    (['not (['and a b] :seq)] :seq)
    (list 'or (nnf' (list 'not  a)) (nnf' (list 'not b)))
    ;; not .. or
    (['not (['or a b] :seq)] :seq)
    (list 'and (nnf' (list 'not  a)) (nnf' (list 'not b)))
    ;: not .. not
    (['not (['not a] :seq)] :seq) (nnf' a)
    ;; and ..
    (['and a b] :seq) (list 'and (nnf' a) (nnf' b))
    ;; or ..
    (['or a b] :seq) (list 'or (nnf' a) (nnf' b))
    ;; TODO ==> et <=>
    ;; not ==> a b  -> (and (nnf a) (nnf (not b)))
    (['not (['==> a b] :seq)] :seq) (list 'and (nnf' a) (nnf' (list 'not b)))
    ;; ==> a b  -> (or (nnf (not a)) (nnf b))
    (['==> a b] :seq) (list 'or (nnf' (list 'not a)) (nnf' b))
    ;; not <=> a b -> (or (and (nnf (not a)) (nnf b)) (and (nnf a) (nnf (not b))))
    (['not (['<=> a b] :seq)] :seq)
         (list 'or (list 'and (nnf' (list 'not a)) (nnf' b)) (list 'and (nnf' a) (nnf' (list 'not b))))
    ;; <=> a b -> (and (or (nnf (not a)) (nnf b)) (or (nnf a) (nnf (not b))))
    (['<=> a b] :seq)
         (list 'and (list 'or (nnf' (list 'not a)) (nnf' b)) (list 'or (nnf' a) (nnf' (list 'not b))))
:else f))

(fact
  (nnf' '(==> (or a b) (<=> c d)))
  => '(or (and (not a) (not b)) (and (or (not c) d) (or c (not d))))

  (nnf' '(not (or true (not false))))
  => '(and (not true) false)

  (nnf' '(or (not (or (not true)
                    (and (or (not x) false)
                         (or (not false) x))))
           (not (or y (and false z)))))
  => '(or (and true (or (and x (not false)) (and false (not x))))
          (and (not y) (or (not false) (not z))))

  (nnf' '(or (<=> l1c7b1 (not l3c7b1)) (or (<=> l1c7b2 (not l3c7b2)) (<=> l1c7b3 (not l3c7b3)))))
  => '(or (and (or (not l1c7b1) (not l3c7b1)) (or l1c7b1 l3c7b1))
        (or (and (or (not l1c7b2) (not l3c7b2))  (or l1c7b2 l3c7b2))
            (and (or (not l1c7b3) (not l3c7b3)) (or l1c7b3 l3c7b3))))
  )




(defn nnf [f]
  (nnf' (simplify f)))


(fact
  (nnf '(or (not (or (not true)
                    (and (or (not x) false)
                         (or (not false) x))))
           (not (or y (and false z)))))
  => '(or x (not y)))


(defn distrib [f]
  (match f
    (['and (['or a b] :seq) c] :seq)
    (list 'or (distrib (list 'and a c))
              (distrib (list 'and b c)))
    (['and a (['or b c] :seq)] :seq)
    (list 'or (distrib (list 'and a b))
              (distrib (list 'and a c)))
    (['or (['and a b] :seq) c] :seq)
    (list 'and (distrib (list 'or a c))
               (distrib (list 'or b c)))
    (['or a (['and b c] :seq)] :seq)
    (list 'and (distrib (list 'or a b))
               (distrib (list 'or a c)))
    :else f))


(defn cnf' [f]
  (match f
    (['and a b] :seq) (list 'and (cnf' a)
                                 (cnf' b))
    (['or a b] :seq) (distrib (list 'or (cnf' a)
                                        (cnf' b)))

    :else f))

(defn cnf [f]
  (cnf' (nnf f)))

(cnf '(<=> (and a (or b c)) (and (not a) (not c))))


(defn setify-or [f]
  (match f
    (['or a b] :seq)
         (set/union (setify-or a) (setify-or b))
    :else #{f}))

(fact
  (setify-or '(or a (or a (or (not b) (not b)))))
  => '#{a (not b)})


(defn setify-cnf [f]
  (match f
    (['and a b] :seq) (set/union (setify-cnf a) (setify-cnf b))
    (['or a b] :seq) #{(setify-or f)}
    :else #{#{f}}))


(fact
 (setify-cnf '(and a b))
  => '#{#{a} #{b}}
 (setify-cnf '(and (and (or a c) (or a d)) (and (or b c) (or b d))))
  => '#{#{c b} #{a d} #{b d} #{a c}}
  )


(defn filter-trivial-one [clause];;retourne true si clause à supprimer, false sinon, les littéraux doivent être des symboles
 (loop [clause clause, m {}]
   (if (seq clause)
     (let [[litt, signe] (if (symbol? (first clause))
                           [(first clause), :positif]
                           [(second (first clause)), :negatif])]
       (if (= signe :positif)
         (if (= :negatif (get m litt))
           true
           (recur (rest clause), (assoc m litt signe)))
         (if (= :positif (get m litt))
           true
           (recur (rest clause), (assoc m litt signe)))))
     false)))

(fact
  (filter-trivial-one '#{a b (not a)})
  => true
  (filter-trivial-one '#{a b})
  => false)

(defn filter-trivial-cnf [f];; f est un ensemble d'ensembles
  (loop [f f, resf #{}]
    (if (seq f)
      (if (filter-trivial-one (first f))
        (recur (rest f) resf)
        (recur (rest f) (conj resf (first f))))
      resf)))

(fact
  (filter-trivial-cnf '#{#{a b (not a)} #{a b}})
  => '#{#{a b}})

(defn filter-contains-cnf-aux [clause1 clause2]
  (if (<= (count clause1) (count clause2))
    false
    (loop [c clause2]
      (if (seq c)
        (if (contains? clause1 (first c))
          (recur (rest c))
          false)
        true))))

(fact
  (filter-contains-cnf-aux '#{a b c} '#{a})
  => true
  (filter-contains-cnf-aux '#{a} '#{b c})
  => false
  (filter-contains-cnf-aux '#{a b c} '#{a b})
  => true)


(defn filter-contains-cnf-clause [f' clause]
  (loop [f' f' m #{}]
    (if (seq f')
      (if (filter-contains-cnf-aux (first f') clause)
        (recur (rest f') (conj m (first f')))
        (recur (rest f') m))
      m)))

(fact
  (filter-contains-cnf-clause '#{#{a b c} #{a b}} '#{a})
  => '#{#{a b c} #{a b}}
  (filter-contains-cnf-clause '#{#{a b c} #{a b}} '#{a b c d})
  => '#{})

(defn disj-phi-m [phi m]
  (loop [phi phi, m m]
    (if (seq m)
      (recur (disj phi (first m)) (rest m))
      phi)))

(fact
   (disj-phi-m '#{#{a b c} #{a b}} '#{#{a b c} #{a b}})
  => '#{})

(defn map-clause-size [f]
  (loop [f f, m {}]
    (if (seq f)
      (recur (rest f) (assoc m (count (first f)) (if (nil? (get m (count (first f))))
                                                   #{(first f)}
                                                   (conj (get m (count (first f))) (first f)))))
      m)))

(fact
  (map-clause-size '#{#{a b c}})
  => '{3 #{#{a c b}}}
  (map-clause-size '#{#{a b} #{b d}})
  => '{2 #{#{a b} #{b d}}})


(defn filter-contains-cnf [f]; si une clause B contient une clause A de taille 1 alors on supprime B

  (let [mcs (map-clause-size f)]
  (loop [mcs mcs, m #{},i 1]
    (if (= 1 i)
      (if (empty? (get mcs i))
        (recur mcs m (inc i))
        (let [w (filter-contains-cnf-clause f (first (get mcs i)))]
          (recur (assoc mcs i (disj (get mcs i) (first (get mcs i)))) (set/union m w) i)))
      (disj-phi-m f m)))))


(fact
  (filter-contains-cnf '#{#{a b c}})
  => '#{#{a b c}}
  (filter-contains-cnf '#{#{a} #{a b c} #{a b}})
  => '#{#{a}}
   (filter-contains-cnf '#{#{a z} #{a b c}})
  => '#{#{a z} #{a b c}})

(defn cnfs [f]
  (setify-cnf (cnf f)))

(declare estLitteral)

(defn estLitteral [litt?]
  (or (symbol? litt?)
      (and (seq litt?)
           (= (first litt?) 'not))))


(declare dcnf-aux)

(declare dcnf)

(defn dcnf-aux [f equivs]
  (match f
         ([op a b] :seq) (let [[a', equivs1] (dcnf-aux a equivs)
                               [b', equivs2] (dcnf-aux b equivs1)
                               f' (list op a' b')]
                           (if-let [eq (get equivs2 f')]
                             [eq, equivs2]
                             (let [v (symbol (str "$" (inc (count equivs2))))]
                               [v, (assoc equivs2 f' v)])))
         :else [f equivs]))

(defn dcnf [f]
  (let [[y0, aux] (dcnf-aux (nnf f) {})]
    (reduce set/union #{#{y0}} (map #(let [[x1 x2] %]
                                       (cnfs (list '<=> x1 x2))) aux))))




