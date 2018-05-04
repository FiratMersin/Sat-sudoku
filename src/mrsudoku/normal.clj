(ns mrsudoku.normal
   (:require [clojure.core.match :refer [match]])
  (:use midje.sweet)
  (:require [clojure.set :as set]))


;; EXERCICE : ajouter  l'implication et l'équivalence dans tout ce qui suit

'(==> a b)
'(<=> a b)


;;; ## Simplifications des formules

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

;;; ## Forme normale NNF

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

;;; ## Forme normale disjonctive CNF

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

(cnf '(or (and a (or b c)) (and (not a) (not c))))


(defn setify-or [f]
  (match f
    (['or a b] :seq)
    (set/union (setify-or a) (setify-or b))
    :else #{f}))



(setify-or '(or a (or a (or (not b ) (not b)))))



(defn setify-cnf [f]
  (match f
    (['and a b] :seq) (set/union (setify-cnf a) (setify-cnf b))
    (['or a b] :seq) #{(setify-or f)}
    :else #{#{f}}))


(setify-cnf (cnf '(==> (or a b) (<=> c d))))




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


(defn filter-trivial-cnf [f];; f est un ensemble d'ensembles
  (loop [f f, resf #{}]
    (if (seq f)
      (if (filter-trivial-one (first f))
        (recur (rest f) resf)
        (recur (rest f) (conj resf (first f))))
      resf)))


(filter-trivial-cnf #{#{(symbol (str "x" 2 "y" 4 "b" 3)) (symbol (str "x" 2 "y" 3 "b" 0)) (symbol (str "x" 2 "y" 3 "b" 4))}
                      #{(symbol (str "x" 2 "y" 3 "b" 3)) (symbol (str "x" 2 "y" 3 "b" 2)) (list 'not (symbol (str "x" 2 "y" 3 "b" 2)))}})


(declare cnfs)

(defn cnfs [f]
  (filter-trivial-cnf (setify-cnf (cnf f))))


(declare estLitteral)

(defn estLitteral [litt?]
  (or (symbol? litt?)
      (= litt? true)
      (and (seq litt?)
           (= (first litt?) 'not))))

(declare dcnf-aux)

(defn dcnf-aux [f]
  (if (estLitteral f)
    [f, f]
    (let [[conn, gauche, droite] f
          [vgauche, g'] (dcnf-aux gauche)
          [vdroite, d'] (dcnf-aux droite)
          v (gensym "o")]
      [v, (list 'and (cnf (list '<=> v (list conn vgauche vdroite)))
                (list 'and g' d'))])))

(declare dcnf)

(defn dcnf [f]
  (let [[v, f'] (dcnf-aux f)]
    (list 'and v f')))

















