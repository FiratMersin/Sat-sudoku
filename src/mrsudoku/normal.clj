(ns mrsudoku.normal
   (:require [clojure.core.match :refer [match]])
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
    ;;(==> true true) -> true
    (['==> true true] :seq) true
    ;;(==> true false) -> false
    (['==> true false] :seq) false
    ;;(==> false a) -> true
    (['==> false a] :seq) true
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

(simplify-one '(not true))
(simplify-one '(not false))
(simplify-one '(not (not true)))
(simplify-one '(not (or true (not false))))

(defn simplify [f]
  (match f
    ([op a] :seq) (simplify-one (list op (simplify a)))
    ([op a b] :seq) (simplify-one (list op (simplify a)
                                          (simplify b)))
    :else f))

(simplify '(or (not (or (not true)
                        (and (or (not x) false)
                             (or (not false) x))))
               (not (or y (and false z)))))

(simplify '(not (or true (not false))))

(simplify '(==> (or true (not false)) false))

(simplify '(<=> (not (or true (not false))) false))

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

(nnf' '(not (or true (not false))))
(nnf' '(or (not (or (not true)
                    (and (or (not x) false)
                         (or (not false) x))))
           (not (or y (and false z)))))

(nnf' '(==> (or a b) (<=> c d)))

(simplify '(or (not (or (not true)
                    (and (or (not x) false)
                         (or (not false) x))))
           (not (or y (and false z)))))

(simplify (nnf' '(or (not (or (not true)
                              (and (or (not x) false)
                                   (or (not false) x))))
                     (not (or y (and false z))))))




(defn nnf [f]
  (nnf' (simplify f)))


(nnf '(==> (or a b) (<=> c d)))

;;; ## Forme normale disjonctive DNF

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

;; Remarque : f doit être en NNF
(defn dnf' [f]
  (match f
    (['and a b] :seq) (distrib (list 'and (dnf' a)
                                          (dnf' b)))
    (['or a b] :seq) (list 'or (dnf' a)
                               (dnf' b))
    :else f))

(defn dnf [f]
  (dnf' (nnf f)))

(dnf '(and (or a (and b c)) (or (not a) (not c))))

(dnf '(==> (or a b) (<=> c d)))

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

;;; Problème : c'est pas lisible et c'est simplifiable
;;; Solution : représentation sous forme d'ensemble (conjonctif) de clauses (disjonctives)

(defn setify-and [f]
  (match f
    (['and a b] :seq)
    (set/union (setify-and a) (setify-and b))
    :else #{f}))

(defn setify-or [f]
  (match f
    (['or a b] :seq)
    (set/union (setify-or a) (setify-or b))
    :else #{f}))

(setify-and '(and a (and a (and (not b ) (not b)))))

(setify-or '(or a (or a (or (not b ) (not b)))))

(defn setify-dnf [f]
  (match f
   (['and a b] :seq) #{(setify-and f)}
   (['or a b] :seq) (set/union (setify-dnf a) (setify-dnf b))
   :else #{#{f}}))

(defn setify-cnf [f]
  (match f
    (['and a b] :seq) (set/union (setify-cnf a) (setify-cnf b))
    (['or a b] :seq) #{(setify-or f)}
    :else #{#{f}}))


(setify-dnf
  '(or (or (and a (not a)) (and a (not c))) (or (and (and b c) (not a)) (and (and b c) (not c)))))

(setify-dnf (dnf '(==> (or a b) (<=> c d))))

(setify-cnf (cnf '(==> (or a b) (<=> c d))))

(nnf '(==> (or a b) (<=> c d)))

;; EXERCICE : retirer les clauses qui contiennent un litéral et sa négation
;; fonction :  filter-trivial

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


;; EXERCICE : si on a une clause C1 incluse dans une clause C2
;; (par exemple: #{a (not b)}   et  #{a (not b) c})
;; alors on retire la plus grande C2 ..
;; fonction :  filter-subsume

(defn get-map-litt-signe [clause]
  (loop [clause clause, m {}]
    (if (seq clause)
      (let [[litt, signe] (if (symbol? (first clause))
                           [(first clause), :positif]
                           [(second (first clause)), :negatif])]
        (recur (rest clause), (assoc m litt signe)))
      m)))

(defn containsall? [clause m]
  (loop [clause clause]
    (if (seq clause)
      (let [[litt, signe] (if (symbol? (first clause))
                           [(first clause), :positif]
                           [(second (first clause)), :negatif])]
        (if (nil? (get m litt))
          false
          (if (= signe (get m litt))
            (recur (rest clause))
            false)))
      true)))


(defn filter-subsume-aux [clause1 clause2];;clause1 de taille >= à clause2
  (let [m (get-map-litt-signe clause1)]
    (if (containsall? clause2 m)
      true
      false)))





(defn filter-subsume-cnf [f]
  (let [size (count f)]
  (loop [ftmp f, resf #{}, n 1]
    (if (= size n)
      resf
      nil))));;à terminer!!!!!!!!!!!





;; EXERCICE : en déduire une fonction dnfs qui prend une
;; formule quelconque et retourne la formule DNF simplifiée représentée par des ensembles



;; En déduire une fonction :  cnfs prend une
;; formule quelconque et retourne la formule CNF simplifiée représentée par des ensembles
(declare cnfs)

(defn cnfs [f]
  (filter-trivial-cnf (setify-cnf (cnf f))))


(declare estLitteral)

(defn estLitteral [litt?]
  (or (symbol? litt?)
      (= (first litt?) 'not)))

(declare dcnf-aux)

(defn dcnf-aux [f]
  (if (estLitteral f)
    [f, f]
    (let [[conn, gauche, droite] f
          [vgauche, g'] (dcnf-aux gauche)
          [vdroite, d'] (dcnf-aux droite)
          v (gensym "x")]
      [v, (list 'and (cnf (list '<=> v (list conn vgauche vdroite)))
                (list 'and g' d'))])))

(declare dcnf)

(defn dcnf [f]
  (let [[v, f'] (dcnf-aux f)]
    (list 'and v f')))













  (dcnf (cnf (or (and true (symbol (str "x" 2 "y" 4 "b" 2)))
               (and (or (symbol (str "x" 2 "y" 4 "b" 1))  (symbol (str "x" 2 "y" 4 "b" 0)))
               (or (symbol (str "x" 2 "y" 4 "b" 1)) (list 'not (symbol (str "x" 2 "y" 4 "b" 1))))))))






















