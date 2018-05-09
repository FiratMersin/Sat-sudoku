(ns mrsudoku.solver
   (:require
     [mrsudoku.normal :as nform]
     [mrsudoku.dplls :as ds]
     [mrsudoku.sat.encode :as encode]
     [seesaw.core :refer [text! invoke-later]]
     [mrsudoku.grid :as g]))

(def grid-to-solve @#'g/sudoku-grid3)

(defn get-val-cell [mdpll x y];;retourne la valeur de la case [x y] depuis la map résultat d'un dpll
  (let [bo (get mdpll (symbol (str "x" x "y" y "b" 0)))
        b1 (get mdpll (symbol (str "x" x "y" y "b" 1)))
        b2 (get mdpll (symbol (str "x" x "y" y "b" 2)))
        b3 (get mdpll (symbol (str "x" x "y" y "b" 3)))
        res0 (if bo
                1
                0)
        res1 (if b1
               (+ 2 res0)
               res0)
        res2 (if b2
               (+ 4 res1)
               res1)
        resf (if b3
               (+ 8 res2)
               res2)]
      resf))

(defn get-res [mdpll grid];;prend en entrée le résultat d'un dpll et une grille
  (loop [x 1 y 1 rgrid grid]
    (if (= 10 x)
      rgrid
      (if (= :empty (get (g/cell rgrid x y) :status))
        (if (= 9 y)
          (recur (inc x) 1 (g/change-cell rgrid x y {:status :set, :value (get-val-cell mdpll x y)}))
          (recur x (inc y) (g/change-cell rgrid x y {:status :set, :value (get-val-cell mdpll x y)})))
        (if (= 9 y)
          (recur (inc x) 1 rgrid)
          (recur x (inc y) rgrid))))))

(defn get-clause-size1 [f];;retourne une formule contenant seulement les clauses de taille 1 de f
  (loop [f f, res #{}]
    (if (empty? f)
      res
      (if (= 1 (count (first f)))
        (recur (rest f) (conj res (first f)))
        (recur (rest f) res)))))



(defn filter-dcnf-res-map  [m clause]
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

(defn filter-dcnf-res [f]; retire les paires de clauses A,B résultat d'un dcnf avec A = (not B)
  (loop [f f m {} fres #{}]
    (if (seq f)
      (if (and (= 1 (count (first f))) (not= \o (first (name (ds/get-var (ffirst f))))))
        (recur (rest f) (filter-dcnf-res-map m (first f)) fres)
        (recur (rest f) m (conj fres (first f))))
      (loop [m m fres fres]
        (if (seq m)
          (let [[x signe] (first m)]
            (if (= :suppr signe)
              (recur (rest m) fres)
              (if (= :negatif signe)
                (recur (rest m) (conj fres #{(list 'not x)}))
                (recur (rest m) (conj fres #{x})))))
          fres)))))




(defn affiche-res [m grid]; affiche le résultat
  (do (println "Solution :")
  (loop [x 1, y 1]
    (if (> 10 x)
      (let [toprint (if (= :empty (get (g/cell grid y x) :status))
                      (str "["(get-val-cell m y x)"]")
                       (str " "(get-val-cell m y x)" "))]
      (if (= 9 y)
        (do (println toprint)
          (recur (inc x) 1))
        (do (print toprint)
          (recur x (inc y)))))))))


(defn maj-grid [ctrl oldgrid resgrid]
  (loop [x 1, y 1]
    (if (> 10 x)
      (do
        (if (= :empty (get (g/cell oldgrid x y) :status))
          (do
            (let [cell-widget ((resolve 'mrsudoku.control/fetch-cell-widget) ctrl x y)]
            ((resolve 'mrsudoku.control/cell-validate-input!) ctrl cell-widget x y (g/cell-value (g/cell resgrid x y)))
            (invoke-later (text! cell-widget (g/cell-value (g/cell resgrid x y)))))))
        (if (= 9 y)
          (recur (inc x) 1)
          (recur x (inc y)))))))


(defn solve [ctrl grid]
  (let [res (ds/dpll (nform/filter-contains-cnf (filter-dcnf-res (nform/setify-cnf (nform/dcnf (nform/nnf
     (encode/encode-sudoku grid)))))))]
    (if (not= res false)
      (maj-grid ctrl grid (get-res res grid))
      (println "Pas de solution pour cette grille"))))

