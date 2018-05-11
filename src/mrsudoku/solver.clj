(ns mrsudoku.solver
  (:use midje.sweet)
   (:require
     [mrsudoku.normal :as nform]
     [mrsudoku.dplls :as ds]
     [mrsudoku.sat.encode :as encode]
     [seesaw.core :refer [text! invoke-later]]
     [mrsudoku.grid :as g]))

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

(fact
  (get-val-cell '{x1y1b3 false x1y1b2 true x1y1b1 true x1y1b0 true} 1 1)
  => 7)


(defn get-res [mdpll grid];;prend en entrée le résultat d'un dpll et une grille et retourne la grille solution
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

(defn maj-grid [ctrl oldgrid resgrid];;affiche la grille solution
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


(defn solve [ctrl]
  (try
    (let [grid (:grid (deref ctrl))]
      (let [res (ds/dpll (nform/dcnf (nform/nnf
         (encode/encode-sudoku grid))) )]
        (if res
          (maj-grid ctrl grid (get-res res grid))
          (println "Pas de solution pour cette grille"))))
    (catch Exception e (println "Pas de solution pour cette grille"))))


