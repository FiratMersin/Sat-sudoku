(ns solver
   (:require
     [mrsudoku.normal :as nform]
     [mrsudoku.dplls :as ds]
     [mrsudoku.sat.encode :as encode]
     [mrsudoku.grid :as g]))

(def grid-to-solve @#'g/sudoku-grid3)

(defn get-sym-string [s]
  (if (seq? s)
    (name (second s))
    (name s)))

(defn get-val-cell [mdpll x y]
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




grid-to-solve


(def res (ds/dpll (nform/setify-cnf (nform/dcnf (nform/nnf (encode/encode-sudoku grid-to-solve))))))

(def gridsolved (get-res res encode/ex-grille))
(get res (symbol "x4y4b3"))
(get-val-cell res 1 1)

(get-val-cell res 4 4)
(get-val-cell res 5 4)
(get-val-cell res 6 4)

(get-val-cell res 4 5)
(get-val-cell res 5 5)
(get-val-cell res 6 5)

(get-val-cell res 4 6)
(get-val-cell res 5 6)
(get-val-cell res 6 6)


gridsolved
