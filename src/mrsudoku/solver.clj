(ns solver
   (:require
     [mrsudoku.normal :as nform]
     [mrsudoku.dplls :as ds]
     [mrsudoku.sat.encode :as encode]))


(ds/dpll (nform/setify-cnf (nform/cnf (nform/nnf (encode/encode-sudoku encode/ex-grille)))))





