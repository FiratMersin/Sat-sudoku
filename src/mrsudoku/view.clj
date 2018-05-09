
(ns mrsudoku.view
  (:require
   [mrsudoku.grid :as g]
   [mrsudoku.solver :as solv]
   [seesaw.core :refer [frame label text config! grid-panel invoke-later select
                        horizontal-panel vertical-panel button separator action to-frame]]
   [seesaw.border :refer [line-border]]))



(def default-color "white")
(def conflict-color "red")
(def set-color "blue")
(def solved-color "gray")

(defn mk-cell-view
  [cell cx cy ctrl]
  (case (:status cell)
    :init (label :text (str (:value cell))
                 :h-text-position :center
                 :v-text-position :center
                 :halign :center
                 :valign :center
                 :background default-color)
    :empty (let [cell-widget (text :columns 1
                                   :halign :center
                                   :id (keyword (str "cell-" cx "-" cy))
                                   :foreground set-color
                                   :background default-color)]
             (config! cell-widget
                      :listen [:document
                               ;; XXX: normally, we should not depend from the controller
                               ;;      but it's an emblamatic counter-example
                               ((resolve 'mrsudoku.control/cell-input-handler) ctrl cell-widget cx cy)])
             cell-widget)
    (throw (ex-info "Can only build widget for :init or :empty cells." {:cell cell,
                                                                        :cx cx,
                                                                        :cy cy}))))

(defn mk-block-view
  [block bref ctrl]
  (let [cell-widgets (g/reduce-block
                      (fn [widgets _ cx cy cell]
                        (conj widgets (mk-cell-view cell cx cy ctrl))) [] block bref)]
    (grid-panel :rows 3
                :columns 3
                :hgap 3
                :vgap 3
                :border (line-border :thickness 2 :color "black")
                :items cell-widgets
                :id (keyword (str "block-" bref)))))

(defn mk-grid-view [grid ctrl]
  (let [block-widgets (for [i (range 1 10)]
                        (mk-block-view (g/block grid i) i ctrl))]
    (grid-panel :rows 3
                :columns 3
                :border 6
                :hgap 6
                :vgap 6
                :items (into [] block-widgets))))

(defn update-cell-view!
  [cell cell-widget]
  (case (:status cell)
    :conflict (config! cell-widget :background conflict-color)
    (:set :init :empty) (config! cell-widget :background default-color)
    :solved (config! cell-widget :backround solved-color :editable? false)
    (throw (ex-info "Cannot update cell widget." {:cell cell :cell-widget cell-widget}))))

(defn fetch-cell-widget [ctrl cx cy]
  (let [id-widget (keyword (str "#cell-" cx "-" cy))]
    (if-let [cell-widget (select (:grid-widget (deref ctrl)) [id-widget])]
      cell-widget
      (throw (ex-info "Widget not found" {:cell-x cx :cell-y cy})))))

(defn maj-grid [ctrl oldgrid resgrid]
  (loop [x 1, y 1]
    (if (> 10 x)
      (do
        (if (= :empty (get (g/cell oldgrid x y) :status))
           ((resolve 'mrsudoku.control/cell-validate-input!) ctrl (fetch-cell-widget ctrl x y) x y (g/cell-value (g/cell resgrid x y))))
        (if (= 9 y)
          (recur (inc x) 1)
          (recur x (inc y)))))))


(defn mk-main-frame [grid ctrl]
  (let [grid-widget (mk-grid-view grid ctrl)
        main-frame (let [close-action (action
                                        :handler (fn [e] (.dispose (to-frame e)))
                                        :name "Exit"
                                        )]
                          (frame :title "MrSudoku"
                          :content (horizontal-panel
                                    :items [grid-widget
                                            [:fill-h 32]
                                            (vertical-panel
                                             :items [:fill-v
                                                     (grid-panel
                                                      :columns 1
                                                      :vgap 20
                                                      :items [(button :action (action
                                                                                :handler (fn [e] (let [ngrid (solv/solve grid)]
                                                                                                   (if ngrid
                                                                                                   (maj-grid ctrl grid ngrid))
                                                                                                   ))
                                                                                :name "sudoku solver"
                                                                                )
                                                                      :text "Solve")
                                                              (button :action close-action :text "Quit")])
                                                     :fill-v])
                                            [:fill-h 32]])
                          :minimum-size [540 :by 380]
                          :on-close :exit))]
    (swap! ctrl #(assoc % :grid-widget grid-widget :main-frame main-frame))
    main-frame))



