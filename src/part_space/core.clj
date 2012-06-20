(ns part-space.core
  (:use [quil.core])
  (:gen-class))

(defn gen-bsp
  [x y z wdth hght depth nestings]
  (if (== 0 nestings)
    [x y z wdth hght depth]
    (let [dir (int (rand 3))
          hwdth (/ wdth 2)
          hhght (/ hght 2)
          hdepth (/ depth 2)
          n (dec nestings)]
      (condp == dir
        0 [(gen-bsp x y z hwdth hght depth n)
           (gen-bsp (+ x hwdth) y z hwdth hght depth n)]
        1 [(gen-bsp x y z wdth hhght depth n)
           (gen-bsp x (+ y hhght) z wdth hhght depth n)]
        2 [(gen-bsp x y z wdth hght hdepth n)
           (gen-bsp x y (+ z hdepth) wdth hght hdepth n)])))) 

(defn make-new-sculpture
  []
  (let [bsp (gen-bsp (- (/ (width) 2)) (- (/ (height) 2)) (- (/ (width) 2))
                     (width) (height) (width) 8)]
    (reset! (state :bsp) bsp)))

(defn draw-sculpture
  [bsp]
  (if (== 2 (count bsp))
    (let [pt1 (draw-sculpture (first bsp))
          pt2 (draw-sculpture (second bsp))]
      (stroke 150)
      (line pt1 pt2)
      (vec (map #(/ (+ %1 %2) 2) pt1 pt2)))
    (let [loc   (vec (take 3 bsp))
          dim   (vec (drop 3 bsp))
          trans (vec (map #(+ %1 (/ %2 2)) loc dim))]
      (no-stroke)
      (apply fill (map #(* (abs %) (/ 200 (max (width) (height))))
                       trans))
      (with-translation trans
        (box 10))
      trans)))
    
(defn setup
  []
  (smooth)
  (set-state! :bsp (atom nil)
              :zoom (atom -500))
  (make-new-sculpture))

(defn draw []
  (background 255)
  (with-translation [(/ (width) 2) (/ (height) 2) @(state :zoom)]
    (with-rotation   [(* (mouse-x) (/ (* 2 PI) (width)))  0 1 0]
      (with-rotation [(* (mouse-y) (/ (* 2 PI) (height))) 1 0 0]
        (draw-sculpture @(state :bsp))))))

(defn key-pressed []
  (make-new-sculpture))

(defn mouse-dragged []
  (let [dy (- (mouse-y) (pmouse-y))
        zoom (state :zoom)]
    (swap! zoom #(constrain (+ % dy) -500 200))))

(defn -main [& _]
  (sketch
   :title "parting space"
   :setup setup
   :key-pressed key-pressed
   :mouse-dragged mouse-dragged
   :draw draw
   :renderer :p3d))
