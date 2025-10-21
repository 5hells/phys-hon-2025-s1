(import numpy
        matplotlib [pyplot]
        math
        os
        functools [reduce]
        matplotlib.patches [Rectangle])

(pyplot.rcParams.update {"font.size" 14})
(pyplot.style.use "ggplot")
(pyplot.rcParams.update {"figure.autolayout" True})

(defn mtocm [meters seconds]
  (* meters 100))

(defn eval-parabola [coeffs t]
  (let [a (get coeffs 0)
        b (get coeffs 1)
        c (get coeffs 2)]
    (+ c (* b t) (* a (math.pow t 2)))))

(defn cmfin [centimeters]
  (* centimeters 2.54))

(setv points [[
    [0 0]
    [0.25 (cmfin 26)]
    [0.5 (cmfin 41)]
    ; 0.75 out of frame
    [1 0]
] [
    [0 0]
    [0.25 (cmfin 31)]
    [0.5 (cmfin 41)]
    [0.75 (cmfin 26.25)]
    [1 (cmfin 0)]
] [
    [0 0]
    [0.25 (cmfin 8.75)]
    [0.5 (cmfin 31.25)]
    [0.75 (cmfin 43)]
    [1 0]
] [
    [0 0]
    [0.25 (cmfin 27)]
    [0.5 (cmfin 43)]
    [0.75 (cmfin 37)]
    [1 0]
] [
    [0 0]
    [0.25 (cmfin 24.5)]
    [0.5 (cmfin 43.5)]
    [0.75 (cmfin 37)]
    [1 0]
]])

(setv g 9.81) ; m/s^2

; estimate average parabola for all tests

(defn estimate-vi [points]
  (let [first-point (get points 0)
        second-point (get points 1)
        delta-y (- (get second-point 1) (get first-point 1))
        delta-x (- (get second-point 0) (get first-point 0))]
    (/ delta-y delta-x)))

(setv initial_h 0) ; initial height in meters
(setv vi_list [])
(for [pts points]
  (setv vi (estimate-vi pts)) ; initial velocity in m/s
  (.append vi_list vi))

(defn add [a b]
  (+ a b))

(defn rangef [start end step]
  (let [out []
        cur start]
    (while (< cur end)
      (.append out cur)
      (setv cur (+ cur step)))
    out))

(setv vi_avg (/ (reduce add vi_list) (len vi_list)))

(setv t_start 0)
(setv all-xs [])
(for [pts points]
  (for [p pts]
    (.append all-xs (get p 0))))
(setv t_end (max all-xs))
(setv t_step (/ t_end 200))
(setv vi_cm (* vi_avg 100)) ; initial velocity in cm/s


(defn max [values]
  (reduce (fn [a b] (if (> a b) a b)) values))

(defn conju [args]
  (.join "" (map str args)))

(defn plot-adv [avg-coeffs points t-start t-end t-step xlabel ylabel title description outpath xlim ylim]
  (setv times (rangef t-start t-end t-step))
  (let [[fig ax] (pyplot.subplots :figsize (tuple [8 5]))]
    (pyplot.subplots_adjust :bottom 0.2)
    (let [fit-values []]
      (for [t times]
        (.append fit-values (eval-parabola avg-coeffs t)))
      (ax.plot times fit-values :color "black" :linestyle "-" :linewidth 3 :label "Average Fit Parabola"))
    (let [colors ["red" "green" "orange" "purple" "brown"]
          i 0]
      (for [pts points]
        (let [point-xs (list (map (fn [p] (get p 0)) pts))
              point-ys (list (map (fn [p] (get p 1)) pts))]
          (ax.scatter point-xs point-ys :color (get colors i) :s 40 :label (conju ["Test n=" (+ i 1)]) :zorder 5)
          (setv i (+ i 1)))))
    (ax.legend)
    (ax.set_xlabel xlabel)
    (ax.set_ylabel ylabel)
    (ax.set_title title)
    (ax.grid True)
    (ax.text 0.5 -0.25 description
      :horizontalalignment "center"
      :verticalalignment "bottom"
      :style "italic" :transform ax.transAxes)
    (when xlim (ax.set_xlim xlim))
    (when ylim (ax.set_ylim ylim))
    (fig.savefig outpath)))

(setv outdir "figout/hopper")
(when (not (os.path.exists outdir))
  (os.makedirs outdir))

(setv times_s (rangef t_start t_end t_step))
(setv displacements_cm [])
(for [t times_s]
  (setv displacement_cm (+ (* vi_cm t) (* 0.5 (- g) (mtocm t t))))
  (.append displacements_cm displacement_cm))

(setv all-coeffs [])
(for [pts points]
  (setv px (list (map (fn [p] (get p 0)) pts)))
  (setv py (list (map (fn [p] (get p 1)) pts)))
  (.append all-coeffs (numpy.polyfit px py 2)))

(setv avg-a (/ (reduce add (map (fn [c] (get c 0)) all-coeffs)) (len all-coeffs)))
(setv avg-b (/ (reduce add (map (fn [c] (get c 1)) all-coeffs)) (len all-coeffs)))
(setv avg-c (/ (reduce add (map (fn [c] (get c 2)) all-coeffs)) (len all-coeffs)))
(setv avg-coeffs [avg-a avg-b avg-c])

(setv all-ys [])
(for [pts points]
  (for [p pts]
    (.append all-ys (get p 1))))
(setv ylim [0 (max all-ys)])

(plot-adv avg-coeffs points t_start t_end t_step
  "Time (s)" "Displacement (cm)"
  "Average Hopper Displacement over Time"
  "Projectile Motion"
  (conju [outdir "/f1.7_plot.png"])
  [t_start t_end] ylim)