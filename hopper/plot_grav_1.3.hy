(import
  os
  matplotlib [pyplot]
  math
  functools [reduce]
  matplotlib.patches [Rectangle]
  numpy)

(pyplot.rcParams.update {"font.size" 14})
(pyplot.style.use "ggplot")
(pyplot.rcParams.update {"figure.autolayout" True})

;; Estimate initial velocity from first two points
(defn estimate-vi [points]
  (let [first-point (get points 0)
        second-point (get points 1)
        delta-y (- (get second-point 1) (get first-point 1))
        delta-x (- (get second-point 0) (get first-point 0))]
    (/ delta-y delta-x)))

(defn cmfin [centimeters]
  (* centimeters 2.54))


; Recorded points for test n=1
(setv points [[
    [0 0]
    [0.25 (cmfin 31)]
    [0.5 (cmfin 41)]
    [0.75 (cmfin 26.25)]
    [1 (cmfin 0)]
]])

(setv g 9.81) ; m/s^2
(setv initial_h 0) ; initial height in meters
(setv vi (estimate-vi (get points 0))) ; initial velocity in m/s

(setv t_start 0)
(setv all-xs [])
(for [pts points]
  (for [p pts]
    (.append all-xs (get p 0))))
(setv t_end (max all-xs))
(setv t_step (/ t_end 200))
(setv vi_cm (* vi 100)) ; initial velocity in cm/s

(defn round [num decimals]
  (let [factor (math.pow 10 decimals)]
    (/ (math.floor (+ (* num factor) 0.5)) factor)))

(defn gravity-at-time [time-seconds vi ih]
    (let [initial-height ih]
        (+ initial-height
         (* vi time-seconds)
         (* -0.5 g (math.pow time-seconds 2)))))

;; range of floats
(defn rangef [start end step]
  (let [out []
        cur start]
    (while (< cur end)
      (.append out cur)
      (setv cur (+ cur step)))
    out))


(defn cmtom [centimeters]
  (/ centimeters 100))

(defn mtocm [meters]
  (* meters 100))

(defn max [values]
  (reduce (fn [a b] (if (> a b) a b)) values))

(defn conju [args]
  (.join "" (map str args)))

(defn plot-adv [all-coeffs points t-start t-end t-step xlabel ylabel title description outpath xlim ylim]
  (setv times (rangef t-start t-end t-step))
  (let [[fig ax] (pyplot.subplots :figsize (tuple [8 5]))]
    (pyplot.subplots_adjust :bottom 0.2)
    (let [fit-colors ["blue" "cyan" "magenta" "yellow" "black"]
          i 0]
      (for [coeffs all-coeffs]
        (let [fit-values []]
          (for [t times]
            (.append fit-values (eval-parabola coeffs t)))
          (ax.plot times fit-values :color (get fit-colors i) :linestyle "--" :linewidth 2 :label (conju ["Fit Parabola " (+ i 1)]))
          (setv i (+ i 1)))))
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
  (.append displacements_cm (mtocm (gravity-at-time t vi initial_h))))

;; Convert times and displacements into point pairs for fitting
(setv point-pairs (list (zip times_s displacements_cm)))

;; Function derived from algorithm "Least Squares Fitting of Data Points to a Parabola"
;; Source: https://mathworld.wolfram.com/LeastSquaresFitting.html

(defn add [a b]
  (+ a b))

(setv all-coeffs [])
(for [pts points]
  (setv px (list (map (fn [p] (get p 0)) pts)))
  (setv py (list (map (fn [p] (get p 1)) pts)))
  (.append all-coeffs (numpy.polyfit px py 2)))

(defn eval-parabola [coeffs x]
  (+ (* (get coeffs 0) x x) (* (get coeffs 1) x) (get coeffs 2)))

(setv all-ys [])
(for [pts points]
  (for [p pts]
    (.append all-ys (get p 1))))
(setv ylim [0 (max all-ys)])

(setv xlim [t_start t_end])

(plot-adv
    all-coeffs
    points
    t-start
    t-end
    t-step
    "Time (s)"
    "Displacement (cm)"
    "Projectile Motion"
    "A projectile (n=2) is launched vertically upward. The displacement over time is shown with a fitted parabolic curve."
    (conju [outdir "/f1.3_plot.png"])
    xlim
    ylim)