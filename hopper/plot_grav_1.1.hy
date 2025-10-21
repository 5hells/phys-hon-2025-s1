(import
  os
  matplotlib [pyplot]
  math
  functools [reduce]
  matplotlib.patches [Rectangle])

(pyplot.rcParams.update {"font.size" 14})
(pyplot.style.use "ggplot")

(setv m-magic 39.3700787) ; inches per meter

(setv weight-grams 9.5) ; weight of pop-up toy in grams
(setv g 9.81) ; m/s^2

(defn gravity-at-time [time-seconds vi ih]
  (let [initial-height ih]
    (+ initial-height
       (* vi time-seconds)
       (* -0.5 g (math.pow time-seconds 2)))))

(defn round [num decimals]
  (let [factor (math.pow 10 decimals)]
    (/ (math.floor (+ (* num factor) 0.5)) factor)))

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

(defn plot-adv [f t-start t-end t-step xlabel ylabel title description outpath xlim ylim]
  (setv times (rangef t-start t-end t-step))
  (setv values [])
  (for [t times]
    (.append values (f t)))
  (let [[fig ax] (pyplot.subplots :figsize (tuple [8 5]))]
    (pyplot.subplots_adjust :bottom 0.2)
    (ax.plot times values)
    (ax.set_xlabel xlabel)
    (ax.set_ylabel ylabel)
    (ax.set_title title)
    (ax.grid True)
    (ax.text 0.5 -0.25 description
      :horizontalalignment "center"
      :verticalalignment "bottom"
      :style "italic" :transform ax.transAxes)
    (ax.add_patch (Rectangle [0.5 0.95] 0.1 0.05
      :transform ax.transAxes
      :color "white"
      :alpha 0.8
      :zorder 2))
    (ax.text 0.55 1 (conju [(round (max values) 2) "cm"])
      :horizontalalignment "center"
      :verticalalignment "top"
      :transform ax.transAxes
      :zorder 3)
    (when xlim (ax.set_xlim xlim))
    (when ylim (ax.set_ylim ylim))
    (fig.savefig outpath)))

; plotting parameters
(setv initial_h (cmtom 0)) ; initial height in meters
(setv vi_cm 300) ; initial velocity in cm/sec
(setv vi (/ vi_cm 100)) ; convert cm/s to m/s

; time of flight: when y = 0 (returns to ground)
(setv t_end (/ (* 2 vi) g))
(setv t_start 0)
(setv t_step (/ t_end 200))

(setv outdir "figout/hopper")
(when (not (os.path.exists outdir))
  (os.makedirs outdir))

(setv times_s (rangef t_start t_end t_step))
(setv displacements_cm [])
(for [t times_s]
  (.append displacements_cm (mtocm (gravity-at-time t vi initial_h))))

(plot-adv
  (fn [t] (mtocm (gravity-at-time t vi initial_h)))
  t_start t_end t_step
  "Time (s)"
  "Y Displacement (cm)"
  (conju ["Gravity on " weight-grams "g object at " vi_cm " cm/s"])
  (conju ["A " weight-grams "g object is launched upwards at " vi_cm " cm/s (" (cmtom vi_cm) " m/s)."])
  (os.path.join outdir "f1.1_plot.png")
  [t_start t_end]
  [(+ -0.5 (min displacements_cm)) (+ 0.5 (max displacements_cm))])