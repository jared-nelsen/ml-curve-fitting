(ns ml-curve-fitting.Animation
  (:require [quil.core :as q]
            [ml-curve-fitting.Evaluation :as evaluation]
            [ml-curve-fitting.Data :as data]))

;; Currently only supports animation for parallel evolution

(def curveDrawingPointInterval 0.1)

(defn getFittestBCurveInAlgorithmContext
  "Gets the fittest B Curve out of the given Algorithm Context."
  [context]
  (let [population (get context :population)
        fittestIndex (get context :indexOfFittestMember)]
    (get population fittestIndex)))

(defn getFittestBCurveInPopulationOfAlgorithmContexts
  "Retrieves the fittest B Curve within the given population of
   algorithm contexts."
  [contexts]
  (loop [contexts contexts
         fittestBCurve (getFittestBCurveInAlgorithmContext (first contexts))
         bestFitness (get fittestBCurve :fitness)]
    (if (empty? contexts)
      fittestBCurve
      (let [currentContext (first contexts)
            fittestBCurveInContext (getFittestBCurveInAlgorithmContext currentContext)
            currentFitness (get fittestBCurveInContext :fitness)]
        (if (< currentFitness bestFitness)
          (recur (rest contexts)
                 fittestBCurveInContext
                 currentFitness)
          (recur (rest contexts)
                 fittestBCurve
                 bestFitness))))))

(def animationContent
  (atom {:pointsToFit []
         :controlPointVector []
         :bCurvePointVector []}))

(defn updateP
  [algorithmContexts]
  (let [x (count algorithmContexts)
        dataToFit (get (first algorithmContexts) :data) ;;Constant across contexts
        fittestCurve (getFittestBCurveInPopulationOfAlgorithmContexts algorithmContexts)
        controlPoints (get fittestCurve :controlPointVector)
        bCurvePointVector (evaluation/generateBCurvePointsOnIntervalP controlPoints
                                                                     curveDrawingPointInterval)]
    (do (swap! animationContent assoc
               :pointsToFit (:points dataToFit)
               :controlPointVector controlPoints
               :bCurvePointVector (:bCurvePointVector bCurvePointVector))
        algorithmContexts)))

;; Drawing
(def windowSizeXY 1000)
(def xyScaleFactor 0.5) ;; % of window size
(defn scalePoint
  [point]
  (let [scalePx (int (* windowSizeXY xyScaleFactor))
        x (+ (:x point) scalePx)
        y (+ (:y point) scalePx)]
    (assoc point :x x :y y)))

(defn drawPointsToFit
  "Draws the data that is being fit with the algorithm."
  []
  (loop [pointsToFit (get @animationContent :pointsToFit)]
    (if (not-empty pointsToFit)
      (let [point (scalePoint (first pointsToFit))
            x (:x point)
            y (:y point)]
        (q/fill 0 120 255)
        (q/ellipse x y 10 10)
        (recur (rest pointsToFit)))
      nil)))

(defn drawControlPointVector
  "Draws the control point vector."
  []
  (loop [controlPointVector (get @animationContent :controlPointVector)]
    (if (not-empty controlPointVector)
      (let [point (scalePoint (first controlPointVector))
            x (:x point)
            y (:y point)]
        (q/fill 0 204 0)
        (q/ellipse x y 10 10)
        (recur (rest controlPointVector)))
      nil)))

(defn drawBCurve
  "Draws the actual generated B Curve."
  []
  nil)

(defn resetBackground
  "Resets the background of the sketch by drawing a new sqare over it."
  []
  (q/fill 0 0 0)
  (q/rect 0 0 windowSizeXY windowSizeXY))

(defn draw
  []
  (q/background 255)
  (drawPointsToFit)
  (drawControlPointVector))
  
(q/defsketch bCurveAnimation
  :title "Test"
  :settings #(q/smooth 2)
  :setup nil
  :draw draw
  :size [windowSizeXY windowSizeXY])
