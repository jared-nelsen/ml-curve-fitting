# ML Curve Fitting

Curve fitting with Genetic Algorithms.

Uses Bezier Curves to fit data points on a graph.
This experiment was to determine if there was a more efficient way to fit curves on a 2d plan than evolving high-degree polynomials.
Ultimately the evaluation function was quite expensive and defeated any reasonably large data set.

## Evaluation

   - M evenly spaced points will be selected between [0 -> 1]
   - The explicit formula for a point on a Bezier curve will be used to calculate a
     point for each M
           - https://github.com/nashvail/BezierCurveGenerator/blob/master/main.js
