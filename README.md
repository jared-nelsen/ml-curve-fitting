# ML Curve Fitting

Curve fitting with Genetic Algorithms

## Construct

   - Central Construct is called BezierCurve
   - Bezier Curve has a fitness value and the vector of control points
   - Control Points are a vector of Records of x and y coordinates
   - Generate a random set of control points

## Data
   
   - Data to compare against is a vector of Records of x and y coordinates
   - Randomly generate Data to solve
   - Global defs for length of data
   - Max and Min for X and Y magnitudes of points
## Mutation

   1. Go through the control points and make minor random adjustments to their x y position
   2. Randomly add a control point
   3. Randomly remove a control point

## Crossover

   [[x y] [x y] [x y]] <> [[x y] [x y] [x y]]

   - Crossover strategy is that for two candidates we will step through the control points
     in order and cross over their x and y coordinates.
   - Stop at the length of the shorter candidate

## Evaluation

   - M evenly spaced points will be selected between [0 -> 1]
   - The explicit formula for a point on a Bezier curve will be used to calculate a
     point for each M
           - https://github.com/nashvail/BezierCurveGenerator/blob/master/main.js