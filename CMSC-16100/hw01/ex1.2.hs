-- <Alex Miller>
-- Exercise 1.1

-- | A module for working with triangles. 

module Hypotenuse where

-- | Compute the length of the hypotenuse of a triangle from the lengths
--   of its sides.

hypotenuse :: Double -> Double -> Double
hypotenuse a b = sqrt (square a + square b)

-- | Square a number.

square :: Num n => n -> n
square x = x ^ 2

-- | Find the third side of a triangele given two sides and the angle at the vertex of those two sides

law_of_cosines :: Double -> Double -> Double -> Double
law_of_cosines a b gamma = 
 let
  x = square a
  y = square b
  z = 2*a*b*cos(degree_convert gamma)
 in
 sqrt (x + y - z)
-- | Convert a degree to a radian

degree_convert :: Double -> Double
degree_convert theta = theta/(360)*2*pi
