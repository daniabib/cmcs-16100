-- | A module for working with triangles.

module Hypotenuse where

-- | Compute the length of the hypotenuse of a triangle from the lengths
--   of its sides.

hypotenuse :: Double -> Double -> Double
hypotenuse a b = sqrt (square a + square b)

-- | Square a number.

square :: Num n => n -> n
square x = x ^ 2

lawOfCosines :: Floating a => a -> a -> a -> a
lawOfCosines a b gamma = sqrt(square a + square b - 2 * a * b * cos (degreesToRadians gamma))

degreesToRadians :: Floating n => n -> n
degreesToRadians x = x * pi / 180