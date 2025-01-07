roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e)
    where d = sqrt (b * b - 4 * a * c)
          e = 2 * a

unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (a, b) = (a/u, b/u)
    where u = sqrt(a^2 + b^2) 

calculateArea :: (Double, Double, Double) -> (Double)
calculateArea (a, b, c) = sqrt(p*(p - a) * (p - b) * (p - c))
    where p = 1/2 * (a + b + c)

