sqr :: Double -> Double
sqr x = x * x

vec2DLen :: (Double, Double) -> Double
vec2DLen (x, y) = sqrt(x^2 + y^2)

vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x, y, z) = sqrt(x^2 + y^2 + z^2)

swap :: (Int, Char) -> (Char, Int)
swap (a, b) = (b, a)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x, y, z) = (x == z && x == y && z == y)

calculateArea :: (Double, Double, Double) -> Double
calculateArea (a, b, c) = sqrt(((a + b + c) / 2) * (((a + b + c) / 2) - a) * (((a + b + c) / 2) - b) * (((a + b + c) / 2) - c))

