module Gold where

-- The golden ratio
phi :: Double
phi = (sqrt 5 + 1) / 2

polynomial :: Double -> Double
polynomial x = x^2 - x - 1

-- f x = polynomial (polynomial x)

quadratic a b c = ((-(b) + sqrt (b^2 - 4 * a * c)) / (2 * a),(-b - sqrt (b^2 - 4 * a * c)) / (2 * a))


price product = if product == "milk" then 3 else 2


increment x = x+1

f 0 = 1
f 1 = 1
f x = if x < 0 then 0 else x * f(x-1)


compute x = let a = x+1
            in a
main = do
  print (phi)
  print (polynomial phi)
  print (f phi)