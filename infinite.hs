import Data.List

-- optimized versions of functions to produce infinite list of prime numbers

properfactors' :: Int -> [Int]
properfactors' x = filter (\y -> (x `mod` y == 0)) [2..(x `div` 2)] -- only need to check up to half of x

primes' :: [Int]
primes' = 2 : -- start with 2, which is the only even prime number
  (filter 
    (\x -> null (properfactors' x)) -- using null instead of length prevents traversing the list
    [3,5..] -- only check odd numbers starting with 3
  )

-- original versions of functions from course article

properfactors :: Int -> [Int]
properfactors x = filter (\y -> (x `mod` y == 0)) [2..(x-1)]

numproperfactors :: Int -> Int
numproperfactors x = length (properfactors x)

primes :: [Int]
primes = filter (\x -> (numproperfactors x == 0)) [2..]
