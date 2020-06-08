--Perfect number is equal to sum of its divisors; excluding the number itself
--6 = 1+2+3
--28 = 1+2+4+7+14
--It is not known whether there are any odd perfect numbers,
---nor whether infinitely many perfect numbers exist.
perfectfactors :: Int -> [Int]
perfectfactors x = filter (\y->(x `mod` y == 0)) [1..x-1]

perfectNums :: [Int]
perfectNums = filter(\x-> (sum(perfectfactors x) ==x))[1..10000]

--[6,28,496,8128], takes awhile to find; next number is 33550336
--https://oeis.org/A000396

--Below is a series of filter expressions to calculate an infinite list of prime numbers.
--Best to run with a finite argument like [2..1000] instead of [2..]
--inorder to avoid the program running endlessly
properfactors :: Int -> [Int]
properfactors x = filter (\y->(x `mod` y == 0)) [2..(x-1)]

numproperfactors :: Int -> Int
numproperfactors x = length (properfactors x)

primes :: [Int]
primes = filter (\x-> (numproperfactors x == 0)) [2..1000]


--Standard slow fibonacci number generator
slow_fib :: Int -> Integer
slow_fib 0 = 0
slow_fib 1 = 1
slow_fib n = slow_fib (n-2) + slow_fib (n-1)

--Fast fibonacci with momization
memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)

--Calculate 'e'
--e = 1+ 1/1! + 1/2! + 1/3! + 1/4! + 1/5! 
--Set up factorial
fac :: Float -> Float
fac 0 = 1
fac 1 = 1
fac n = n*fac(n-1)

--Helper reciprocal function
rec :: Float -> Float
rec n = 1/n

--Reciprocal of factorial
rec_fac:: Float -> Float
rec_fac n = (rec.fac) n

--Compute e
compute_e :: Float
compute_e = sum(map rec_fac [0..20])

--Calculation of 'pi' as :-
--pi = (4/1) - (4/3) + (4/5) - (4/7) + (4/9) ...
--Too slow in practice
--pi_term:: Int -> Float
sign :: Int-> Int
sign n = if n `rem` 2 == 0 then 1 else (-1)
pi_term :: Int -> Float
pi_term n = fromIntegral (sign n) *(4.0/(2.0*(fromIntegral n)+1))
compute_pi :: Float
compute_pi = sum(map pi_term [0..2000000])



