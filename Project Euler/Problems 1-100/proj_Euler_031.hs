{-
Project Euler
https://projecteuler.net/
'''
Coin sums
  
Problem 31
In the United Kingdom the currency is made up of pound (£) and pence (p). 
There are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p), and £2 (200p).
It is possible to make £2 in the following way:

1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
How many different ways can £2 be made using any number of coins?
'''
***************************************************************************************
Python solution first

#Dictionary to store coin values ith coin has value coin_dict[i]
coin_dict = {1:1, 2:2,3:5, 4:10, 5:20, 6:50, 7:100,8:200}

#initialize an empty (m+1)x(nx1) grid  with 0(s)
m=8
n=200
grid = [[0]*(n+1) for i in range(m+1)]
    
#Fill first column with ones
for i in range(m+1):
    grid[i][0] = 1
    
 #Fill first row with amounts
for j in range(n+1):
    grid[0][j] = j

 #Fill first row with ones 
for j in range(1,n+1):
    grid[1][j] = 1   
    

#displays the grid for debugging
def printGrid(grid):
    for i in range(m+1):
        for j in range(n+1):
            print(grid[i][j], end =" ")
        print()
    print()

'''   
The initial grid
[[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20...], 
 [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...], 
 [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...], 
 [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...], 
 [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...], 
 [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...], 
 [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...], 
 [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...], 
 [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...]]


Dynamic programming solution:

'''
for i in range(2,m+1):
    for j in range(1,n+1):
        if j-coin_dict[i]>=0:
            grid[i][j] = grid[i-1][j]+grid[i][j-coin_dict[i]]
        else:
            grid[i][j] = grid[i-1][j]

#Solution value
print(grid[8][200]) 

'''
Congratulations, the answer you gave to problem 31 is correct.

You are the 82973rd person to have solved this problem.      
'''
-}
-- ***************************************************************************
-- Haskell Solution 
-- Coin values in pence, add value 0 to match Python solution above
coin_list :: [Integer]
coin_list = [0, 1, 2, 5, 10, 20, 50, 100, 200]

-- Computes j-coin_list[i]
diff :: Integral a => Int -> a -> Integer
diff i j = fromIntegral j - coin_list!!i

-- Define Dynamic Programming grid function
grid 0 j = j
grid _ 0 = 1
grid 1 _  = 1
grid i j = if diff i j >=0 then grid (i-1) j+ grid i (diff i j) else grid (i-1) j

-- Compute solution
answer :: Integer
answer = grid 8 200

-- main -> *******
--- (0.19 secs, 59,119,248 bytes)
main :: IO ()
main = do  
    putStrLn$show answer




