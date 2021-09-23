{-
-- Project Euler
-- https://projecteuler.net/
'''
Digit cancelling fractions 
Problem 33
The fraction 49/98 is a curious fraction, as an inexperienced mathematician 
in attempting to simplify it may incorrectly believe that 49/98 = 4/8, 
which is correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, 
less than one in value, and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms, 
find the value of the denominator.
'''
-- ***********************************************************
-- Python solution was completed first
# get string intersection 
def get_int(str1,str2):
    res = "" 
    for i in str1: 
        if i in str2 and not i in res: 
            res += i 
    if (len(res)==1):
        return res
    else:
        return ""
fd = []
#Build list of 2 digit numbers not ending in zero
for i in range (11, 100):
    if(i % 10 != 0):
        fd.append(str(i))

#Test all fractions 11/12, 11/13,...,97/99,98/99
for i in range(0, len(fd)):
    for j in range(i+1,len(fd)):
        inter = get_int(fd[i],fd[j])
        if len(inter)== 1:
            d1 = int(fd[i])
            d2 = int(fd[j])
            q1 = d1/d2
            
            new_d1 = fd[i].replace(inter, '', 1) 
            new_d2 = fd[j].replace(inter, '', 1) 
            
            q2 = int(new_d1)/int(new_d2)
            if(q1 == q2):
                print(fd[i]+"/"+fd[j],inter, new_d1+"/"+new_d2)
'''    
16/64 6 1/4
19/95 9 1/5
26/65 6 2/5
49/98 9 4/8
  
Congratulations, the answer you gave to problem 33 is correct.

You are the 69873rd person to have solved this problem.  
'''  
-} 
-- ************************************************************
-- Haskell solution
import System.IO  
import qualified Data.Set as Set

-- Helper function
-- char2float '2' ->2.0
char2float :: Char -> Float
char2float n = fromInteger (read [n])

-- Helper tuple functions used below

get1st :: (a, b, c, d) -> a
get1st (a,_,_,_) = a

get2nd :: (a, b, c, d) -> b
get2nd (_,a,_,_) = a

get3rd :: (a, b, c, d) -> c
get3rd (_,_,a,_) = a

get4th :: (a, b, c, d) -> d
get4th (_,_,_,a) = a

-- Build list of all 2 digit numbers not ending in zero
-- [11,12,13,14,15,16,...,94,95,96,97,98,99]
lst_2d :: [Integer]
lst_2d = [n|n<-[11..99], n `mod ` 10 /= 0]

-- Convert lst_2d to Strings
-- ["11","12","13",...,,"97","98","99"]
lst_2d_string :: [String]
lst_2d_string = map show lst_2d

-- Use the four forms below to build  candidate lists

-- ax/bx = a/b 
-- [("11","21",'1','2'),("11","31",'1','3'),("11","41",'1','4')...]
lst_1 :: [(String, String, Char, Char)]
lst_1 = [(x,y,head x, head y)| x<- lst_2d_string ,  y<- lst_2d_string, x<y, (last  x == last y), (head  x /= head y)]  

-- ax/xb = a/b
-- [("11","12",'1','2'),("11","13",'1','3'),("11","14",'1','4')...]
lst_2 :: [(String, String, Char, Char)]
lst_2 = [(x,y, head x, last y)| x<- lst_2d_string ,  y<- lst_2d_string, x<y, (last  x == head y), (head  x /= last y) ]  

-- xa/xb = a/b
-- [("11","12",'1','2'),("11","13",'1','3'),("11","14",'1','4')...]
lst_3 :: [(String, String, Char, Char)]
lst_3 = [(x,y,last x, last y)| x<- lst_2d_string ,  y<- lst_2d_string, x<y, (head  x == head y), (last  x /= last y) ]  

-- xa/bx = a/b
-- [("11","21",'1','2'),("11","31",'1','3'),("11","41",'1','4')...]
lst_4 :: [(String, String, Char, Char)]
lst_4 = [(x,y,last x, head y)| x<- lst_2d_string ,  y<- lst_2d_string, x<y, (head  x == last y), (last x /= head y)]  

-- Filter the four lists to find solutions

-- Empty list
sol_1 :: [(String, String, Char, Char)]
sol_1 = [x| x<- lst_1,  (read(get1st x)::Float)/(read(get2nd x)::Float) == (char2float$get3rd x)/(char2float$get4th x)]

-- Here are the four solutions
-- [("16","64",'1','4'),("19","95",'1','5'),("26","65",'2','5'),("49","98",'4','8')]
sol_2 :: [(String, String, Char, Char)]
sol_2 = [x| x<- lst_2,  (read(get1st x)::Float)/(read(get2nd x)::Float) == (char2float$get3rd x)/(char2float$get4th x)]

-- Empty list
sol_3 :: [(String, String, Char, Char)]
sol_3 = [x| x<- lst_3,  (read(get1st x)::Float)/(read(get2nd x)::Float) == (char2float$get3rd x)/(char2float$get4th x)]

-- Empty list
sol_4 :: [(String, String, Char, Char)]
sol_4 = [x| x<- lst_4,  (read(get1st x)::Float)/(read(get2nd x)::Float) == (char2float$get3rd x)/(char2float$get4th x)]

-- main = 100
main :: IO ()
main = do  
    putStrLn$show (4*5*5)




