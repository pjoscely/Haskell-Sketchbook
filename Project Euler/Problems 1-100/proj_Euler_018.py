
'''
Maximum path sum I
 
Problem 18
By starting at the top of the triangle below and moving to adjacent numbers on the row below, 
the maximum total from top to bottom is 23.

                    3
                   7 4
                  2 4 6
                 8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

                                  75
                                 95 64
                                17 47 82
                               18 35 87 10
                              20 04 82 47 65
                            19 01 23 75 03 34
                           88 02 77 73 07 63 67
                          99 65 04 28 06 16 70 92
                         41 41 26 56 83 40 80 70 33
                        41 48 72 33 47 32 37 16 94 29
                       53 71 44 65 25 43 91 52 97 51 14
                      70 11 33 28 77 73 17 78 39 68 17 57
                     91 71 52 38 17 14 91 43 58 50 27 29 48
                    63 66 04 68 89 53 67 30 73 16 69 87 40 31
                   04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route. 
However, Problem 67, is the same challenge with a triangle containing one-hundred rows; 
it cannot be solved by brute force, and requires a clever method! ;o)
'''
small_tri = [[3],[7,4],[2,4,6],[8, 5, 9, 3]]

big_tri = [[75],[95,64],[17,47,82],[18,35,87,10],[20,4,82,47,65],[19,1,23,75,3,34],
           [88,2,77,73,7,63,67],[99,65,4,28,6,16,70,92],[41,41,26,56,83,40,80,70,33],
           [41,48,72,33,47,32,37,16,94,29],[53,71,44,65,25,43,91,52,97,51,14],
           [70,11,33,28,77,73,17,78,39,68,17,57],
           [91,71,52,38,17,14,91,43,58,50,27,29,48],
           [63,66,4,68,89,53,67,30,73,16,69,87,40,31],
           [4,62,98,27,23,9,70,98,73,93,38,53,60,4,23]]

def dec_to_bin(x):
    return format(x, '014b')

max = -1
for i in range(2**14):
    path = dec_to_bin(i)
    row_index = 1
    sum = 75
    curr_index = 0
    for p in path:
        if p == '1':
             curr_index+=1
        sum = sum+big_tri[row_index][curr_index]
        row_index+=1
    if(sum > max):
        max = sum
print(max)
#1074

'''
Congratulations, the answer you gave to problem 18 is correct.
You are the 142242nd person to have solved this problem.
'''    
