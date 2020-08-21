/*
Magic 5-gon ring
https://projecteuler.net/problem=68
Problem 68
*/
include "alldifferent.mzn";

var 1..10: E1;   
var 1..10: E2;  
var 1..10: E3;
var 1..10: E4;   
var 1..10: E5;  
var 1..10: E6;
var 1..10: E7;   
var 1..10: E8;
var 1..10: E9;
var 1..10: E10;   

var 6..27: S;

constraint E1+E2+E3 = S;
constraint E4+E3+E5 = S;
constraint E6+E5+E7 = S;
constraint E8+E7+E9 = S;
constraint E10+E9+E2 = S;

constraint E1 > 5;
constraint E4 > 5;
constraint E6 > 5;
constraint E8 > 5;
constraint E10 > 5;

constraint alldifferent([E1,E2,E3,E4,E5,E6,E7,E8,E9,E10]);

solve satisfy;
output ["\(E1)\(E4)\(E6)\(E8)\(E10)\n",
        "\(E1)\(E2)\(E3)\(E4)\(E3)\(E5)\(E6)\(E5)\(E7)\(E8)\(E7)\(E9)\(E10)\(E9)\(E2)"];
/*
Possible solutions from running the code

6531031914842725 is the largest

876109
8427256531031914
----------
678910
6357528249411013
----------
910678
9411013635752824
----------
610987
6531031914842725
----------
789106
7528249411013635
----------
987610
9148427256531031
----------
106789
1013635752824941
----------
761098
7256531031914842
----------
891067
8249411013635752
----------
109876
1031914842725653
----------
*/

