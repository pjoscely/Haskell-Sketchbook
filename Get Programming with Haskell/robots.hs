
-- Fighting Robots OOP Haskell Project

--A robot will have some basic properties:

-- A name
-- An attack strength
-- A number of hit points


-- killerRobot = robot ("Kill3r",25,200)
robot :: (a, b, c) -> ((a, b, c) -> t) -> t
robot (name,attack,hp)  = \message -> message (name,attack,hp)

--  helper functions
-- use as 
-- getName aRobot = aRobot name
-- getAttack aRobot = aRobot attack
-- getHP aRobot = aRobot hp

name :: (a, b, c) -> a
name (n,_,_) = n


attack :: (a, b, c) -> b
attack (_,a,_) = a


hp :: (a, b, c) -> c
hp (_,_,hp) = hp


-- getName, getAttack, and getHP accessors

-- killerRobot = robot ("Kill3r",25,200)

-- killerRobot name --> "Kill3r"
getName :: (((a, b, c) -> a) -> t) -> t
getName aRobot = aRobot name

-- killerRobot attack --> 25
getAttack :: (((a, b, c) -> b) -> t) -> t
getAttack aRobot = aRobot attack

-- killerRobot hp --> 200
getHP :: (((a, b, c) -> c) -> t) -> t
getHP aRobot = aRobot hp

-- In Haskell, you can create new objects by modifying copies of old, existing ones:

-- killerRobot = robot ("Kill3r",25,200)
--nicerRobot = setName killerRobot "kitty"
setName:: (((a1, b, c) -> ((a2, b, c) -> t1) -> t1) -> t2) -> a2 -> t2
setName aRobot newName = aRobot (\(n,a,h) -> robot (newName,a,h))


-- killerRobot = robot ("Kill3r",25,200)
-- gentlerRobot = setAttack killerRobot 5
setAttack:: (((a, b1, c) -> ((a, b2, c) -> t1) -> t1) -> t2) -> b2 -> t2
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n,newAttack,h))


-- killerRobot = robot ("Kill3r",25,200)
-- softerRobot = setHP killerRobot 50
setHP:: (((a, b, c1) -> ((a, b, c2) -> t1) -> t1) -> t2) -> c2 -> t2
setHP aRobot newHP = aRobot (\(n,a,h) -> robot (n,a,newHP))


-- One more nice function would be to print all your robot’s stats. 
-- define a printRobot message that works much like a toString method in other languages.
-- printRobot killerRobot
-- "Kill3r attack:25 hp:200"

-- printRobot nicerRobot
-- "kitty attack:25 hp:200"

-- printRobot gentlerRobot
-- "Kill3r attack:5 hp:200"

-- printRobot softerRobot
-- "Kill3r attack:25 hp:50"
printRobot:: (Show a1, Show a2) => ((([Char], a1, a2) -> [Char]) -> t) -> t
printRobot aRobot = aRobot (\(n,a,h) -> n ++
                                        " attack:" ++ (show a) ++
                                        " hp:"++ (show h))



-- With the damage message, you can tell a robot that it has taken damage:


-- killerRobot = robot ("Kill3r",25,200)
-- printRobot$damage killerRobot 100
-- "Kill3r attack:25 hp:100"
damage:: Num c =>(((a, b, c) -> ((a, b, c) -> t1) -> t1) -> t2) -> c -> t2
damage aRobot attackDamage = aRobot (\(n,a,h) -> robot (n,a,h-attackDamage))


-- Now it’s time to fight! This is your first case of having one object interact with another, 
-- so you’re doing some real OOP now. Your fight message is going to be the mainstream OOP 
-- equivalent of the following:  robotOne.fight(robotTwo)

-- ****** Example *********
-- gentleGiant = robot ("Mr. Friendly", 10, 300)

-- killerRobot = robot ("Kill3r",25,200)

-- gentleGiantRound1 = fight killerRobot gentleGiant
-- killerRobotRound1 = fight gentleGiant killerRobot
-- gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
-- killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
-- gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
-- killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2

-- printRobot gentleGiantRound3
-- "Mr. Friendly attack:10 hp:225"
-- printRobot killerRobotRound3
-- "Kill3r attack:25 hp:170"


fight
  :: (Ord a1, Num a1) =>
     (((a2, b1, b1) -> b1) -> a1)
     -> (((a3, b2, a1) -> ((a3, b2, a1) -> t1) -> t1) -> t2) -> t2

fight aRobot defender = damage defender attack
  where attack = if getHP aRobot > 10
                 then getAttack aRobot
                 else 0



-- fastRobot = robot ("speedy", 15, 40)
-- slowRobot = robot ("slowpoke",20,30)

-- Order has no importance in execution of Haskell code

-- Any bugs that might come up because of the order in which the functions 
-- have been written are much less common in Haskell. Because you can control 
-- exactly when and how state is modeled, there are no mysteries at all 
-- in how the code is executed.

-- This robot fight could happen in any order, and the results are the same.

-- slowRobotRound1 = fight fastRobot slowRobot
-- fastRobotRound1 = fight slowRobotRound1 fastRobot
-- slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
-- fastRobotRound2 = fight fastRobotRound1 slowRobotRound1
-- slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
-- fastRobotRound3 = fight slowRobotRound3 fastRobotRound2


-- fastRobotRound3 = fight slowRobotRound3 fastRobotRound2
-- fastRobotRound2 = fight slowRobotRound2 fastRobotRound1
-- fastRobotRound1 = fight slowRobotRound1 fastRobot
-- slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
-- slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
-- slowRobotRound1 = fight fastRobot slowRobot

-- printRobot fastRobotRound3
-- "speedy attack:15 hp:20"
-- printRobot slowRobotRound3
-- "slowpoke attack:20 hp:-15"

fastRobot = robot ("speedy", 15, 40)
slowRobot = robot ("slowpoke",20,30)

fastRobotRound3 = fight slowRobotRound3 fastRobotRound2
fastRobotRound2 = fight slowRobotRound2 fastRobotRound1
fastRobotRound1 = fight slowRobotRound1 fastRobot
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
slowRobotRound1 = fight fastRobot slowRobot



-- speedy attack:15 hp:20
-- slowpoke attack:20 hp:-15
main :: IO ()
main = do
       putStrLn (printRobot fastRobotRound3)
       putStrLn (printRobot slowRobotRound3)


