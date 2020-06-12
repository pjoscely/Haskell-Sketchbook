-- Lesson 12. Creating your own types
-- After reading lesson 12, you’ll be able to

-- Define type synonyms to clarify code
-- Create your own data type
-- Build types from other types
-- Work with complex types by using record syntax

-- Warm up function to be refactored below
patientInfo :: String -> String -> Int -> Int -> String
patientInfo fname lname age height = name ++ " " ++ ageHeight
 where name = lname ++ ", " ++ fname
       ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"


-- Type synonyms: FirstName, LastName, Age, and Height

type FirstName = String
type LastName = String
type Age = Int
type Height = Int

type PatientName = (String,String)

-- Create a few helper functions to get the first and last name of the patient.

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient

-- Define Sex type

data Sex = Male | Female

-- Defining the sexInitial function
sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

-- Rewrite patientInfo to use your patientName type, reducing the total arguments 
-- needed to three instead of four.
patientInfoV2 :: PatientName -> Int -> Int -> String
patientInfoV2 (fname,lname) age height = name ++ " " ++ ageHeight
 where name = lname ++ ", " ++ fname
       ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

-- Model blood type.

data RhType = Pos | Neg

data ABOType = A | B | AB | O

-- Combining ABOType and RhType to create BloodType

data BloodType = BloodType ABOType RhType


-- Create BloodType

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

-- Displaying your types: showRh, showABO, showBloodType
showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh)  = showABO abo ++ showRh rh

-- Example 
-- showBloodType patient3BT
-- "AB+"


-- A function canDonateTo to determine whether one BloodType can donate to another.

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False --otherwise

-- Example 
-- canDonateTo patient1BT patient2BT
-- False
-- canDonateTo patient2BT patient1BT
-- True
-- canDonateTo patient2BT patient3BT
-- True
-- canDonateTo patient1BT patient3BT
-- True
-- canDonateTo patient3BT patient1BT
-- False

-- Support different names: MiddleName and Name
type MiddleName = String
data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName

-- Displaying multiple constructors: showName
showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

-- Example 
name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"


-- GHCi> showName name1
-- "Jerome Salinger"
-- GHCi> showName name2
-- "Jerome David Salinger"


-- clunky Patient1 version 1
data Patient1 = Patient1 Name Sex Int Int Int BloodType

johnDoe :: Patient1
johnDoe = Patient1 (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

janeESmith :: Patient1
janeESmith = Patient1 (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 28 62 140 (BloodType AB Neg)

-- cumbersome functions to get each value by using pattern matching.

getName :: Patient1 -> Name
getName (Patient1 n _ _ _ _ _) = n

getAge :: Patient1 -> Int
getAge (Patient1  _ _ a _ _ _) = a

getBloodType :: Patient1 -> BloodType
getBloodType (Patient1 _ _ _ _ _ bt) = bt



-- Defining a new data type by using record syntax makes it much easier to understand 
-- which types represent which properties of the data type.

data Patient2 = Patient2 { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType }

jackieSmith :: Patient2
jackieSmith = Patient2 {name = Name "Jackie" "Smith"
                      , age = 43
                      , sex = Female
                      , height = 62
                      , weight = 115
                      , bloodType = BloodType O Neg }

-- In addition, you don’t have to write your getters; each field 
-- in the record syntax automatically creates a function to access that value from the record:

-- GHCi> height jackieSmith
-- 62
-- GHCi> showBloodType (bloodType jackieSmith)
-- "O-"
-- GHCi> showName (name jackieSmith)
-- "Jackie Smith"

jackieSmithUpdated = jackieSmith { age = 44 }

-- GHCi> age jackieSmithUpdated 
-- 44

-- Write a function similar to canDonateTo that takes two patients 
-- as arguments rather than two BloodTypes.

donorFor :: Patient2 -> Patient2 -> Bool
donorFor p1 p2 = canDonateTo (bloodType p1) (bloodType p2)


-- Implement a patientSummary function that uses your final Patient type. 
-- patient-Summary should output a string that looks like this:

-- **************
-- Patient Name: Smith, John
-- Sex: Male
-- Age: 46
-- Height: 72 in.
-- Weight: 210 lbs.
-- Blood Type: AB+
-- **************

showSex Male = "Male"
showSex Female = "Female"

patientSummary :: Patient2 -> String
patientSummary patient = "**************\n" ++
                         "Sex: " ++ showSex (sex patient) ++ "\n" ++
                         "Age: " ++ show (age patient) ++ "\n" ++
                         "Height: " ++ show (height patient) ++ " in.\n" ++
                         "Weight: " ++ show (weight patient) ++ " lbs.\n" ++
                         "Blood Type: " ++ showBloodType (bloodType patient) ++
                         "\n**************\n"

-- patientSummary jackieSmith 
-- "**************\nSex: Female\nAge: 43\nHeight: 62 in.\nWeight: 115 lbs.\nBlood Type: O-\n**************\n"

