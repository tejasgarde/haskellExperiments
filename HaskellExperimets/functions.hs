-- Tejas Garde
-- www.github.com/tejasgarde @tejas_garde
-- All below programs are experimental and should be used for learning purpose only
-- This file should be load in GHCi

import Data.List
import qualified Data.Map as Map
import qualified Data.Char as Char



-- Comments in haskell
-- below code shows the Pattern matching in haskell
-- It works Just Like Switch statement in Other Languages 
lucky :: Int -> String
lucky 7 = "You Are lucky person"
lucky x = "Better luck Next Time"
-- END

-- factorial in Pattern matching
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial(x - 1)

-- END

-- failing of Pattern matching
charName :: Char -> String
charName 'a' = "You Typed a"
charName 'b' = "You Typed b"
charName 'c' = "You Typed c"

-- if we call charName with any parameter other than a , b,c then this matching will fail
-- END


-- Pattern Matching in Tuples
-- Function that takes two tuples and return  Tuple
addVector :: (Double , Double) -> (Double , Double) -> (Double , Double) 
addVector (x1,y1) (x2,y2) = (x1 + x2 , y1 + y2)
-- END

-- Pattern Matching in List and List Comprehensions
-- Not working in haskell GHCi
-- xs = [(1,2) , (1,2),(1,2),(1,2),(1,2) ] 
-- [a+b | (a,b) <- xs]
-- END 

-- A replication of head function head that picks out head function

head' :: [a] -> a
head [] = error "Sorry empty list is not allowed !!"
head' (x:_) = x

-- Format of list is 1:2:3:[] for list of [1,2,3]
-- END 



-- Guards in Haskell its just like a if statements 

bmiCal ::  Double -> String
bmiCal bmi 
	| bmi <= 18.5 = "You are Underweight"
	| bmi <= 25.0 = "You are Normal"
	| bmi <= 30.0 = "You are overweight"
	| otherwise = "You are whale !!"

-- A guard is indicated by pipe (|) character followed by Boolean Expression


-- max function to find maximum value of two 
max' :: (Ord a) => a -> a -> a
max' a b 
	| a <= b = b
	| otherwise = a

-- Compare two entities
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
	| a == b = EQ
	| a <= b = LT
	| otherwise = GT

-- END


-- Some values can be stored in a temp veriable using where keyword 
-- this veriable is then used in the calculation
myBMICalc :: Double -> Double -> String
myBMICalc weight height
	| bmi <= 18.5 = "You are Underweight !! "
	| bmi <= 25.0 = "You are Normal "
	| bmi <= 30.0 = "You are Overweight !! "
	where bmi = weight/height ^ 2
-- END

-- To declear multiple veriable in where clause we must decelear it globally
-- we use Patter matching here

badGreeting :: String
badGreeting = "Ohh It's You "

niceGreeting :: String
niceGreeting = "Hello !! So very nice to see you "

greet :: String -> String
greet "Jaun" = niceGreeting ++ "Juan !"
greet "Fernando" = niceGreeting ++ "Fernando !!"
greet name = badGreeting ++ name


-- Here we wil use Pattern Matching with here clause
-- Our BMI exampple with where clause

myBMIWithPatterMatch :: Double -> Double -> String

myBMIWithPatterMatch weight height
	| bmi <= skinny = "You are skinny"
	| bmi <= normal = "You are Normal"
	| bmi <= fat = "You are fat"
	where 
		bmi = weight/height ^ 2
		(skinny,normal,fat) =  (18.0,25.0,30.0)




calcInitial :: String -> String -> String
calcInitial firstname lastname = [f] ++ ". " ++ [l] ++ ". "
	where 
		(f:_) = firstname
		(l:_) = lastname

-- END

-- Just like where binding can be used to bind veriable at the end of the function
-- let will let you bind veriable anywhere and is expression itself
-- where is span accors guards whereas let is not
-- syntax is let <bindings> in <expression>
cylinder :: Double -> Double -> Double
cylinder r h =
		let 
			sideArea = 2 * pi * r * h
			topArea = pi * r ^ 2
		in 
			sideArea + 2 * topArea
-- END


-- case expression is as given below 
-- it is similar to pattern matching
-- syntax is
-- case <expression> of <pattern> -> result

caseHead :: [a] -> a
caseHead xs = case xs of 
						[] -> error "Empty List is Not Allowed"
						(x:_) -> x
-- END 


-- Recurssion

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty List is not Allowed"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)


replicate' :: Int -> a -> [a]
replicate' n x
		| n <= 0 = []
		| otherwise = x : replicate' (n-1) x

-- Curied Functions are Function that has Only One Parameter
-- Below Function Type is  Int -> Int -> Int -> Int
-- Which Can Also Be Written Int -> (Int -> (Int -> Int))
-- function takes a value of type Int and returns a function of type (Int -> (Int -> Int)

multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

-- Below Function Will Call The multThree With 9 As one Fixed Parameter 


multTwoWithNine :: Int -> Int -> Int
multTwoWithNine x y = multThree 9 x y

-- Here (multThree 9) is A Function That Takes 2 parameters
--END


-- Sections

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)


isUpperAlphaNum :: Char -> Bool
isUpperAlphaNum = (`elem` ['A'..'Z'])

-- Higer Order Function 
-- Below We will use Functions That Take another Function as Parameter

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

--END


-- Implementing zipWith
--zipWith' (+) [1,2,3] [4,5,6]
-- > [5,7,9]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' fun (x:xs) (y:ys) = fun x y : zipWith' fun xs ys


-- END


-- Replication Of Map Function
--funct1 funct2 List
--	result  funtion applued to each element of the list
-- (+3) [1,2,3,4]
-- [4,5,6,7]


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- END

-- Filter Function
-- predicate is a function that tells whether something is true or false 
-- that is, a function that returns a Boolean value
-- The filter function takes a predicate and a list, and returns the list of elements
-- that satisfy that predicate
-- general Formal filter (predicate Function) -> List -> List that satisfy the predicate

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
	| f x = x : filter' f xs
	| otherwise = filter' f xs


-- END


-- More Example on Map 
-- Programme to Find largest Number below 10k Divisible By 2829

largestDivisible :: Integer
largestDivisible = head' (filter' p [10000,9999..]) 
				where p x = x `mod` 3829 == 0	 



-- Lambdas
-- Lambdas are anonymous funtion , they are actully expression which can be passed as parameter
-- In blow example we will call map' function with Lambda that will receive 1 parameter

--map' (\x -> x + 3) [1,2,3,4]   Commented for Compilation issue

-- This Will call Map' function by passsing anonymous function that starts with \ and list of parameters
-- END


-- Fold function 
-- foldl 
-- fold function will accumulate a list to get one value from all the elements of the list 
-- the computation logic of the accumulation is present in lambda expression
-- in below example we are summing up all the values of the list 
-- Hence Summing logic is present in lambda ecpression passed to it

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- END

-- foldr function
map'' :: (a -> b ) -> [a] -> [b]
map'' f xs = foldr(\x acc -> f x : acc) [] xs


-- in above function the Folding starts from Right  and Proceed to left 
-- Differenece is the the Accumulator starts accumulating the values from thr Right of the list and 
-- the order of thr parameter of the anonymous function is reversed

-- END


-- Function Application with $ 	
-- $ has low precidence than the normal function applcation its just Function Application
-- Normal function application has left associative like f a b c is same as (((f a) b ) c)
-- Function appication with $ has Right associativity
-- Ex sum (filter (>10) (map (*2) [1..20]))
-- Above function will sum up all values in list of 1 to 20 which
-- are multiplied by 2 and the filter out that are greater than 10
-- Same can be written by using $ as
-- sum $ filter (>10) $ map (*2) [1..20]
-- So Every parenthesis Pait for Function Application is replace by $ and code is more clean


-- Modules in Haskell
--import Data.List
numUnique :: (Eq a) => [a] -> Int
numUnique = length . nub
-- \xs -> length (nub xs).

-- To import specific functios of the module use
--  import Data.List (nub, sort)

-- To filter out some function if the module we can use
-- import Data.List hiding (nub)

-- when server function name are same then we use the following syntax
--    import qualified Data.Map as M
-- now to call filter function of Data.Map usr M.filter


wordMap :: String -> [(String, Int)]
wordMap = map' (\ws -> (head' ws, length ws)) . group . sort . words



-- Key value in Haskell 
-- a Simple Map in haskell can be a lIst of Tuple which are nothing but a Key Value Pairs

-- phoneNumbers = [("Tejas",1234),("Tejas2",12346),("Tejas3",45435)]

--findByKey :: (Eq k) => k -> (k,v) -> v
--findByKey key xs = head' . filter (\(k,v) -> key == k) $ xs


-- Use haskell Preddefined Maps using import qualified Data.Map as Map


-- if the key is not available then empty tuple will throw error

findByKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findByKey' key [] = Nothing
findByKey' key ((k,v):xs)
		| key == k = Just v
		| otherwise = findByKey' key xs

-- Now we will use Data.Char module to Convert the list of Char (String) to convert it into INt List
-- We will Use Data.Char as Char as loaded above and use its Functions isDigit which will verify the 
-- Char element is Int or not and Eliminate rest chars like - , etc 
-- after filtering it we will Pass this list to Map with function digitToIn which will do the conversion from
-- Char to Int


string2Digit :: String -> [Int]
string2Digit str = map Char.digitToInt $ filter' Char.isDigit str

-- string2Digit "123345-243-345234"
-- > [1,2,3,3,4,5,2,4,3,3,4,5,2,3,4]


-- Making our own Type and Classes 

-- First way to Create Type is by Using Type
-- Ex to create the Bool Type 
-- data Bool = True | False
-- Read as Bool is a type with Value as True or False 
-- Similarly for Int Type 
-- data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647


-- Vlaue Constructor (Ctor) For our own Shape
--data Shape = Circle Float Float Float | Rectangle Float Float Float Float
--	deriving (Show)
-- :t Circle => Circle :: Float -> Float -> Float -> Shape
-- :t Rectangle => Rectangle :: Float -> Float -> Float -> Float -> Shape

-- Now we will need a Function That Will Take Shape as Generic Param and return Area


--areaOfShape :: Shape -> Float
--areaOfShape (Circle _ _ r) = pi * r * r
--areaOfShape (Rectangle x1 y1 x2 y2) = (abs $ x1 - y1) * (abs $ x2 - y2)


-- *Main> areaOfShape (Circle 10 20 10)
-- 314.15927

-- In above we have Pased the Circle Value to the areaOfShape function and it returned us the Area
-- The Circle Vlaue(Object) is defined as Circle 10 20 10 and in type Matching it Lands in Circle Type


-- FOr Now  if we declear the Circle Type then GHC will throw the error. 
-- TO show it on the Type must be derived from the SHow Type

-- data Shape = Circle Float Float Float | Rectangle FLoat Float Float Float
--		Deriving (Show)




-- improving Shape with Data TYpe point

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- Now the New Area function will be

areaOfShape :: Shape -> Float
areaOfShape (Circle _ r) = pi * r * r
areaOfShape (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

--ghci> area (Rectangle (Point 0 0) (Point 100 100))
-- 10000.0

-- Now we will write a function which will nudge a shape i.e It will return the Shape with points SHifted on x and y axis

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b 
		= Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))



-- Create the Basic SHapes

basicCircle :: Float -> Shape
basicCircle r = Circle (Point 0 0 ) r


basicRect :: Float -> Float -> Shape
basicRect w h = Rectangle (Point 0 0 ) (Point w h)



-- ghci> nudge (baseRect 40 100) 60 23
-- Rectangle (Point 60.0 23.0) (Point 100.0 123.0)

-- exporting function to Modules
-- We can use (..) to export the value Ctor  
-- module Shape 
--( Point (..)
--, Shape (..)
--,area
-- )

-- By using Shape(..), we export all the value constructors for Shape. 
-- This means that people who import our module can make shapes by us- ing the Rectangle and Circle value constructors.
-- It’s the same as writing Shape (Rectangle, Circle), but shorter.
-- Also, if we decide to add some value constructors to our type later on, we don’t need to modify the exports.
-- That’s because using .. automatically exports all value constructors for a given type.




-- Define Person Type

-- data Person = Person String String Int Float String String deriving (Show)

-- tejas = Person "tejas" "Garde" 25 5.6 "Pune" "India"


-- TO define Person Type we will have to create Person instance like above but 
-- Its Difficult to read , But haskell provides a unique way to define the type


data Person = Person { firstName :: String
				,lastName :: String 
				, age :: Int
				, height :: Float
				, phoneNumber :: String 
				, flavor :: String } deriving (Show)	






data Car = Car { company :: String
				, model :: String
				, year :: Int } deriving (Show)



tellCar :: Car -> String
tellCar (Car {company = c, model = m , year = y }) = "This" ++ c ++ " " ++ m ++ "Was Made in " ++ show y 


-- TO create new Type instance we use {} braces.

-- Type Synonyms
-- in Type Synonyms we just modify the new type and provide it a new name 
-- type String = [Char]
-- we Know that the Strings are just a Char List so its easy to refer it as String rather
-- than List of Char

-- Now we will modify the phoneBook 

-- type PhoneBook = [(String,String)]

-- in THis way we Can have New Type as PhoneBook

-- we can also create our won typw for each of the type of Phone book to make it more readable

-- type Name = String
-- type PhoneNumber = String
-- type PhoneBook = [(Name,PhoneNumber)]


data LockerSate = Taken | Free deriving (Show , Eq)

type Code = String

type LockerMap = Map.Map Int (LockerSate , Code)


-- Recursive Data Types

--data List a = Empty | Cons a (List a) deriving (Show,Read,Eq,Ord)
-- ghci> 3 `Cons` (4 `Cons` (5 `Cons` Empty))
-- Cons 3 (Cons 4 (Cons 5 Empty))

-- fixity provides how the operator is binds and then it also defins the operator associativity 
-- fixity of the * operator is like infixir 7 *
-- that is its infix operator and has ficity is 7 which is higher

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)


-- Here we difrine a :-: operator with fixity of 5
-- with the fixity we use the function as the operator
--Infix constructors must begin with a colon


-- Adding Two lIst togather 
-- Heare we are concating the String with the 
infixr 5 +++
(+++) :: [a] -> [a] -> [a]
[]  +++ yy = yy
(x:xs) +++ yy = x : (xs +++ yy)


-- Now we will use the Data Type that we have defined for our List and the operator it 
-- lets create new operator ^++

infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)




data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving(Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree


insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree x EmptyTree = singleton x
insertTree x (Node a left right)
		| x == a = Node x left right
		| x < a = Node x (insertTree x left) right
		| x > a = Node x left (insertTree x right)




































