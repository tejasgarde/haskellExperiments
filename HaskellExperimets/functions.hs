-- Tejas Garde
-- www.github.com/tejasgarde @tejas_garde
-- All below programs are experimental and should be used for learning purpose only
-- This file should be load in GHCi

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



