import Data.List
import Data.Ord
data Ex = Ex Float Float String String deriving Show 
data NewSt = NewSt Float Float String deriving Show 
data Dist = Dist Float NewSt Ex deriving Show

-------------------------------------------------------------------------- euclidean--------------------------------------------------------------------------
-- euclidean function uses m1 m2 (corresponds to midterm Newst and midterm EX ),same for q1 and q2 (corresponds to midterm Newst and midterm EX ) 
-- to calculate the  euclidean equation
euclidean :: NewSt -> Ex -> Dist
euclidean (NewSt m1 q1 c) (Ex m2 q2 y z)=Dist (sqrt(((m1-m2)^2)+((q1-q2)^2))) (NewSt m1 q1 c) (Ex m2 q2 y z) 


-------------------------------------------------------------------------- manhattan--------------------------------------------------------------------------
-- manhatten function uses m1 m2 (corresponds to midterm Newst and midterm EX ),same for q1 and q2 (corresponds to midterm Newst and midterm EX ) 
-- to calculate the  manhatten equation
manhattan :: NewSt -> Ex -> Dist
manhattan (NewSt m1 q1 c) (Ex m2 q2 y z)=Dist ((abs (m1-m2) )+(abs (q1-q2))) (NewSt m1 q1 c) (Ex m2 q2 y z) 

----------------------------------------------------------------------------------dist------------------------------------------------------------------------
-- dist function takes a distance metric function (f) (euclidean/manhattan) which takes an input point (p1) and output another function which takes the second input point (p2)
dist :: (a -> b -> c) -> a -> b -> c
dist f p1 p2 = f p1 p2

-------------------------------------------------------------------------- all_dists--------------------------------------------------------------------------
--Base CAse is that the list is empty we return empty list.
-- all_dists function uses the dist function (which takes a distance metric function (f) (euclidean/manhattan) which takes the NewSt as point .... 
-- and the ouput functions takes the first Ex in the list and return the Dist object,then we countine the recursive call.
all_dists :: (a -> b -> c) -> a -> [b] -> [c]
all_dists f x []=[]
all_dists f x (y:ys)= dist f x y:all_dists f x ys

-------------------------------------------------------------------------- takeN------------------------------------------------------------------------------
--We have to base cases 1) the list is empty and we rturn an empty list.
--                      2) the number is zero and we rturn an empty list.
--Recursive call :we keep taking the elements until the (n) is bigger than zero,so every time we enter the recusive call we decrement the number (n) until it reaches zero.
takeN :: Num a => a -> [b] -> [b]
takeN _ []=[]
takeN 0 l =[]
takeN n (x:xs)= x:takeN (n-1) xs


-------------------------------------------------------------------------- closest-----------------------------------------------------------------------------
--Base Case :we have empty list and return empty list
--Recursive Call :First calls the all_dists function to supply our function with the list of Dist Objects,Second we sort them using the imported sortBy using .....
-- ....sortGT function (see description below) and finally we take the first n elements by using takeN function.
closest::Num a => (b -> c -> Dist) -> a -> [c] -> b -> [Dist]
closest f n [] y=[]
closest f n l y=takeN n (sortBy sortGT (all_dists f y l))

--sortGT takes a Dist object as input and return a function which takes another Dist object and return a value (comparing the float d1 with d2)
sortGT::Dist->Dist->Ordering
sortGT (Dist d1  a  b) (Dist d2  c  d ) 
										|d1>d2=GT    --if second Dist object is greater than the first Dist object returns GT (greater than)
										|d1<d2=LT    --if second Dist object is less than the first Dist object returns GT (less than)
										|otherwise=EQ --if both float numbers are equal


-------------------------------------------------------------------------- grouped_dists------------------------------------------------------------------------
-- grouped_dists uses closest function to get the n closest Dist objects ,then uses imported groupBy function to group them into two different Dist objects
grouped_dists :: Num a => (b -> c -> Dist) -> a -> [c] -> b -> [[Dist]]
grouped_dists f n (x:xs) y=groupBy group2 (closest f n (x:xs) y) 

-- group2 function takes a Dist object as input and return a function which takes another Dist object and return a bool value if both z1 and z2 are ...
-- .... equal (could be "pass"or"fail")then they are grouped into one list and if z1 and z2 are different then they belong to different lists.
group2 (Dist a b (Ex w x y z1)) (Dist a2 b2 (Ex w2 x2 y2 z2))= z1==z2

-------------------------------------------------------------------------- mode----------------------------------------------------------------------------------
-- mode function uses grouped_dists to supply our function with list containing two lists,then we use the imported maxiumBy and the helper function ......
-- ......is also imported from Data.Ord which compares the length between the two lists and return the greater list.
mode :: Num a => (b -> c -> Dist) -> a -> [c] -> b -> [Dist]
mode f n (x:xs) y= maximumBy (mycomparing_length) (grouped_dists f n (x:xs) y)

mycomparing_length :: [Dist] -> [Dist] -> Ordering
mycomparing_length list1 list2 
					 | length list1> length list2 =GT
					 | length list1< length list2 =LT
					 | otherwise=EQ

-------------------------------------------------------------------------- label_of------------------------------------------------------------------------------
--First Base Case:empty list returns an error
--Second base Case: takes one Dist object and return the name of the NewSt(c) and the label of EX (z)
label_of :: [Dist] -> ([Char],[Char]) 
label_of []=error "empty Dist"
label_of ((Dist n (NewSt a b c) (Ex w x y z)):xs)= (c,z)

-------------------------------------------------------------------------- classify ------------------------------------------------------------------------------
-- classify function uses mode to get the Dist list (which is the most repeated label) and then get the name and the label from the returned list
classify :: Num a => (b -> c -> Dist) -> a -> [c] -> b -> ([Char],[Char])
classify f n (x:xs) y= label_of (mode f n (x:xs) y )

-------------------------------------------------------------------------- classify_all --------------------------------------------------------------------------
--Base Case:empty list return empty list
--Recrsive Call: calls the classify function to get the name and label of the first NewSt only,then we countine calling again with the rest NewSt until ......
-- ....the list becomes empty
classify_all :: (a -> b -> Dist) -> Int -> [b] -> [a] -> [([Char],[Char])]
classify_all f n (x:xs) []=[]
classify_all f n (x:xs) (y:ys)= (classify f n (x:xs) y):classify_all f n (x:xs) ys