module Practica01 where

--TIPOS ALGEBRAICOS

--Ejercicio 1
data Shape = Circle Float | --representa el radio
            Square Float | --representa un lado
            Rectangle Float Float| --representa base y altura
            Triangle Float | --representa un lado
            Trapeze Float Float Float --representa base mayor, base menor y altura
            deriving (Show, Eq)

--Funcion que calcula el area de las figuras
area :: Shape -> Float
area(Circle r) = pi * r * r
area(Square l) = l * l
area(Rectangle b h) = b * h
area(Triangle l) = (sqrt 3 / 4) * l * l
area(Trapeze bM bN h) = ((bM + bN) / 2) * h

--Funcion que calcula el perimetro de las figuras
perimeter :: Shape -> Float
perimeter(Circle r) = 2 * pi * r
perimeter(Square l) = 4 * l
perimeter(Rectangle b h) = 2 * (b + h)
perimeter(Triangle l) = 3 * l
perimeter(Trapeze bM bm h) = bM + bm + 2 * sqrt (h**2 + ((bM- bm) / 2)**2)

--Ejercicio 2 (Les toca arreglar el sinonimo)
type Point = (Float, Float)

-- Funcion para calcular la distancia entre dos puntos
distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
--Funcion para calcular la distancia de un punto al origen
from0 :: Point -> Float
from0 (x, y) = sqrt (x * x + y * y)

--Ejercicio 3
data Haskellium = Haskellium {name :: String,
                            lastName1 :: String,
                            lastName2 :: String,
                            location :: Point,
                            houseShape :: Shape} deriving (Show, Eq)

--Funcion para regresar el hijo de dos Haskelliums dado su nombre
son :: Haskellium -> Haskellium -> String -> Haskellium
son h1 h2 n = Haskellium {name = n,
                        lastName1 = lastName1 h1,
                        lastName2 = lastName1 h2,
                        location = location h1,
                        houseShape = houseShape h1}

--Funcion para calcular las unidades para construir la casa de un Haskellium
houseCost :: Haskellium -> Float
houseCost h = area (houseShape h) + perimeter (houseShape h) * 2.5 --Considerando el área como el techo de la casa y el perímetro como las paredes, con una altura de 2.5 unidades

--Funcion para calcular el tiempo que le toma a un Haskellium para llegar a su trabajo
timeToWork :: Haskellium -> Float
timeToWork h
    | from0 (location h) <= 300 = from0(location h) / 30 --Si esta a 300 unidades o menos, usa bicicleta a 30 unidades por hora
    | otherwise = from0(location h) / 70 --Si esta a mas de 300 unidades, usa moto a 70 unidades por hora

--LISTAS Y FUNCIONES
--Ejercicio 1
palindromo :: (Eq a) => [a] -> Bool
palindromo [] = True
palindromo [_] = True
palindromo (x:xs) = if x == myLast xs  --Implemento mis propias funciones init y last
                then palindromo (myInit xs) 
                else False

--Funcion auxiliar para palindromo
myInit :: [a] -> [a]
myInit [] = []
myInit [_] = []
myInit (x:xs) = x:myInit xs

--Funcion auxiliar para palindromo
myLast :: [a] -> a
myLast [a] = a
myLast (x:xs) = myLast xs

--Ejercicio 2
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ b [] = b
myFoldr function b (x:xs) = function x (myFoldr function b xs)

--Ejercicio 3
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = [x:y | y <- conjuntoPotencia xs] ++ conjuntoPotencia xs

--ARBOLES

--Implementacion
data OneTwoTree a = Void | 
                    Node { value :: a, child :: (OneTwoTree a) } | 
                    Branch { value :: a, leftChild :: (OneTwoTree a), rightChild :: (OneTwoTree a)} 
                    deriving Show

--Ejercicio 2
suma :: OneTwoTree Int -> Int
suma (Void) = 0
suma (Branch a lc rc) = a + suma lc + suma rc
suma (Node a c) = a + suma c