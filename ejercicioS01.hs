-- Estructuras Discretas 2027-1
-- Ejercicio semanal 01

-- Nombre: Amanda del Pilar Piñones Contreras
-- Número de cuenta: N/A

mensaje :: String
mensaje = "Hola Haskell"

doble :: Int -> Int
doble x = 2 * x

-- Funciones solicitadas

-- 1. Producto de una lista
producto :: Num a => [a] -> a
producto [] = 1
producto (x:xs) = x * producto xs
{- Pruebas:
ghci> producto [2, 3, 4]
24
ghci> producto []
1
ghci> producto [5]
5
ghci> producto [2, 2, 2, 5]
40
-}

-- 2. Contar ceros
cuentaCeros :: [Int] -> Int
cuentaCeros [] = 0
cuentaCeros (x:xs) = (if x == 0 then 1 else 0) + cuentaCeros xs
{- Pruebas:
ghci> cuentaCeros []
0
ghci> cuentaCeros [0]
1
ghci> cuentaCeros [0, 2, 0, 5, 0]
3
ghci> cuentaCeros [0, 0, 1]
2
-}

-- 3. Cuadrados
cuadrado :: Num a => [a] -> [a]
cuadrado [] = []
cuadrado (x:xs) = (x^2) : cuadrado xs
{- Tuve que reeler varias cosas del pdf para hacerlo,
todavía no había entendido bien el ":" en las listas. -}
{- Pruebas:
ghci> cuadrado [1]
[1]
ghci> cuadrado [2]
[4]
ghci> cuadrado [1, 2, 3]
[1,4,9]
ghci> cuadrado []
[]
-}

-- 4. Solo pares
soloPares :: Integral a => [a] -> [a]
soloPares [] = []
soloPares (x:xs) = (if even x then x : soloPares xs else soloPares xs)
{- Originalmente había puesto solo "Int" pensando que en el pdf
solo habían sido muy correctos, al intentarlo me marcó error y entendí que
no jeje. -}
{- Pruebas:
ghci> soloPares [1, 2, 3, 4, 5, 6]
[2,4,6]
ghci> soloPares []
[]
ghci> soloPares [1, 3, 5]
[]
-}

-- 5. Pertenencia
pertenece :: Eq a => a -> [a] -> Bool
pertenece a [] = False
pertenece a (x:xs) = (if a == x then True else pertenece a xs)
-- Confuso tener que indicar la variable antes de la lista pero hace sentido.
{- Pruebas:
ghci> pertenece 2 [1, 2, 3, 4, 5]
True
ghci> pertenece 10 [1, 2, 3]
False
ghci> pertenece 4 []
False
ghci> pertenece 7 [0, 1, 2]
False
-}

-- 6. Preguntas de reflexión

-- 6.1 ¿Qué es el caso base de una función recursiva y por qué es necesario?
{- Es el caso que se puede resolver automáticamente, el punto en que ya no 
es necesario volver a evaluar sino que ya podemos detenernos y regresar 
hasta tener el resultado deseado. Es necesario ya que si no se tiene la función
nunca terminaría y, por lo tanto, no se podría llegar a un resultado. -}

-- 6.2 En el patrón (x:xs), ¿qué representa x y que representa xs?
{- x representa la cabeza de la lista, el primer valor, y xs la cola de la
lista, el resto de elementos. -}

-- 6.3 ¿Qué ocurre si una función recursiva nunca alcanza su caso base?
{- Continua indefinidamente, si no hay un caso base o no se puede llegar
a este, simplemente la función no podrá terminar y por lo tanto no podrá
dar un resultado final. -}

{- 6.4 Explica con tus propias palabras qué significa: "Resolver un problema
utilizando una versión más pequeña del mismo problema" -}
{- Significa dividir un problema en distintos pasos hasta poder conseguir
el resultado completo. Por ejemplo, si quiero sumar (1 + 2 + 3 + 4) primero
sumo 1 + 2, ese resultado después lo sumo con el siguiente elemento, y así
hasta terminar. -}







