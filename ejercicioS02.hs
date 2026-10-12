-- Estructuras Discretas 2027-1
-- Ejercicio semanal 02

-- Nombre: Amanda del Pilar Piñones Contreras
-- Número de cuenta: N/A

-- Ejercicio 1. Definiciones por casos
anterior :: Integer -> Integer
anterior x = if x == 0 then 0 else x-1
{- Primero tuve un error porque puse x = 0 en vez de x == 0 :(
Pruebas:
ghci> anterior 0
0
ghci> anterior 1
0
ghci> anterior 2
1
ghci> anterior 11
10
-}

-- Ejercicio 2. Definiciones locales
totalCompra :: Double -> Integer -> Double
totalCompra precio cantidad = subtotal - descuento
           where 
            subtotal = precio * fromIntegral cantidad
            descuento = if subtotal >= 500 then subtotal * 0.15 else 0
{- Jeje no vimos fromIntegral. Le pedí ayuda a la IA para que me diera 
pistas de cómo hacerlo y mencionó que no podría operar un double y un
integer así como así y que usara fromIntegral entonces me confundí más.
Investigué y encontré esta página y ya le entendí a cómo usarlo y
pude terminar el ejercicio :)
Página: https://learnyouahaskell.github.io/types-and-typeclasses.html
Pruebas:
ghci> totalCompra 100 3
300.0
ghci> totalCompra 100 6
510.0
ghci> totalCompra 20.90 4
83.6
ghci> totalCompra 1001.50 1
851.275
 -}

-- Ejercicio 3. Contando mediante recursión
sucesor :: Integer -> Integer
sucesor n = n + 1
-- indica que si tenemos un número n, su sucesor (número siguiente) sera n + 1. Ej: el sucesor de 1 es 2, lo cual es 1 +1

contar :: Integer -> Integer -> [Integer]
contar a b = if a > b then []
{- en el momento en que a > b ya habrá sido un sucesor de b, 
por eso es el caso base -}
else a : contar (sucesor a) b
-- estructura x:xs
{- Pruebas:
ghci> contar 3 7
[3,4,5,6,7]
ghci> contar 3 3
[3]
ghci> contar 0 10
[0,1,2,3,4,5,6,7,8,9,10]
ghci> contar 10 5
[]
ghci> contar 11 15
[11,12,13,14,15]
-}

-- Ejercicio 4. Suma recursiva
-- Ya se había declarado sucesor anteriormente :)
suma :: Integer -> Integer -> Integer
suma a 0 = a
suma a b = sucesor (suma a (b - 1))
-- a + S(b) = S(a + b)
{- Pruebas:
ghci> suma 4 0
4
ghci> suma 0 0
0
ghci> suma 4 3
7
ghci> suma 3 4
7
ghci> suma 22 2
24
-}
{- Evaluación paso a paso de suma 3 2:
i)  suma 3 2 = sucesor (suma 3 (2 - 1))
= sucesor (suma 3 1)
ii)  suma 3 1 = sucesor (suma 3 (1 - 1))
= sucesor (suma 3 0)
= sucesor 3 = 4
Sustituimos ii) en i)
suma 3 2 = sucesor 4 = 5
suma 3 2 = 5 -}


-- Ejercicio 5. Conversión de unidades
flOz :: Double
flOz = 1
-- Onzas (flOz) como unidad base

cup :: Double 
cup = 8 * flOz
-- equivalencia de una taza en onzas

tbsp :: Double
tbsp = cup / 16
-- equivalencia de tbsp en tazas, de ahí se puede conseguir la equivalencia a onzas

tsp :: Double
tsp = tbsp / 3
-- equivalencia tsp en tbsp, de ahí a tazas y de tazas a onzas

convertirA :: Double -> Double -> Double
convertirA unidad cantidad = cantidad / unidad
{- Teoría pruebas:
convertirA tsp (2 * cup)
1 cup = 8 flOz, 2 cup = 16 flOz 
1 tsp = (8/16)/3 = 1/6 = 0.1666 flOz 
16 / (1/6) = 96
Resultado: 96.0

convertirA tbsp cup
16 tbsp en 1 cup ya que tbsp = cup/16
Resultado: 16.0

convertirA cup (2 * cup)
¿Cuántas tazas caben en 2 tazas? 2/1 = 2
Resultado: 2.0

convertirA tsp tbsp
3 tsp en 1 tbsp ya que tsp = tbsp/3
Resultado: 3.0

convertirA flOz (3 * tsp)
3 tsp = 1 tbsp, 1 tbsp = 0.5 flOz
Por lo tanto: 0.5 / 1.0 = 0.5
Resultado: 0.5 -}

{- Resultado pruebas:
ghci> convertirA tsp (2 * cup)
96.0
ghci> convertirA tbsp cup
16.0
ghci> convertirA cup (2 * cup)
2.0
ghci> convertirA tsp tbsp
3.0
ghci> convertirA flOz (3 * tsp)
0.5
-}




 



