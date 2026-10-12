
{-
  Estructuras Discretas 2027-1
  Binarios representados como listas de 0s y 1s
  Nombre: 

  IMPORTANTE:
  En esta representación el bit MENOS significativo aparece a la izquierda.

      []        representa 0
      [1]       representa 1
      [0,1]     representa 2
      [1,1]     representa 3
      [0,0,1]   representa 4
      [1,0,1]   representa 5

  La idea recursiva es:

      valor []       = 0
      valor (0:xs)   = 2 * valor xs
      valor (1:xs)   = 2 * valor xs + 1

  Por ejemplo:

      valor [1]
    = valor (1:[])
    = 2 * valor [] + 1
    = 2 * 0 + 1
    = 1
-}

module BinariosListas where

-- ============================================================
-- TIPO
-- ============================================================

-- Un binario es una lista cuyos elementos deben ser 0 o 1.
type Bin = [Int]


-- ============================================================
-- EJEMPLOS
-- ============================================================

cero, uno, dos, tres, cuatro, cinco, seis, siete, ocho, nueve, diez :: Bin

cero   = []
uno    = [1]
dos    = [0,1]
tres   = [1,1]
cuatro = [0,0,1]
cinco  = [1,0,1]
seis   = [0,1,1]
siete  = [1,1,1]
ocho   = [0,0,0,1]
nueve  = [1,0,0,1]
diez   = [0,1,0,1]


-- ============================================================
-- VALIDACIÓN
-- ============================================================

-- Verifica que todos los elementos de una lista sean bits.
esBinario :: Bin -> Bool
-- Caso Base
esBinario [] = True
-- Porque una lista vacía es, por definición, 0 y el 0 es un binario.
-- Caso Recursivo
esBinario (x:xs) = (x == 1 || x == 0) && esBinario xs


-- ============================================================
-- BINARIO A DECIMAL
-- ============================================================

-- [] representa al cero.
--
-- Si el primer bit es 0:
--   valor (0:bs) = 2 * valor bs
--
-- Si el primer bit es 1:
--   valor (1:bs) = 2 * valor bs + 1
--
-- La fórmula general funciona para ambos bits:
--   valor (b:bs) = 2 * valor bs + b

btd :: Bin -> Int
btd [] = 0
btd (x:xs) = 2 * btd xs + x
-- preferí usar x:xs que b:bs jeje


-- ============================================================
-- DECIMAL A BINARIO
-- ============================================================

-- divMod n 2 regresa (cociente, residuo).
-- El residuo es el siguiente bit que colocamos al inicio.
--
-- Ejemplo:
--   dtb 5
--   = 1 : dtb 2
--   = 1 : 0 : dtb 1
--   = 1 : 0 : 1 : dtb 0
--   = [1,0,1]

dtb :: Int -> Bin
dtb 0 = []
dtb n = mod n 2 : dtb (div n 2)
-- Se va poniendo en una lista el residuo y luego se sigue dividiendo

-- ============================================================
-- DOBLE Y DOBLE DEL SUCESOR
-- ============================================================

-- Si xs representa x, entonces 0:xs representa 2x.
doble :: Bin -> Bin
doble bs = 0 : bs

-- Si xs representa x, entonces 1:xs representa 2x + 1.
dobleSuc :: Bin -> Bin
dobleSuc bs = 1 : bs


-- ============================================================
-- INCREMENTO
-- ============================================================

-- inc suma 1 a un binario.
--
-- Casos:
--
--   inc []       = [1]
--   inc (0:ds)   = 1:ds
--   inc (1:ds)   = 0:inc ds
--
-- En el último caso aparece el acarreo.

inc :: Bin -> Bin
inc [] = [1]
inc (0:ds) = 1 : ds
inc (1:ds) = 0 : inc ds

-- ============================================================
-- SUMA
-- ============================================================

-- La suma se obtiene de las cuatro posibilidades:
--
-- 0x + 0y = 0(x+y)
-- 0x + 1y = 1(x+y)
-- 1x + 0y = 1(x+y)
-- 1x + 1y = 0(inc(x+y))
--
-- Como trabajamos con listas:
--
--   0x se escribe 0:xs
--   1x se escribe 1:xs

badd :: Bin -> Bin -> Bin
badd [] ys = ys
badd xs [] = xs
badd (0:xs) (0:ys) = 0 : badd xs ys
badd (0:xs) (1:ys) = 1 : badd xs ys
badd (1:xs) (0:ys) = 1 : badd xs ys
badd (1:xs) (1:ys) = 0 : inc(badd xs ys)


-- ============================================================
-- PRODUCTO
-- ============================================================

-- Esta sección es una extensión de la misma construcción.
-- No es necesaria para entender btd, inc y badd, pero muestra
-- que también podemos definir el producto directamente sobre
-- la estructura de las listas.

bprod :: Bin -> Bin -> Bin
bprod [] _ = []
bprod _ [] = []
bprod (0:xs) ys = doble (bprod xs ys)
bprod (1:xs) ys = badd ys (doble (bprod xs ys))


-- ============================================================
-- PRUEBAS
-- ============================================================

pruebasBtd :: Bool
pruebasBtd =
     btd cero   == 0
  && btd uno    == 1
  && btd dos    == 2
  && btd tres   == 3
  && btd cuatro == 4
  && btd cinco  == 5
  && btd diez   == 10

pruebasDtb :: Bool
pruebasDtb =
     dtb 0  == []
  && dtb 1  == [1]
  && dtb 2  == [0,1]
  && dtb 5  == [1,0,1]
  && dtb 10 == [0,1,0,1]

pruebasInc :: Bool
pruebasInc =
     btd (inc cero)  == 1
  && btd (inc uno)   == 2
  && btd (inc tres)  == 4
  && btd (inc siete) == 8

pruebasSuma :: Bool
pruebasSuma =
     btd (badd uno uno)     == 2
  && btd (badd dos tres)    == 5
  && btd (badd tres cuatro) == 7
  && btd (badd cinco seis)  == 11

pruebasProducto :: Bool
pruebasProducto =
     btd (bprod dos tres)    == 6
  && btd (bprod tres cuatro) == 12
  && btd (bprod cinco seis)  == 30

todasLasPruebas :: Bool
todasLasPruebas =
     pruebasBtd
  && pruebasDtb
  && pruebasInc
  && pruebasSuma
  && pruebasProducto


-- ============================================================
-- EJEMPLOS PARA GHCI
-- ============================================================

{-
  :load BinariosListas.hs

  btd [1]
  -- 1

  btd [0,1]
  -- 2

  btd [1,0,1]
  -- 5

  dtb 6
  -- [0,1,1]

  inc [1,1]
  -- [0,0,1]

  badd [0,1] [1,1]
  -- [1,0,1]

  bprod [0,1] [1,1]
  -- [0,1,1]

  todasLasPruebas
  -- True

Las comprobé

ghci> btd[1]
1
ghci> btd[0,1]
2
ghci> btd[1,0,1]
5
ghci> dtb 6
[0,1,1]
ghci> inc[1,1]
[0,0,1]
ghci> badd[0,1][1,1]
[1,0,1]
ghci> bprod[0,1][1,1]
[0,1,1]
ghci> todasLasPruebas
True

-}

-- Ejercicios:

-- Escribe como listas los números 0, 1, 2, 3, 4, 5, 6, 7, 8.
{- 
0 = [] 
0 = [0]
1 = [1]
2 = [0, 1]
3 = [1, 1]
4 = [0, 0, 1]
5 = [1, 0, 1]
6 = [0, 1, 1]
7 = [1, 1, 1]
8 = [0, 0, 0, 1]
-}

-- Evalúa manualmente: btd[0, 1, 1]
{-
 btd [0, 1, 1]
= 2 * (btd [1, 1] + 0)
= 2 * ((2 * btd [1] + 1)+0)
= 2 * ((2 * (2 * [] +1) + 1)+0)
= 2 * ((2 *  (+1) + 1)+0)
= 2 * ((2 + 1)+0)
= 2 * ((3)+0)
= 2 * 3 = 6
-}

-- Evalúa inc[1,1,1]
{-
inc[1,1,1]
= 0 : inc [1, 1]
= 0 : 0 : inc [1]
= 0 : 0 : 0 : inc []
= 0 : 0 : 0 : 1
= [0, 0, 0, 1]

btd[1,1,1]
⁠= 2 * btd [1, 1] + 1⁠
⁠= 2 * (2 * btd [1] + 1) + 1⁠
⁠= 2 * (2 * (2 * btd [] + 1) + 1) + 1⁠
⁠= 2 * (2 * (2 * 0 + 1) + 1) + 1⁠
⁠= 2 * (2 * (1) + 1) + 1⁠
⁠= 2 * (2 + 1) + 1⁠
⁠= 2 * 3 + 1⁠
⁠= 6 + 1⁠
⁠= 7⁠

btd[0,0,0,1]
⁠= 2 * btd [0, 0, 1] + 0⁠
⁠= 2 * (2 * btd [0, 1] + 0) + 0⁠
⁠= 2 * (2 * (2 * btd [1] + 0) + 0) + 0⁠
⁠= 2 * (2 * (2 * (2 * btd [] + 1) + 0) + 0) + 0⁠
⁠= 2 * (2 * (2 * (2 * 0 + 1) + 0) + 0) + 0⁠
⁠= 2 * (2 * (2 * (1) + 0) + 0) + 0⁠
⁠= 2 * (2 * (2 + 0) + 0) + 0⁠
⁠= 2 * (2 * 2 + 0) + 0⁠
⁠= 2 * (4 + 0) + 0⁠
⁠= 2 * 4 + 0⁠
⁠= 8 + 0⁠
⁠= 8⁠
-}

-- Justifica algebraicamente: 0x + 1y = 1(x+y)
{-
Por definición: 0x = 2x && 1y = 2y +1
Entonces: 0x + 1y = 2x + (2y +1)
= 2x + 2y + 1
= 2(x+y) +1
Y como 2y + 1 = 1 y tenemos que
= 1(x+y)
-}

-- Desarrolla paso a paso: badd [1,1] [1,0,1]
{-
badd [1,1] [1,0,1]
= 0 : inc (badd [1] [0,1])
= 0 : inc (1 : badd [] [1])
= 0 : inc (1 : [1])
= 0 : inc [1,1]
= 0 : (0 : inc [1])
= 0 : (0 : (0 : inc []))
= 0 : (0 : (0 : [1]))
= 0 : (0 : [0,1])
= 0 : [0,0,1]
= [0,0,0,1]
-}

-- ¿Qué lista representa al número 12? 
{- [0,0,1,1]
que básicamente es 2^0 * 0 + 2^1 * 0 + 2^2 * 1 + 2^3 * 1
lo cual es 0 + 0 + 4 + 8 que es 12 :) 
-}

-- ¿Por qué agregar un 0 al inicio de una lista representa multiplicar por 2?
{- Porque según la posición de cada dígito de una lista binaria se sabe en qué
potencia de 2 se encuentra, siendo cada potencia el doble de la anterior. Entonces
al mover todos los elementos a la siguiente potencial al añadir un 0 al inicio, cada
valor binario ahora representará el doble de su posición anterior y por lo tanto
todo el número representado en la lista será duplicado en valor. -}

