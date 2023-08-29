--Ejemplo de función que usa guardas
f:: Int -> Int
f x | x == 1 = 2
    | x == 2 = 5
    | x == 6 = 4
    |otherwise = 10


--Función contiene.
--Decide si un elemento e, pertenece a una lista
contiene :: (Eq a) => a -> [a] -> Bool
contiene _ [] = False
contiene e (x:xs) = if e==x then True else contiene e xs

--Función que recibe una lista de enteros y devuelve la suma de éstos
sumaLista :: [Int] -> Int
sumaLista ls = sumaListaAux 0 ls

{-Función auxiliar de sumaLista, el primer parámetro es el acumulador de la suma parcial
acumulada
-}
sumaListaAux :: Int -> [Int] -> Int
sumaListaAux acc [] = acc
sumaListaAux acc (x:xs) = sumaListaAux (acc + x) xs

--Función que eleva al cuadrado los elementos de una lista
listaCuadrado :: [Int] -> [Int]
listaCuadrado [] = []
listaCuadrado (x:xs) = ((x*x):listaCuadrado xs)

--Función que eleva al cuadrado a un número
cuadrado x = x*x

--Nuestra propia versión chafa(limitada) de la función map
myMap :: (Int -> Int) -> [Int] -> [Int]
myMap _ [] = []
myMap f (x:xs) = ((f x):myMap f xs)

--Función que recibe una lista y devuelve la reversa de ésta.
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = (reversa xs) ++ [x]





