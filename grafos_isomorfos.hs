import List

--(Grafo a), que representará un grafo a parti de la lista de sus vértices y de aristas (que son pares de vértices).
data Grafo a = Grafo [a] [(a,a)] deriving (Show, Eq)

--representaremos a las funciones como listas de pares.
type Funcion a b = [(a,b)]

--(Conj a) que representa a los conjuntos como listas ordenadas sin repetición.
type Conj a = [a]

--(aristas g) es la lista de las aristas del grafo g.
aristas :: Grafo a -> [(a,a)]
aristas (Grafo _ as) = as

--(vertices g) es la lista de los vértices del grafo g
vertices :: Grafo a -> [a]
vertices (Grafo vs _) = vs

--(adyacentes g v) es la lista de los vértices adyacentes al vértice v en el grafo g.
adyacentes :: Eq a => Grafo a -> a -> [a]
adyacentes (Grafo vs as) v = [ b | (x,b) <- as, x==v ] ++ [ b | (b,x) <- as, x==v ]

--(aristaEn a g) se verifica si a es una arista del grafo g
aristaEn :: (Eq a, Ord a) => (a,a) -> Grafo a -> Bool
aristaEn a (Grafo _ as) = parOrdeno a `elem` as

--La función (imagen f x) es la imagen del elemento x en la función f.
image :: (Ord a, Ord b) => Funcion a b -> a -> b
image f x = head (imagenRelacion f x)

--(esSubconjunto c1 c2) se verifica si c1 es un subconjunto de c2.
esSubconjunto :: Eq a => [a] -> [a] -> Bool
esSubconjunto c1 c2 = all (`elem` c2) c1

--(esUnitario c) se verifica si el conjunto c es unitario.
esUnitario :: Eq a => [a] -> Bool
esUnitario c = c == take 1 c

--(imagenRelacion r x) es la imagen de x en la relación r.
imagenRelacion :: (Ord a, Ord b) => [(a,b)] -> a -> [b]
imagenRelacion r x = nub [ y | (z,y) <- r, z==x ]

-- (rango r) devuelve el rango de la relación binaria r.
rango :: Ord b => [(a,b)] -> [b]
rango r = listaAConjunto (map snd r)  

--(antiImagenRelacion r y) es la antiimagen del elemento y en la relación binaria r.
antiImagenRelacion :: (Ord a, Ord b) => [(a,b)] -> b -> [a]
antiImagenRelacion r y = nub [x | (x,z) <- r, z == y]

parOrdeno :: Ord a => (a,a) -> (a,a)
parOrdeno (x,y) | x <= y = (x,y)
				| otherwise = (y,x)

-- (secuenciaGrados g) devuelve la secuencia de los grados del grafo g en orden decreciente.
secuenciaGrados :: Eq a => Grafo a -> [Int]
secuenciaGrados g = sortBy (flip compare)[grado g v | v <- vertices g]

--(esSobreyectiva xs ys f) se verifica si la función f es sobreyectiva. A la hora de definirla, estamos contando con que f es una función entre xs y ys.
esSobreyectiva :: (Ord a,Ord b) => [a] -> [b] -> Funcion a b -> Bool
esSobreyectiva _ ys f = ys `esSubconjunto` rango f

--(esInyectiva fs) se verifica si la función fs es inyectiva.
esInyectiva :: (Ord a, Ord b) => Funcion a b -> Bool
esInyectiva f = all esUnitario [antiImagenRelacion f y | y <- rango f]

--(esBiyectiva xs ys f) se verifica si la función f es biyectiva.
esBiyectiva :: (Ord a, Ord b) => [a] -> [b] -> Funcion a b -> Bool
esBiyectiva xs ys f = esInyectiva f && esSobreyectiva xs ys f

--(listaAConjunto xs) devuelve el conjunto cuyos elementos son los de la lista xs.
listaAConjunto :: Eq a => [a] -> Conj a
listaAConjunto = nub

-- (variacionesR n xs) devuelve las variaciones con con repetición de los elementos de xs en listas de n elementos.
variacionesR :: Int -> [a] -> [[a]]
variacionesR _ [] = [[]]
variacionesR 0 _ = [[]]
variacionesR k us = [u:vs | u <- us, vs <- variacionesR (k-1) us]

-- (funciones xs ys) devuelve todas las posibles funciones del conjunto xs en ys.
funciones :: [a] -> [b] -> [Funcion a b]
funciones xs ys = [zip xs zs | zs <- variacionesR (length xs) ys]

--(conservaAdyacencia g h f) se verifica si la función f entre los grafos g y h conserva las adyacencias.
conservaAdyacencia :: (Ord a, Ord b) => Grafo a -> Grafo b -> Funcion a b -> Bool
conservaAdyacencia g h f = and [(image f x, image f y) `aristaEn` h | (x,y) <- aristas g ]

--(biyecciones xs ys) devuelven la lista de todas las biyecciones entre los conjuntos xs y ys. Lo hace filtrando las funciones entre los conjuntos que son biyectivas
biyecciones :: (Ord a, Ord b) => [a] -> [b] -> [Funcion a b]
biyecciones xs ys = filter (esBiyectiva xs ys) (funciones xs ys)

--(tamaño g) devuelve el orden del grafo g.
tamanno :: Grafo a -> Int
tamanno = length . aristas

--(orden g) devuelve el orden del grafo g
orden :: Grafo a -> Int
orden = length . vertices

--(entorno g v) devuelve el entorno del vértice v en el grafo g.
entorno :: Eq a => Grafo a -> a -> [a]
entorno = adyacentes

--(grado g v) devuelve el grado del vértice v en el grafo g.
grado :: Eq a => Grafo a -> a -> Int
grado g a = length(entorno g a)

--(isomorfismos g h) devuelve todos los isomorfismos posibles entre los grafos g y h.
isomorfismos :: (Ord a, Ord b) => Grafo a -> Grafo b -> [Funcion a b]
isomorfismos g h |orden g /= orden h = []
				|tamanno g /= tamanno h = []
				|secuenciaGrados g /= secuenciaGrados h =[]
				|otherwise = [f | f <- biyecciones vs1 vs2, conservaAdyacencia g h f]
				where 
					vs1 = vertices g
					vs2 = vertices h

 --(isomorfos g h) se verifica si los grafos g y h son isomorfos
isomorfos :: (Ord a, Ord b) => Grafo a -> Grafo b -> Bool
isomorfos g = not. null . isomorfismos g



-------------------------------------------------------
----------------------AUXILIARES-------------------------
-------------------------------------------------------

creaGrafo :: Ord a => [a] -> [(a,a)] -> Grafo a
creaGrafo a b = Grafo a b 

-------------------------------------------------------
----------------------EJEMPLOS-------------------------
-------------------------------------------------------
ejEmplo :: Grafo Int
ejEmplo = creaGrafo [1..5] [(1,2),(2, 3),(3,4),(4,5), (1,5),(5,4),(5,3),(5,2)]

g1 :: Grafo Int
g1 = creaGrafo [1..4] [(1,2),(2,3),(3,4)]

g2 :: Grafo Int
g2 = creaGrafo [1..4] [(1,2),(2,3),(2,4)]

g3 :: Grafo Int
g3 = creaGrafo [4,6..10] [(4,8),(6,8),(8,10)]