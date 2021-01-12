import Data.List
import Test.HUnit
import Midi.Midi

type Tono         = Integer
type Duracion     = Integer
type Instante     = Integer

data Melodia = 
     Silencio Duracion |
     Nota Tono Duracion |
     Secuencia Melodia Melodia | 
     Paralelo [Melodia]
  deriving (Show, Eq)

-- Funciones auxiliares dadas

foldNat :: a->(a->a)->Integer->a
foldNat caso0 casoSuc n 
      | n == 0 = caso0
      | n > 0 = casoSuc (foldNat caso0 casoSuc (n-1))
      | otherwise = error "El argumento de foldNat no puede ser negativo."


-- Funciones pedidas

-- Ejercicio 1
superponer :: Melodia->Duracion->Melodia->Melodia
superponer m1 d m2 = Paralelo [m1, Secuencia (Silencio d) m2]

--- Sugerencia: usar foldNat
canon :: Duracion->Integer->Melodia->Melodia
canon _ 0 _ = Paralelo []
canon d i m = foldNat m (\x -> superponer m d x) (i-1)

--- Se asume que la lista no es vacía.
secuenciar :: [Melodia] -> Melodia
secuenciar = foldr1 Secuencia

-- Ejercicio 2
canonInfinito :: Duracion -> Melodia -> Melodia
canonInfinito d m = foldr (\x y -> superponer x d y) m (repeat m)


-- Ejercicio 3
--- Indica cómo se recorre la estructura para cada caso.

foldMelodia :: (Duracion -> b) -> (Tono -> Duracion -> b) -> (b -> b -> b) -> ([b] -> b) -> Melodia -> b
foldMelodia fSil fNota fSecu fPara m = case m of
                                        Silencio d -> fSil d

                                        Nota t d -> fNota t d

                                        Secuencia m1 m2 -> fSecu (rec m1) (rec m2)

                                        Paralelo ms -> fPara (map rec ms)
  where rec = foldMelodia fSil fNota fSecu fPara

-- Ejercicio 4
--- Ya no redefine funcion
mapMelodia :: (Tono -> Tono) -> Melodia -> Melodia
mapMelodia f = foldMelodia (Silencio) (\t d-> Nota (f t) d) (Secuencia) (Paralelo)

transportar :: Integer -> Melodia -> Melodia
transportar n = mapMelodia (+n)

--- Esto considerando que las secuencias que tienen nota silencio, el silencio es parte de la melodía.
duracionTotal :: Melodia -> Duracion
duracionTotal = foldMelodia (id) (\t d -> d) (+) (foldl max 0)

-- Ya no redefine funcion
cambiarVelocidad :: Float -> Melodia -> Melodia --Sugerencia: usar round y fromIntegral
cambiarVelocidad f = foldMelodia (\d -> Silencio (round (fromIntegral(d) * f))) (\t d -> Nota t (round (fromIntegral(d) * f))) (Secuencia) (Paralelo)

--No tiene sentido invertir paralelo.
-- Ya no redefine funcion
invertir :: Melodia -> Melodia
invertir = foldMelodia (Silencio) (Nota) (\m1 m2 -> Secuencia m2 m1) (Paralelo)


-- Ejercicio 5

-- En instantes menores que 0 no suena ninguna nota. Se puede usar recursión explícita. Resaltar las partes del código que hacen que no se ajuste al esquema fold.

--Esto originalmente era notasQueSuenan :: Melodia->Instante->[Tono]
notasQueSuenan :: Instante -> Melodia -> [Tono]
-- No es una nota.
notasQueSuenan _ (Silencio d) = []

-- Si el instante es menor a 0, devuelve [] porque nada esta sonando.
notasQueSuenan i _ | i < 0 = []

-- Si d > i entonces la nota esta sonando en el instante.
notasQueSuenan i (Nota t d) = if d > i then [t] else []


--Si en una secuencia la primera nota no esta sonando, deberia reducirse la duracion de esta en el instante.
--Ademas si se encuentra una nota sonando, la siguiente evidentemente no va a sonar.
--Si la duracion de la nota es igual al instante, ¿suena esa o la siguiente? (considero la segunda)

--Esta parte no se ajusta al esquema fold, porque no estas usando el mismo parametro para las dos melodias. Cuando llamas recursivamente con m2, modificaste el valor del instante en funcion de m1.
notasQueSuenan i (Secuencia m1 m2) = nub $ notasQueSuenan i m1 ++ notasQueSuenan (i-duracionTotal m1) m2
-- notasQueSuenan i (Secuencia m1 m2) = if dm1 > i then notasQueSuenan i m1 else notasQueSuenan (i - dm1) m2
-- where dm1 = duracionTotal m1

--Debo ver cada nota que suena en Paralelo
notasQueSuenan i (Paralelo xs) = nub $ concatMap (notasQueSuenan i) xs

--Sugerencia: usar concatMap.

{-Intento definir la funcion con foldMelodia
notasQueSuenan i = foldMelodia (\d -> [])
                               (\t d -> if d > i && i >= 0 then [t] else [])
                               (\x y -> x ++ y) --Esta funcion no logra reflejar el comportamiento de notasQueSuenan, porque la recursion a la segunda melodia de la secuencia no es con los mismos parámetros.
                               (xs -> concat xs) 
-}

{-
notasQueSuenan' :: Melodia -> (Instante -> [Tono])
notasQueSuenan' = foldMelodia (\d -> (\i -> []))
                              (\t d -> (\i -> if d > i && i >= 0 then [t] else []))
                              (\f g -> (\i -> f i ++ g i)) --f y g son funciones Instante -> [Tono]; pero esta mal.. el parametro que le pasas a la funcion g no deberia ser i.
                              (\fs -> (\i -> concatMap (\f -> f i) fs)) --fs es una lista de funciones Instante -> [Tono]. Le aplico a cada funcion la funcion "evaluar en i", y luego concateno los resultados.

-}
-- No se puede hacer usando foldMelodia porque para el caso de Secuencia m1 m2, hay que modificar el instante en base a informacion de m1 cuando se llama recursivamente con m2.
--Y esto no es posible pasandole funciones.

-- notasQueSuenan' tampoco se puede por el mismo motivo basicamente. Necesitas conocer informacion sobre la primer melodia de Secuencia m1 m2, para modificar el instante para el resultado de la otra melodia.


-- Ejercicio 6

data Evento = On Instante Tono | Off Instante Tono deriving (Show, Eq)

--Sugerencia: usar listas por comprensión. No repetir eventos.

--Para todo t perteneciente a t2 y no a t1, el resultado debe tener On i t.
--Para todo t perteneciente a t1 y no a t2, el resultado debe contener el elemento Off i t. 
--No pone en orden por tono.

cambios :: [Tono] -> [Tono] -> Instante -> [Evento]
cambios t1 t2 i = nub ([Off i t | t <- t1, t `notElem` t2] ++ [On i t | t <- t2, t `notElem` t1])



--Sugerencia: usar foldl sobre la lista de 0 a la duración.
--La ultima parte corresponde a poner en off todo lo que estaba sonando desde antes.
eventosPorNotas :: (Instante->[Tono])->Duracion->[Evento]
eventosPorNotas f d = foldl (\es i -> es ++ cambios (f (i-1)) (f i) i ) [] [0..d] ++ cambios (f d) [] (d+1)

notasQueSuenan' :: Melodia -> Instante -> [Tono]
notasQueSuenan' m i = notasQueSuenan i m

-- Ya no repite codigo
eventos :: Melodia -> Duracion -> [Evento]
eventos m d = eventosPorNotas (notasQueSuenan' m) d



-- GENERADOR

unev (On i x)  = (i, Left x)
unev (Off i x) = (i, Right x)

generarMidi :: String -> [Evento] -> IO ()
generarMidi archivo eventos = midiCreateFile archivo midiEvents
  where
    eventos' = let e = map unev eventos in zipWith (\(t0, _) (t1, e) -> (t1 - t0, e)) ((0, error ""):e) e
    midiEvents = case eventos' of
                   [] -> [midiNoteOn 1 0 0 10, midiNoteOn 1 0 0 0]
                   es -> toMidi es

toMidi = map (\(d, ev) -> case ev of
                Left  n -> midiNoteOn d 0 n 127
                Right n -> midiNoteOn d 0 n 0)

--Notas para pruebas.

_sol0 = Nota 55
_si0  = Nota 59
_do = Nota 60
_reb  = Nota 61
_re = Nota 62
_mib  = Nota 63
_mi = Nota 64
_fa = Nota 65
_fas  = Nota 66
_sol = Nota 67
_lab  = Nota 68
_la = Nota 69
_sib  = Nota 70
_si = Nota 71
_do2  = Nota 72
_reb2 = Nota 73
_re2  = Nota 74
_mib2 = Nota 75
_fa2  = Nota 77

-- Melodías para pruebas.

acorde :: Melodia
acorde = Paralelo [_do 10, Secuencia (Silencio 3) (_mi 7), Secuencia (Silencio 6) (_sol 4)]

doremi :: Melodia
doremi = secuenciar [_do 3, _re 1, _mi 3, _do 1, _mi 2, _do 2, _mi 4]

cumpleanios :: Melodia
cumpleanios = secuenciar [_sol 2, _sol 2, _la 4, _sol 4, _do 4, _si 8, 
                          _sol 2, _sol 2, _la 4, _sol 4, _re 4, _do 8,
                          _do 2, _mi 2, _sol 4, _mi 4, _do 4, _si 4, _la 4, _fa 2, _fa 2, _mi 4, _do 4, _re 4, _do 8 
                          ]

-- Pongan sus propias pruebas y melodías. Pueden definir más notas, la numeración es por semitonos.

-- Canon APL (autor: Pablo Barenbaum)

rhoRhoRhoOfX, alwaysEqualsOne, rhoIsDimensionRhoRhoRank, aplIsFun :: Melodia
rhoRhoRhoOfX = secuenciar $ map (\(d, f)->f d) [(4, _do), (4, _do), (3, _do), (1, _re), (4, _mi)]
alwaysEqualsOne = secuenciar $ map (\(d, f)->f d) [(3, _mi), (1, _re), (3, _mi), (1, _fa), (8, _sol)]
rhoIsDimensionRhoRhoRank = secuenciar $ map (\(d, f)->f d) [(12, _do2), (12, _sol), (12, _mi), (12, _do)]
aplIsFun = secuenciar $ map (\(d, f)->f d) [(3, _sol), (1, _fa), (3, _mi), (1, _re), (8, _do)]

mezcla :: Melodia
mezcla = Paralelo [rhoRhoRhoOfX, Secuencia (Silencio 4) alwaysEqualsOne, Secuencia (Silencio 8) rhoIsDimensionRhoRhoRank, Secuencia (Silencio 12) aplIsFun]

-- Cangrejo (autor: Pablo Barenbaum)

stac :: Tono -> Melodia
stac t = Secuencia (Nota t 9) (Silencio 1)

stacatto :: Melodia -> Melodia
stacatto = foldMelodia Silencio (\t d->stac t) Secuencia Paralelo

cangrejo1 = secuenciar $ 
         [Silencio 4, _do 2, _mib 2]
      ++ [_sol 2, _lab 4, Silencio 2]
      ++ [_si0 4, Silencio 2, _sol 4] 
      ++ [_fas 4, _fa 4]              
      ++ [_mi 2, Silencio 2, _mib 4]  
      ++ [_re 2, _reb 2, _do 2]
      ++ [_si0 2, _sol0 2, _do 2, _fa 2]
      ++ [_mib 2, _re 4, Silencio 2]
      ++ [_do 2, _mi 2, Silencio 4]
cangrejo2 = secuenciar $ (map (\(d, f)->f d)) $
               [(2, _do), (2, _mib), (2, _sol), (2, _do2)]
            ++ [(1, _sib), (1, _do2), (1, _re2), (1, _mib2),
                (1, _fa2), (1, _mib2), (1, _re2), (1, _do2)]
            ++ [(1, _re2), (1, _sol), (1, _re2), (1, _fa2),
                (1, _mib2), (1, _re2), (1, _do2), (1, _si)]
            ++ [(1, _la), (1, _si), (1, _do2), (1, _mib2),
                (1, _re2), (1, _do2), (1, _si), (1, _la)]
            ++ [(1, _sol), (1, _lab), (1, _sib), (1, _reb2),
                (1, _do2), (1, _sib), (1, _lab), (1, _sol)]
            ++ [(1, _fa), (1, _sol), (1, _lab), (1, _sib),
                (1, _lab), (1, _sol), (1, _fa), (1, _mib)]
            ++ [(1, _re), (1, _mib), (1, _fa), (1, _sol),
                (1, _fa), (1, _mib), (1, _re), (1, _lab)]
            ++ [(1, _sol), (1, _fa), (1, _mib), (1, _do2),
                (1, _si), (1, _la), (1, _sol), (1, _fa)]
            ++ [(1, _mi), (1, _re), (1, _mi), (1, _sol),
                (1, _do2), (1, _sol), (1, _fa), (1, _sol)]
                
cangrejo = Secuencia c (invertir c)
  where c = Paralelo [cangrejo1, cangrejo2]

--

genMelodia :: String -> Melodia -> Duracion -> IO ()
genMelodia fn m dur = generarMidi fn (eventos m dur)

main :: IO ()
main = do
   putStr "Generando apl-is-fun.mid...\n"
   genMelodia "apl-is-fun.mid" (stacatto mezcla) 500
   putStr "Generando cangrejo.mid...\n"
   genMelodia "cangrejo.mid" (stacatto cangrejo) 1000

-- Tests
tests :: IO Counts
tests = do runTestTT allTests

allTests = test [
  "ejercicio1a" ~: testsEj1a,
  "ejercicio1b" ~: testsEj1b,
  "ejercicio1c" ~: testsEj1c,

  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,

  "ejercicio4a" ~: testsEj4a,
  "ejercicio4b" ~: testsEj4b,
  "ejercicio4c" ~: testsEj4c,
  "ejercicio4d" ~: testsEj4d,
  "ejercicio4e" ~: testsEj4e,

  "ejercicio5" ~: testsEj5,
  "ejercicio6a" ~: testsEj6a,
  "ejercicio6b" ~: testsEj6b,
  "ejercicio6c" ~: testsEj6c
  ]

-- Ejemplos sólo para mostrar cómo se escriben los tests. Reemplazar por los tests propios.

-- variables para testing
sil0 = Silencio 0
sil1 = Silencio 1
sil2 = Silencio 2
not0 = Nota 60 0
not1 = Nota 68 5
not2 = Nota 64 7
sec0 = Secuencia sil0 sil1
sec1 = Secuencia sil0 not2
sec2 = Secuencia not1 not2
par0 = Paralelo [sec1, sil0, not2]
par1 = Paralelo [sec1, sec1, sil0, sil0, not2]
par2 = Paralelo [not1, not0, sil1, par1]

testsEj1a = test [
  Paralelo [(Silencio 0), Secuencia (Silencio 0) (Silencio 1)]  ~=? superponer sil0 0 sil1,
  Paralelo [Paralelo [_do 10, Secuencia (Silencio 3) (_mi 7), Secuencia (Silencio 6) (_sol 4)], Secuencia (Silencio 3) (Nota 60 2)]  ~=? superponer acorde 3 (Nota 60 2)
  ]
testsEj1b = test [
  Paralelo [] ~=? canon 10 0 (Nota 62 5),
  Paralelo [Nota 60 4,Secuencia (Silencio 2) (Paralelo [Nota 60 4,Secuencia (Silencio 2) (Nota 60 4)])] ~=? canon 2 3 (Nota 60 4) 
  ]
testsEj1c = test [
  Nota 62 4 ~=? secuenciar [(Nota 62 4)]  ,
  Secuencia (Silencio 0) (Silencio 1) ~=? secuenciar [(Silencio 0), (Silencio 1)]  ,
  Secuencia (Nota 60 1) (Secuencia (Nota 60 2) (Nota 60 3)) ~=? secuenciar [Nota 60 1, Nota 60 2, Nota 60 3]  ,
  Paralelo [(Secuencia (Silencio 0) (Nota 64 7)), (Secuencia (Silencio 0) (Nota 64 7)), (Silencio 0), (Silencio 0), (Nota 64 7)] ~=? secuenciar [par1] 
  ]
testsEj2 = test [
  [60] ~=? notasQueSuenan 5 (canonInfinito 5 (Nota 60 1)) ,
  [] ~=? notasQueSuenan 4 (canonInfinito 10 (Nota 60 2)) 
  -- notasQueSuenan 1 (canonInfinito 0 (Nota 60 2)) ~=? [] -> Se cuelga ya que la parte izquierda de todas las secuencias tienen siempre duración 0 y son infinitas.
  ]
testsEj3 = test [
  1 ~=? foldMelodia (\x -> 1) (\x y -> 2) (\x y -> 3) (\x -> 4) sil0,
  2 ~=? foldMelodia (\x -> 1) (\x y -> 2) (\x y -> 3) (\x -> 4) not0,
  3 ~=? foldMelodia (\x -> 1) (\x y -> 2) (\x y -> 3) (\x -> 4) sec0,
  4 ~=? foldMelodia (\x -> 1) (\x y -> 2) (\x y -> 3) (\x -> 4) par0
  ]
testsEj4a = test [
  Paralelo [(Nota 99 10), Secuencia (Silencio 3) (Nota 99 7), Secuencia (Silencio 6) (Nota 99 4)]  ~=? mapMelodia (\x -> 99) acorde 
  ]
testsEj4b = test [
  Silencio 0 ~=? transportar 5 sil0,
  (Nota 73 5) ~=? transportar 5 not1,
  Secuencia (Silencio 0) (Nota 69 7) ~=? transportar 5 sec1,
  Paralelo [Secuencia (Silencio 0) (Nota 69 7), (Silencio 0), (Nota 69 7)] ~=? transportar 5 par0
  ]
testsEj4c = test [
  2 ~=? duracionTotal sil2,
  5 ~=? duracionTotal not1,
  12 ~=? duracionTotal sec2,
  7 ~=? duracionTotal par1,
  7 ~=? duracionTotal par2
  ]
testsEj4d = test [
  Silencio 2 ~=? cambiarVelocidad 2.0 sil1,
  Silencio 0 ~=? cambiarVelocidad 0.1 sil1,
  Nota 64 14 ~=? cambiarVelocidad 2.0 not2,
  Nota 64 4 ~=? cambiarVelocidad 0.6 not2,
  Secuencia (Nota 68 10) (Nota 64 14) ~=? cambiarVelocidad 2.0 sec2,
  Paralelo [Nota 68 10, Nota 60 0, Silencio 2, Paralelo [Secuencia (Silencio 0) (Nota 64 14), Secuencia (Silencio 0) (Nota 64 14), Silencio 0, Silencio 0, Nota 64 14]] ~=? cambiarVelocidad 2.0 par2
  ]
testsEj4e = test [
  Secuencia not2 sil0 ~=? invertir sec1,
  Secuencia not2 not1 ~=? invertir sec2,
  Paralelo [Secuencia not2 sil0, sil0, not2] ~=? invertir par0,
  Paralelo [Secuencia not2 sil0, Secuencia not2 sil0, sil0, sil0, not2] ~=? invertir par1
  ]
testsEj5 = test [
  [] ~=? notasQueSuenan (-2) par2,
  [68,64] ~=? notasQueSuenan 1 par2,
  [64] ~=? notasQueSuenan 6 par2,
  [68] ~=? notasQueSuenan 4 sec2
  ]
testsEj6a = test [
  [Off 1 3,On 1 7,On 1 9] ~=? cambios[1,2,3,4,5] [1,2,7,5,7,4,9] 1,
  [On 2 1] ~=? cambios[] [1,1,1,1,1] 2,
  [Off 2 1] ~=? cambios[1,1,1,1,1,1] [] 2,
  [] ~=? cambios[1,2,4,7,8,9] [1,2,4,7,8,9] 2 
  ]
testsEj6b = test [
  --Considerar que, dado un instante, primero obtiene los Off y despues los On.
  [On 0 60, On 1 80, Off 2 60, On 2 90, Off 3 80, Off 3 90] ~=? eventosPorNotas (\x -> if x == 0 then [60] else if x == 1 then [60,80] else if x == 2 then [80,90] else if x == 3 then [80,50,20] else []) 2,
  [On 0 60, On 1 80, Off 2 60, On 2 90, Off 3 90, On 3 50, On 3 20, Off 4 80, Off 4 50, Off 4 20] ~=? eventosPorNotas (\x -> if x == 0 then [60] else if x == 1 then [60,80] else if x == 2 then [80,90] else if x == 3 then [80,50,20] else []) 3
  ]
testsEj6c = test [
  [On 0 60, On 3 64, On 6 67, Off 7 60, Off 7 64, Off 7 67] ~=? eventos acorde 6,
  [On 0 60, Off 3 60, On 3 62, Off 4 62, On 4 64, Off 6 64] ~=? eventos doremi 5,
  [On 0 68, On 0 64, Off 1 68, Off 1 64] ~=? eventos par2 0,
  [] ~=? eventos par2 (-1)

  ]
