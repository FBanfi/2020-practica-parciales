module Lib where
import Text.Show.Functions

laVerdad = True


-- ***********************************************************************************************************************************************************
-- Parcial de Ratones y Hierbas
-- ***********************************************************************************************************************************************************

{-
----------------------------------------------Punto 1-------------------------------------------------------
data Raton = UnRaton {
    nombre::String,
    edad::Int,
    peso::Float,
    enfermedades::[Enfermedad]
} deriving (Show,Eq)

type Enfermedad = String

cerebroElRaton = UnRaton {
    nombre="Cerebro",
    edad=9,
    peso=0.2,
    enfermedades=[]
}
bicenterraLaRata = UnRaton {
    nombre="Bicenterra",
    edad=256,
    peso=0.2,
    enfermedades=[]
}
huesudoElRaton = UnRaton {
    nombre="Huesudo",
    edad=4,
    peso=0,
    enfermedades=[]
}

----------------------------------------------Punto 2---------------------------------------------------------
type Hierba = Raton -> Raton
type Medicamento = Raton -> Raton

hierbaBuena :: Hierba
hierbaBuena raton = raton {edad=(floor.sqrt.fromIntegral) (edad raton)}



hierbaVerde :: String -> Hierba                     -- a los que terminen igual que la palabra entrada los saco de las enfermedades
hierbaVerde terminacionDada raton = raton {enfermedades= filter (not.terminanIgual(terminacionDada)) (enfermedades raton)}

terminanIgual :: String -> String -> Bool
terminanIgual terminacion enfermedad = (==terminacion) (reverse (take (length terminacion) (reverse enfermedad)))



alcachofa :: Hierba
alcachofa raton
    | (>=2) (peso raton) = raton {peso= (peso raton)*0.9}      -- lean ademas de hacer esto hizo abstraccion para cada respuesta de la guarda
    | otherwise = raton {peso= (peso raton)*0.95}                       



hierbaZort :: Hierba
hierbaZort = (cambiarNombre "Pinky").recienNacido.masLimpioQueToallitaDeBebe

cambiarNombre :: String -> Hierba
cambiarNombre nombre raton = raton {nombre= nombre}

recienNacido :: Hierba
recienNacido raton = raton {edad=0}

masLimpioQueToallitaDeBebe :: Hierba
masLimpioQueToallitaDeBebe raton = raton {enfermedades=[]}



hierbaDelDiablo :: Hierba
hierbaDelDiablo raton = raton {peso= max 0 (peso raton - 0.1),enfermedades=filter (not.criterioParaEnfermedad) (enfermedades raton)}

criterioParaEnfermedad :: Enfermedad -> Bool
criterioParaEnfermedad enfermedad = length enfermedad < 10

----------------------------------------------Punto 3------------------------------------------------------------
pondsAntiAge :: Raton -> Raton
pondsAntiAge = hierbaBuena.hierbaBuena.hierbaBuena.alcachofa



reduceFatFast :: Int -> Medicamento
reduceFatFast n raton = foldl aplicarHierba raton ([hierbaVerde "Obesidad"] ++ take n (repeat alcachofa))

aplicarHierba :: Raton -> Hierba -> Raton
aplicarHierba raton hierba = hierba raton


{-
pdepCilina :: Medicamento
pdepCilina raton = raton {enfermedades=filter (not.criterioCilina) (enfermedades raton)}

criterioCilina :: String -> Bool
criterioCilina enfermedad = 
    terminanIgual "sis" enfermedad ||
    terminanIgual "itis" enfermedad ||
    terminanIgual "emia" enfermedad ||
    terminanIgual "cocos" enfermedad 


listaDeCilina = ["sis","itis","emia","cocos"]
-}

pdepCilina :: Medicamento
pdepCilina raton = foldr hierbaVerde raton ["sis","itis","emia","cocos"]


-------------------------------------------Punto 4-----------------------------------------------------------
cantidadIdeal :: (Int->Bool) -> Int
cantidadIdeal condicion = head (filter condicion [1..])



listaDeRatones = [cerebroElRaton,bicenterraLaRata,huesudoElRaton]

lograEstabilizar :: Medicamento -> [Raton] -> Bool
lograEstabilizar medicamento comunidad = all condicionEstabilizar (map medicamento comunidad)

condicionEstabilizar :: Raton -> Bool
condicionEstabilizar raton = peso raton < 1 && (length (enfermedades raton) < 4)



potenciaIdealPalFatFast :: [Raton] -> Int
potenciaIdealPalFatFast comunidad = cantidadIdeal (funcionCondicion comunidad)

funcionCondicion :: [Raton] -> Int -> Bool
funcionCondicion comunidad  potencia = lograEstabilizar (reduceFatFast potencia) comunidad


-}


-- ***********************************************************************************************************************************************************
-- Parcial MiniGolfito
-- ***********************************************************************************************************************************************************

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b


-----------------------------------Punto 1--------------------------------------------------
type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = (UnTiro 10 ((precisionJugador habilidad) *2) 0)

madera :: Palo
madera habilidad = (UnTiro 100 (div (precisionJugador habilidad) 2) 5)

hierros :: Int -> Palo
hierros n habilidad = (UnTiro ((fuerzaJugador habilidad)*n) (div (precisionJugador habilidad) n) (max 0 (n-3)))


listaDePalos = [putter, madera,(hierros 1),(hierros 2),(hierros 3),(hierros 4),(hierros 5),(hierros 6),(hierros 7),(hierros 8),(hierros 9),(hierros 10)]