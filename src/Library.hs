module Library where
import PdePreludat



data Raton = Raton {
    nombre       :: String,
    edad         :: Anios,
    peso         :: Float,
    enfermedades :: [Enfermedad]
}

type Anios      = Int
type Enfermedad = String
type Hierba     = Raton -> Raton
type Potencia   = Int


-------------
-- Punto 1 --
-------------


cerebro :: Raton
cerebro = UnRaton {
    nombre = "cerebro",
    edad = 9,
    peso = 0.2,
    enfermedades = ["brucelosis","sarampion","tuberculosis"]
}

bicenterrata  :: Raton
bicenterrata  = UnRaton {
    nombre = "bicenterrata ",
    edad = 256,
    peso = 0.2,
    enfermedades = []
}

huesudo  :: Raton
huesudo  = UnRaton {
    nombre = "huesudo ",
    edad = 4,
    peso = 10,
    enfermedades = ["altaObesidad","sinusitis"]
}



-------------
-- Punto 2 --
-------------
--A--

hierbaBuena :: Hierba
hierbaBuena = modificarEdad . sqrt edad 

--B--

hierbaVerde :: String -> Hierba
hierbaVerde terminacion = quitarEnfermedadesSegun terminacion

quitarEnfermedadesSegunTerminacio ::  String -> Raton -> Raton
quitarEnfermedadesSegunTerminacio terminacion = enfermedades raton . filter (mismaTerminacion terminacion)

mismaTerminacion :: String -> String -> Bool
mismaTerminacion terminacion enfermedad = take (length terminacion) enfermedad == terminacion

--C--

alcachofa :: Hierba

alcachofa raton
    | peso raton > 2 = modificarPeso 0.9 raton
    | otherwise      = modificarPeso 0.95 raton

modificarPeso :: Float -> Raton -> Raton
modificarPeso cantidad raton = raton {peso = peso raton * cantidad}

--D--

hierbaZort :: Hierba
hierbaZort raton = quitarEnfermedades . modificarEdad 0 $ raton

vaciarEnfermedades :: Raton -> Raton
vaciarEnfermedades raton = raton {enfermedades = []}

modificarEdad :: Anios -> Raton -> Raton
modificarEdad cantidad raton = raton {edad = edad raton * cantidad}

--E--

hierbaDelDiablo :: Hierba
hierbaDelDiablo raton = max 0 . modificarPeso (-0.1) . (eliminarEnfermedad . conMenosDe10)$ raton

conMenosDe10 :: Raton -> Raton
conMenosDe10 raton = raton {enfermedades = filter (menosDe10 raton) (enfermedades raton)}

menosDe10 :: Raton -> Enfermedad -> Bool
menosDe10 raton enfermedad = length enfermedad < 10


-------------
-- Punto 3 --
-------------

type Medicamentos = Raton -> Raton


--A--

pondsAntiAge :: Medicamentos
pondsAntiAge = [hierbaBuena, hierbaBuena, hierbaBuena, alcachofa]

--B--

reduceFatFast :: Potencia -> Medicamentos
reduceFatFast potencia = hierbaBuena "Obesidad" . replicate potencia alcachofa

--C--

sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

pdepCilina :: Medicamentos
pdepCilina = map esEnfermedadInfecciosa
    where
        esEnfermedadInfecciosa = any (terminaCon sufijosInfecciosas) . enfermedades


-------------
-- Punto 4 --
-------------

cantidadIdeal ::


lograEstabilizar :: Medicamentos -> [Raton] -> Bool
lograEstabilizar medicamento unosRatones = all (cumpleCondiciones . map medicamento unosRatones)

cumpleCondiciones :: Raton -> Bool
cumpleaCondiciones raton = noHaySobrepeso . tienenMenosDe3Enfermedades $ raton

noHaySobrepeso :: Raton -> Bool
noHaySobrepeso = (<1) . peso

tienenMenosDe3Enfermedades :: Raton -> Bool
tienenMenosDe3Enfermedades = (<3) . length . enfermedades




