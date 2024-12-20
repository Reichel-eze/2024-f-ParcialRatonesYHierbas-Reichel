module Library where
import PdePreludat

-- 1) Modelar a los ratones mencionados.

data Raton = UnRaton {
    nombre :: String,
    edad :: Number, -- en anios
    peso :: Number,
    enfermedades :: [String]
}deriving(Show, Eq)

-- Cerebro es un ratón con 9 años, 0.2 kg de peso y tiene brucelosis, sarampión y tuberculosis.
-- Bicenterrata es un ratón con 256 años, 0.2kg de peso, y completamente sano.
-- Huesudo es un ratón de 4 años con 10kg de peso, y alta obesidad y sinusitis.

cerebro :: Raton
cerebro = UnRaton "Cerebro" 9 0.2 ["brucelosis","sarampion","tuberculosis"]

bicenterrata :: Raton
bicenterrata = UnRaton "Bicenterrata" 256 0.2 []

huesudo :: Raton
huesudo = UnRaton "Huesudo" 4 10 ["obesidad","sinusitis"]

ratonDe3KG :: Raton
ratonDe3KG = UnRaton "RATON3KG" 10 3 []

-- 2) Existen distintos tipos de hierbas que afectan (modifican) de diferentes maneras al ratón. Definir dichas hierbas:

-- Funciones Auxiliares --

rejuvenecerA :: Number -> Raton -> Raton
rejuvenecerA valor raton = raton {edad = valor}

eliminarEnfermedad :: String -> Raton -> Raton
eliminarEnfermedad enfermedad raton = raton {enfermedades = filter(/= enfermedad) (enfermedades raton)}

perderPeso :: Number -> Raton -> Raton 
perderPeso valor raton = raton {peso = max (peso raton - valor) 0} -- sin disminuir de cero

perderPesoEnPorcentaje :: Number -> Raton -> Raton
perderPesoEnPorcentaje porcentaje raton = perderPeso (porcentaje * peso raton / 100) raton 

type Hierba = Raton -> Raton

-- hierbaBuena, que rejuvenece al ratón a la raíz cuadrada de su edad.
-- Por ejemplo, si a cerebro le doy hierbaBuena, se transforma en un ratón de 3 años.

hierbaBuena :: Hierba
hierbaBuena raton = rejuvenecerA (sqrt (edad raton)) raton   

-- hierbaVerde, elimina las enfermedades que terminen de cierta forma.
-- Por ejemplo, si a cerebro le doy la hierbaVerde del tipo “sis”, queda sólo con sarampión.

hierbaVerde :: String -> Hierba
hierbaVerde terminacion raton = raton {enfermedades = filter (not . terminaCon' terminacion) (enfermedades raton)}

--eliminarEnfermedadConTerminacion :: String -> [String] -> [String]
--eliminarEnfermedadConTerminacion terminacion = filter (not . terminaCon terminacion) 

terminaCon :: String -> String -> Bool
terminaCon terminacion texto = reverse terminacion == take (length terminacion) (reverse texto)

terminaCon' :: String -> String -> Bool
terminaCon' terminacion enfermedad = terminacion == drop (length enfermedad - length terminacion) enfermedad

-- alcachofa, hace que el ratón pierda peso en un 10% si pesa más de 2kg, sino pierde un 5%.
-- Por ejemplo, un raton de 3 kg queda con 2,7 kg y cerebro queda con 0.19 kg. 

alcachofa :: Hierba
alcachofa raton 
    | peso raton > 2 = perderPesoEnPorcentaje 10 raton
    | otherwise      = perderPesoEnPorcentaje 5 raton

alcachofaV2 :: Hierba
alcachofaV2 raton = perderPeso (peso raton * coeficiente raton) raton

coeficiente :: Raton -> Number
coeficiente raton
    | peso raton > 2 = 0.1
    | otherwise      = 0.05

-- hierbaZort, hace que el ratón se transforme en Pinky, perdiendo todas sus enfermedades y quedando con 0 años de edad.

hierbaZort :: Hierba 
hierbaZort = transformarseEnPinky

transformarseEnPinky :: Raton -> Raton
transformarseEnPinky raton = raton {enfermedades = [], edad = 0}

-- hierbaDelDiablo, hace que el ratón pierda 0.1kg (sin disminuir de 0) y elimina todas las enfermedades con menos de 10 letras.

hierbaDelDiablo :: Hierba
hierbaDelDiablo = eliminarEnfermedadConMenosDe 10 . perderPeso 0.1 

eliminarEnfermedadConMenosDe :: Number -> Raton -> Raton
eliminarEnfermedadConMenosDe numero raton = raton {enfermedades = filter (\enf -> length enf >= numero) (enfermedades raton)}

enfermedadConMenosDe :: Number -> String -> Bool
enfermedadConMenosDe numero = (< numero) . length 

-- 3) Medicamentos: Los medicamentos son la administración sucesiva de un conjunto de hierbas. Se pide crear los siguientes
-- medicamentos para luego poder administrarlos en un ratón: 

type Medicamento = [Hierba]

administrarMedicamento :: Medicamento -> Raton -> Raton
administrarMedicamento medicamento raton = foldr aplicarHierba raton medicamento

aplicarHierba :: Hierba -> Raton -> Raton
aplicarHierba hierba raton = hierba raton 

-- a) Hacer el pondsAntiAge, que es un medicamento que está hecho con 3 hierbas buenas y una alcachofa.
-- Por ejemplo, si se lo administramos al ratón Bicenterrata, queda con 2 años y 0.19 kg 

pondsAntiAge :: Medicamento
pondsAntiAge = [hierbaBuena, hierbaBuena, hierbaBuena, alcachofa]

-- b) Hacer el reduceFatFast, (que viene en distintas potencias) y es un medicamento compuesto por una hierbaVerde de “obesidad” 
-- y tantas alcachofas como indique su potencia.
-- Por ejemplo administrándole a Huesudo un reduceFatFast de potencia 1 hace que huesudo pase a pesar 9 kg y sólo quede
-- con sinusitis. Si en lugar de la 1 le administramos un reduceFatFast de potencia 2, pasa a pesar 8.1 kg y queda también 
-- solo con sinusitis.

reduceFatFast :: Number -> Medicamento
reduceFatFast potencia = hierbaVerde "obesidad" : replicate potencia alcachofa  

-- c) Hacer la pdepCilina, que es un medicamento que usa hierbasVerdes para curar todas las enfermedades infecciosas. 
-- Las enfermedades infecciosas son aquellas cuyo nombre termina 