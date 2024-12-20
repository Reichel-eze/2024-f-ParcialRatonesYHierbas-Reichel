module Library where
import PdePreludat
import GHC.Num (Num)

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
administrarMedicamento medicamento raton = foldl (flip aplicarHierba) raton medicamento

aplicarHierba :: Hierba -> Raton -> Raton
aplicarHierba hierba = hierba  

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
-- Las enfermedades infecciosas son aquellas cuyo nombre termina de alguna de estas formas (utilizar esta constante):

sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

pdepCilina :: Medicamento
pdepCilina = map hierbaVerde sufijosInfecciosas 

-- 4) Experimento: Los laboratorios antes de publicar un medicamento, lo prueban con distintos ratones para evaluar 
-- los resultados:

-- a) Hacer la función que encuentra la cantidadIdeal. Recibe una condición y dice cuál es el primer número natural 
-- que la cumple.
-- > cantidadIdeal even           > cantidadIdeal (>5)
-- 2                              6

--cantidadIdeal :: (Num a, Enum a, Ord a, Eq a) => (a -> Bool) -> a
cantidadIdeal :: (Number -> Bool) -> Number
cantidadIdeal condicion = head (filter condicion [1..])

-- b) Saber si un medicamento lograEstabilizar una comunidad de ratones. Esto sucede cuando, luego de aplicarle el
-- medicamento a todos los ratones de la comunidad, se elimina el sobrepeso y todos tienen menos de 3 enfermedades. 
-- Un ratón tiene sobrepeso si pesa más de 1kg.

lograEstabilizar :: Medicamento -> [Raton] -> Bool
lograEstabilizar medicamento = all (\raton -> esRatonEstable raton) . map (administrarMedicamento medicamento)
-- 1ero. Transformo a todos los ratones, administrandoles el mismo medicamento a todos
-- 2dos. Luego verifico si todos son ratones estables (no tienen sobrepeso y tienen menos de 3 enfermedades)

lograEstabilizar' :: Medicamento -> [Raton] -> Bool
lograEstabilizar' medicamento = all (\raton -> not(tieneSobrepreso raton) && menosDeTantasEnfermedades 3 raton) . map (administrarMedicamento medicamento)

aplicarMedicamentoARatones :: Medicamento -> [Raton] -> [Raton]
aplicarMedicamentoARatones medicamento = map (administrarMedicamento medicamento)

esRatonEstable :: Raton -> Bool
esRatonEstable raton = not (tieneSobrepreso raton) && menosDeTantasEnfermedades 3 raton

tieneSobrepreso :: Raton -> Bool
tieneSobrepreso = (> 1) . peso

menosDeTantasEnfermedades :: Number -> Raton -> Bool
menosDeTantasEnfermedades valor = (< valor) . length . enfermedades 

-- c) Diseñar el siguiente experimento: dado una comunidad de ratones, encontrar la potencia ideal del reduceFatFast 
-- necesaria para estabilizar la comunidad.

experimento :: [Raton] -> Number
experimento ratones = cantidadIdeal (\potencia -> lograEstabilizar (reduceFatFast potencia) ratones)

-- 5) Queremos saber si un medicamento logra estabilizar una comunidad infinita. ¿Podemos saberlo? Responder en estos dos casos:
-- a. Si todos los ratones quedan con menos de 1kg y sin enfermedades. Justificar.
-- b. Si un ratón queda con 2kg y 4 enfermedades. Justificar.

-- Tenemos un comunidad infinita de ratones. Si queres saber si un medicamento lograEstabilizar tenemos que chequear/verificar
-- "todos" cada uno de los ratones luego de aplicarles el medicamento. Pero hay una cosa a tener en cuente, que en algunos
-- casos no sera necesario evaluar como quedan todoso los ratones

-- El el caso a. ya nos dicen que todos los ratones pertenecientes a la comunidad que con menos de 1kg y sin enfermedades
-- (es decir, a priori ambas condiciones del all verifican). Pero el problema es que el haskell tiene que chequear TODOS los
-- ratones de la lista, porque nadie le garantiza que existe un raton que no cumpla con alguna de las condiciones (el problema
-- radica en que tenemos que chequear de una LISTA INFINITA si TODOS cumplen). Por lo tanto para el primer caso nos va a saltar
-- un ERROR (NO es posible saber si el medicamento logra estabilizar la comuunidad)

-- El el caso b. nos adelantan que hay un raton que NO cumple con las condiciones, por lo tanto para haskell ya es suficiente
-- para retornar un FALSE (es decir, no va a seguir buscando/recorriendo la lista para ver los demas ratones). Esto es gracias
-- a la evaluacion perezosa (lazy evaluation) con la cual trabaja haskell que le permite NO evaluar toda la lista para
-- llegar a una concluison . Por lo tanto para el segundo caso nos va a retornar un False 
-- (Es posible saber si el medicamento logra estabilizar la comuunidad, que en este caso es False)

-- 6) Responder en base al ejercicio
-- ¿Qué cambios debería hacer para agregar una nueva hierba y construir un medicamento con ella? 
-- ¿Habría que modificar las funciones existentes? 

-- Simplemente se deberia agregar la hierba de la misma forma que fueron agregadas en su momento las hierbas mencionadas
-- Tendria que respetar el tipo "Hierba" y al respetar ese tipo se puede utilizar en las diferentes funciones
-- No habria que modificar ninguna de las funciones existentes ya que fueron realizadas para utilizar cualquier tipo
-- de hierba. 

-- ¿Qué concepto está involucrado en la pregunta anterior? ¿Para qué sirve en este caso?

-- El concepto que está involucrado en la pregunta anterior es el de TAD, en donde se toma a la Hierba
-- como un valor, y en particular la función "componer" como una primitiva. Si cambia el modelado de las 
-- hierbas, no hace falta cambiar ninguno de los medicamentos.

-- Si se cambia el modelo del ratón, por ejemplo, ahora queremos que se registre el peso de un ratón siempre en libras. 
-- ¿Qué funciones habría que modificar?	

-- Si se cambia el peso de un raton a libras, habria que modificar todas aquellas funciones que realicen calculos o
-- comparaciones con el peso del raton ya que estar puestas con valores en kg

-- Por ejemplo la de tieneSobrepreso ya no seria mayor a 1kg, ahora seria 1 libra