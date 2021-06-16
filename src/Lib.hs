module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

doble :: Int->Int
doble= (*2)

--De antemano, nos piden que modelemos a los ladrones y los rehenes.

--Sabemos que el atraco lo realizan ladrones de profesión, de los cuales se conocen su nombre, habilidades
-- y las armas que lleva.

type Arma :: Rehen->Rehen

data Ladron = Ladron {
    nombreLadron :: String,
    habilidades :: [String],
    armas :: [Arma]
}


--De los rehenes conocemos su nombre, su nivel de complot, su nivel de miedo, y su plan contra los ladrones, 
--la cual puede involucrar a algún otro rehén.

type Plan :: Ladron->Ladron

data Rehen =  Rehen {
    nombreRehen :: String,
    nivelComplot :: Int,
    nivelMiedo :: Int,
    planes :: [Plan]
}


{--
Las únicas armas que llegaron a conseguir con su presupuesto son pistolas y ametralladoras, 
pero seguramente pueden conseguir más en el futuro:
Pistola: reduce el nivel de complot de un rehén en 5 veces su calibre, y aumenta su miedo en 3 por la cantidad de 
letras de su nombre

Ametralladora: siempre reduce el nivel de complot a la mitad, y aumenta su miedo en la cantidad de balas que le quedan.
--}

pistola :: Arma
pistola unRehen = unRehen {nivelComplot= (-5) (nivelComplot unRehen), nivelMiedo= (*) ((+3) (nivelMiedo unRehen))  (length (nombre unRehen))}  

ametralladora :: Int->Arma
ametralladora balas unRehen = unRehen {nivelComplot= (/2) (nivelComplot unRehen), nivelMiedo= (+ balas) (nivelMiedo unRehen) }   

{--
Nuestros carismáticos ladrones pasaron meses preparándose para ejecutar este plan, 
durante los cuales acordaron formas de intimidar a los rehenes para desalentarlos de rebelarse:
Disparos: disparar al techo como medida disuasiva. Se usa el arma que le genera más miedo al rehén intimidado.
Hacerse el malo: 
Cuando el que se hace el malo es Berlín, aumenta el miedo del rehén tanto como la cantidad de letras que sumen sus habilidades.
Cuando Río intenta hacerse el malo, le sale mal y en cambio aumenta el nivel de complot del rehén en 20. 
En otros casos, el miedo del rehén sube en 10. 
--}

disparos :: Ladron -> Rehen -> Rehen
disparos unLadron unRehen = unrehen {nivelMiedo= maximum (map ($ unRehen) (armas unLadron))}

hacerseElMalo :: Ladron -> Rehen -> Rehen
hacerseElMalo unLadron { nombre= "Berlin" }  unRehen = unRehen {nivelMiedo= (+) (sum.(map length).habilidades $ unLadron)  nivelMiedo  }  
hacerseElMalo unLadron { nombre= "Rio" }  unRehen =  unRehen {nivelComplot=(+20) nivelComplot }
hacerseElMalo unLadron { nombre=_ }  unRehen = unRehen { nivelMiedo=(+10) nivelMiedo }

{--

A los rehenes no les gusta ser rehenes, por eso intentan rebelarse contra los ladrones, 
siempre que tengan más complot que miedo, ideando planes como:
Atacar al ladrón: le quita tantas armas como la cantidad de letras del nombre de su compañero de ataque, dividido por 10.
Esconderse: Hace que un ladrón pierda una cantidad de armas igual a su cantidad de habilidades dividido 3. 

--}

atacarAlLadron :: Rehen->Plan
atacarAlLadron unRehen unLadron = unLadron {armas= quitarArmas (nombre unRehen) 10 (armas unLadron) } 

quitarArmas :: [a] -> Int -> [Arma] ->  [Arma]
quitarArmas lista divisor armas=   drop (div  (length lista)   divisor ) armas

esconderse :: Plan
esconderse unLadron = unLadron {armas= quitarArmas (habilidades unLadron) 3 (armas unLadron) }


{--
1) Modelar a los siguientes personajes:
tokio, sabe hacer el “trabajo psicológico”, y “entrar en moto”. Lleva dos pistolas calibre 9 milímetros y una ametralladora de 30 balas.
profesor, sabe “disfrazarse de linyera”, “disfrazarse de payaso” y “estar siempre un paso adelante”. No tiene armas
pablo, el cual tiene 40 de complot y 30 de miedo. Su plan es esconderse.
arturito, tiene 70 de complot y 50 de miedo. Su plan es esconderse y luego atacar con pablo.

--}

tokio = Ladron {nombre="Tokio",habilidades= ["trabajo psicológico", "entrar en moto"], armas=[ametralladora 30 ,pistola,pistola]}
profesor = Ladron {nombre="Profesor",habilidades= ["disfrazarse de linyera", "disfrazarse de payaso", "estar siempre un paso adelante"], armas=[]}
pablo = Rehen {nombre="Pablo",nivelComplot=40,nivelMiedo=30,planes=[esconderse]}
arturito = Rehen {nombre="Arturito",nivelComplot=70,nivelMiedo=50},planes=[esconderse, atacarAlLadron pablo]}

{--
2) Saber si un ladrón es inteligente. Ocurre cuando tiene más de dos habilidades, además el Profesor es la mente maestra, 
por lo que indudablemente es inteligente.

--}

esInteligente :: Ladron -> Bool
esInteligente unLadron | (==) (nombre unLadron) "Profesor" = True
                       |  otherwise = (>=2).length.habilidades $ unLadron


-- 3) Que un ladrón consiga un arma nueva, y se la agregue a las que ya tiene.


conseguirArma :: Arma -> Ladron -> Ladron
conseguirArma armaNueva unLadron = unLadron {armas = (:) armaNueva (armas unLadron)  }

--4) Que un ladrón intimide a un rehén, usando alguno de los métodos planeados.

intimidar :: Ladron->Rehen-> (Ladron->Rehen->Rehen) -> Rehen
intimidar unLadron unRehen metodo = metodo unLadron unRehen 

-- 5) Que un ladrón calme las aguas, disparando al techo frente a un grupo de rehenes, de los cuales se calman los
-- que tengan más de 60 de complot.

calmarLasAguas ::  Ladron -> [Rehen] -> [Rehen]
calmarLasAguas unLadron grupoRehenes = map (disparos unLadron)  ( filter (>60).nivelComplot) grupoRehenes )

-- 6) Saber si un ladrón puede escaparse de la policía. Esto se cumple cuando alguna de las
-- habilidades del ladrón empieza con “disfrazarse de”.
palabra :: String
palabra = "disfrasarse de"

puedeEscapar :: Ladron -> Bool
puedeEscapar unLadron = any  ( (== palabra).(take (length (palabra)))) (habilidades unLadron)

{--
7) Saber si la cosa pinta mal, que es cuando dados unos ladrones y unos rehenes, 
el nivel de complot promedio de los rehenes es mayor al nivel de miedo promedio multiplicado por la cantidad de armas de los ladrones.
--}

laCosaPintaMal :: [Ladron] -> [Rehen] -> Bool
laCosaPintaMal ladrones rehenes = (>) (nivelPromedio nivelComplot rehenes) ((* (sum.(map length).(map armas) $ ladrones)).(nivelPromedio nivelMiedo rehenes))

nivelPromedio :: Num a => (Rehen->Int)-> [Rehen] -> a 
nivelPromedio nivel rehenes = (/ (length rehenes)).sum $ (map nivel rehenes)

-- 8) Que los rehenes se rebelen contra un ladrón, usando el plan que tengan en mente. Saben que es mala idea, 
--     por lo que todos pierden 10 de complot antes de comenzar la rebelión. 

rebelarse :: [Rehen] -> Ladron -> Ladron
rebelarse rehenes unLadron =  foldr unLadron (map (head.planes) (map perderComplot rehenes)) 

perderComplot :: Rehen -> Rehen
perderComplot unRehen = unRehen {nivelComplot= (-10) (nivelComplot unRehen) }


 {-- 9) Ejecutar el Plan Valencia, que consiste en escapar con la mayor cantidad de dinero posible. 
 El dinero conseguido, es igual a $1000000, multiplicado por la cantidad de armas que tengan todos los ladrones en total, luego de que:
(*)se armen todos con una ametralladora de 45 balas
(*)todos los rehenes se rebelen contra todos los ladrones

 --}

planValencia:: [Ladron] -> [Rehen] -> [Ladron]
planValencia ladrones rehenes =  (rebelionGrupal rehenes) .(map armarse) $ ladrones

armarse :: Ladron -> Ladron 
armarse unLadron = unLadron {armas= (:) (ametralladora 45) (armas unLadron)}

rebelionGrupal :: [Rehen] -> [Ladron] -> [Ladron]
rebelionGrupal rehenes ladrones = map (rebelarse rehenes) ladrones 



-- 11) Depende. Si es un plan como esconderse, que requiere recorrer la lista de habilidades entera (debido a la funcion length) 
-- entonces no se puede. Pero si se trata de otro plan que no involucre a la lista de habilidades o que no requiera recorrerla completa
-- entonces sí es posible ejecutarlo.

-- 10)  No es posible ya que nunca se podría actualizar la lista entera de las armas de los ladrones aunque se le quiten una cantidad
-- finita de las mismas

