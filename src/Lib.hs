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
data Ladron {
    nombre=String
    habilidades=[Habilidad]
    armas= [Armas]
}


--De los rehenes conocemos su nombre, su nivel de complot, su nivel de miedo, y su plan contra los ladrones, 
--la cual puede involucrar a algún otro rehén.
