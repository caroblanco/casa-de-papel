module Lib where
import Text.Show.Functions

data Ladron = UnLadron {
    nombreL :: String,
    habilidades:: [String],
    armas :: [Arma]
}

data Rehen = UnRehen {
    nombreR :: String,
    nivelComplot::Int,
    nivelMiedo::Int,
    plan :: Ladron->Ladron
}

type Arma = Rehen -> Rehen

--------
pistola :: Int -> Arma
pistola calibre = subirMiedo (\rehen->((*3).length.nombreR)rehen) . cambiarComplot (\nivel -> nivel - 5*calibre)
 
subirMiedo :: (Rehen->Int) -> Rehen -> Rehen
subirMiedo f rehen = rehen {nivelMiedo = f rehen + (nivelMiedo rehen)}

cambiarComplot :: (Int->Int) -> Rehen -> Rehen
cambiarComplot f rehen = rehen {nivelComplot = f (nivelComplot rehen)}

cambiarArmas :: ([Arma] -> [Arma]) -> Ladron -> Ladron
cambiarArmas f ladron = ladron {armas = f (armas ladron)}

ametralladora :: Int -> Arma
ametralladora balas = cambiarComplot (\nivel -> nivel - (div 2 nivel)) . subirMiedo (\_->balas)

---------------
type Intimidar = Ladron -> Rehen -> Rehen
disparos :: Ladron -> Rehen -> Rehen
disparos ladron rehen = undefined
    
    --foldl1 (((>).miedo) rehen) (armas ladron)

hacerseElMalo :: Ladron -> Rehen -> Rehen
hacerseElMalo (UnLadron "Berlin" habs _) rehen = aumentaMiedoB habs rehen
hacerseElMalo (UnLadron "Rio" _ _) rehen = cambiarComplot (\nivel -> nivel + 20) rehen
hacerseElMalo (UnLadron _ _ _) rehen = subirMiedo (\_ -> 10) rehen

aumentaMiedoB :: [String] -> Rehen -> Rehen
aumentaMiedoB habs = subirMiedo (\_-> (sum . map length)habs)

-------------------
atacarLadron :: Rehen -> Ladron -> Ladron
atacarLadron rehen = quitarArmas ((length.nombreR)rehen) 

esconderse :: Ladron -> Ladron
esconderse ladron = quitarArmas (((div 3).cantHabilidades)ladron) ladron

quitarArmas :: Int -> Ladron -> Ladron
quitarArmas cant= cambiarArmas (\armas -> drop cant armas)

---------------------
--1
tokio = UnLadron {nombreL = "Tokio", habilidades = ["trabajo psicológico", "entrar en moto"], armas = [pistola 9, pistola 9, ametralladora 30]} 
profesor = UnLadron {nombreL = "Profesor", habilidades = ["disfrazarse de linyera", "disfrazarse de payaso", "estar siempre un paso adelante"], armas = []} 
pablo = UnRehen {nombreR = "Pablo", nivelComplot = 40, nivelMiedo = 30, plan = esconderse} 
arturito = UnRehen {nombreR = "Arturito", nivelComplot = 70, nivelMiedo = 50, plan = atacarLadron pablo.esconderse} 

--2
esInteligente :: Ladron -> Bool
esInteligente ladron = quienEs "Profesor" ladron || cantHabilidades ladron > 2

quienEs :: String -> Ladron -> Bool
quienEs  nombre = (==nombre). nombreL

cantHabilidades :: Ladron -> Int
cantHabilidades = length.habilidades

--3
conseguirArmaNueva :: Arma -> Ladron -> Ladron
conseguirArmaNueva arma = cambiarArmas (\armas -> armas ++ [arma])

--4
intimidar :: (Intimidar) -> Ladron -> Rehen -> Rehen
intimidar metodo ladron = metodo ladron

--5
calmarLasAguas :: Ladron -> [Rehen] -> [Rehen]
calmarLasAguas ladron = filter ((>60).nivelComplot) . map (disparos ladron) 

--6
puedeEscaparse :: Ladron -> Bool
puedeEscaparse =  any (empiezaCon "disfrazarse de") . habilidades

empiezaCon :: Eq a => a -> [a] -> Bool
empiezaCon inicio = (==inicio). take (length inicio)

--7
laCosaPintaMal :: [Ladron] -> [Rehen] -> Bool
laCosaPintaMal ladrones rehenes = complotPromedio rehenes > miedoPromedio rehenes ladrones

complotPromedio :: [Rehen] -> Int
complotPromedio rehenes = (div . length ). sumaNivelComplot

sumaNivelComplot :: [Rehen] -> Int
sumaNivelComplot rehenes = foldl1 ((+).nivelComplot) rehenes
