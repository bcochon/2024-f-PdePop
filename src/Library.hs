module Library where
import PdePreludat

data Cancion = UnaCancion {
    titulo :: String,
    duracion :: Number,
    instrumentos :: [Instrumento]
} deriving (Eq, Show)

data Instrumento = Guitarra | Bajo | Bateria | Teclado deriving (Show, Eq)

-- FUNCIONES

esPar :: Number -> Bool
esPar n = (rem n 2) == 0

esAcapella :: Cancion -> Bool
esAcapella cancion = null (instrumentos cancion)

aceptacion :: Cancion -> Number
aceptacion cancion
     | head (titulo cancion) == 'M' = 500
     | esPar (duracion cancion) = length (titulo cancion) *10
     | esAcapella cancion = 10
     | otherwise = 0

ordenCanciones :: Cancion -> Cancion -> String
ordenCanciones cancion1 cancion2
    | titulo cancion1 > titulo cancion2 = titulo cancion2
    | otherwise = titulo cancion1

aceptada :: Cancion -> Bool
aceptada cancion = aceptacion cancion > 60

necesitaInstrumento :: Cancion -> Instrumento -> Bool
necesitaInstrumento cancion instrumento = elem instrumento (instrumentos cancion)

tocarCancion :: Cancion -> Number
tocarCancion cancion
    | aceptada cancion = duracion cancion
    | otherwise = duracion cancion / 2

-- CANCIONES
patternMatching :: Cancion
patternMatching = UnaCancion "Pattern Matching" 4 [Guitarra, Bajo, Bateria]

seisDieciocho :: Cancion
seisDieciocho = UnaCancion "Seis dieciocho" 3 [Teclado, Guitarra]

vidaEnHaskell :: Cancion
vidaEnHaskell = UnaCancion "La vida en Haskell" 4 []

song2 :: Cancion
song2 = UnaCancion "song 2" 2 [Guitarra, Bajo, Bateria]

dovahkiin :: Cancion
dovahkiin = UnaCancion "Dovahkiin" 4 []

macarena :: Cancion
macarena = UnaCancion "Macarena" 5 [Guitarra, Bajo]