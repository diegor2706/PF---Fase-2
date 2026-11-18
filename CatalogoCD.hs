-------------------------------------------------------------------------------
-- PRACTICA: Gestion de Catalogo de Contenido Digital (FASE 2 - Dominio)
-- Num. del equipo registrado en la egela: PF11
-- Apellidos del primer integrante: Ander Etxegarai Arbelaiz
-- Apellidos del segundo integrante: Diego Román Pose
-------------------------------------------------------------------------------
module CatalogoCD
  ( Cancion
  , Titulo
  , Artista
  , DuracionS
  , DuracionM
  , GeneroC(..)
  , getTitulo
  , getArtista
  , getDuracionS
  , getGeneroC
  , sToM
  , printCancion
  , printCanciones
  , qsortBy
  , contenidoXGeneroMusical
  , contenidoXArtista
  , titCancionesXArtista
  , totalMinutosMusicales
  , todosLosArtistasEnlistadoC
  , artistaMasProlifico
  , rankingGenerosCPorMinutos
  , generosCSinRepresentacion
  , miSeleccionDeCancionesCortas
  ) where

import Data.List (nub, maximumBy)
import Data.Function (on)

type Cancion   = (Titulo, Artista, DuracionS, GeneroC)
type Titulo    = [Char]
type Artista   = [Char]
type DuracionS = Int
type DuracionM = Int

-- Añadimos Read para permitir el parseo seguro desde texto
data GeneroC = Afro | Country | Electro | KPop | Latina | Pop | Rap | Rock
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- ====================================
-- Funciones extractoras y auxiliares BASICAS: Canciones
-- ====================================
getTitulo :: Cancion -> Titulo
getTitulo (t, _, _, _) = t

getArtista :: Cancion -> Artista
getArtista (_, a, _, _) = a

getDuracionS :: Cancion -> DuracionS
getDuracionS (_, _, d, _) = d

getGeneroC :: Cancion -> GeneroC
getGeneroC (_, _, _, g) = g

sToM :: DuracionS -> DuracionM
sToM s = div s 60

printCanciones :: [Cancion] -> IO ()
printCanciones cs = putStr (concat (map printCancion cs))

printCancion :: Cancion -> String
printCancion (t, a, d, g) =
  "Titulo: " ++ t ++ "\nArtista: " ++ a ++ "\nDuracion: " ++ show d ++ " segundos\nGenero: " ++ show g ++ "\n\n"

qsortBy :: Ord b => (a -> b) -> [a] -> [a]
qsortBy f [] = []
qsortBy f (x:xs) = qsortBy f smaller ++ [x] ++ qsortBy f larger
  where
    smaller = [y | y <- xs, f y <= f x]
    larger  = [y | y <- xs, f y > f x]

-- ====================================
-- Funciones principales sobre Canciones
-- ====================================

contenidoXGeneroMusical :: GeneroC -> [Cancion] -> [Cancion]
contenidoXGeneroMusical g = filter ((== g) . getGeneroC)

contenidoXArtista :: Artista -> [Cancion] -> [Cancion]
contenidoXArtista a = filter ((== a) . getArtista)

titCancionesXArtista :: Artista -> [Cancion] -> [Titulo]
titCancionesXArtista a = map getTitulo . contenidoXArtista a

totalMinutosMusicales :: [Cancion] -> DuracionM
totalMinutosMusicales = sToM . sum . map getDuracionS

todosLosArtistasEnlistadoC :: [Cancion] -> [Artista]
todosLosArtistasEnlistadoC = nub . map getArtista

artistaMasProlifico :: [Cancion] -> Artista
artistaMasProlifico xs = fst (maximumBy (compare `on` snd) artistaCanciones)
  where
    artistas = nub (map getArtista xs)
    artistaCanciones = map (\a -> (a, length (contenidoXArtista a xs))) artistas

rankingGenerosCPorMinutos :: [Cancion] -> [(GeneroC, DuracionM)]
rankingGenerosCPorMinutos cs =
  qsortBy snd [(g, totalMinutosMusicales (contenidoXGeneroMusical g cs)) | g <- [minBound .. maxBound]]

generosCSinRepresentacion :: [Cancion] -> [GeneroC]
generosCSinRepresentacion xs = filter (`notElem` presentes) [minBound .. maxBound]
  where
    presentes = map getGeneroC xs

miSeleccionDeCancionesCortas :: Int -> DuracionS -> [Cancion] -> [Cancion]
miSeleccionDeCancionesCortas n dm = take n . filter ((<= dm) . getDuracionS)