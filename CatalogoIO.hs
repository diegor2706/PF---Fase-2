-------------------------------------------------------------------------------
-- PRACTICA: Gestion de Catalogo de Contenido Digital (FASE 2 - Entrada / Salida)
-- Num. del equipo registrado en la egela: PF11
-- Apellidos del primer integrante: Ander Etxegarai Arbelaiz
-- Apellidos del segundo integrante: Diego Román Pose
-------------------------------------------------------------------------------

-- Funcionalidades de Entrada / Salida y Parsing.
module CatalogoIO
  ( cargarCatalogoDesdeFichero
  , imprimirCatalogoConsola        
  , mainCatalogo
  , main
  ) where

import CatalogoCD
import Data.Char (isSpace)
import Text.Read (readMaybe)


-- FUNCIONES AXULIARES PARA EL PARSEO DE CANCIONES.

-- Función que elimina espacios en blanco al principio y al final de un string.
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- Función que divide una cadena usando un caracter separador y devuelve una lista de subcadenas.
splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy delmin (c:cs)
    | c == delmin = "" : rest
    | otherwise   = (c : head rest) : tail rest
    where
        rest = splitBy delmin cs

-- Parsea una línea individual de texto en una Canción.
parsearCancion :: String -> Maybe Cancion
parseCancion linea =
  case map trim (splitBy ';' linea) of
    [titulo, artista, durStr, genStr]
      | not (null titulo) && not (null artista) ->
          case (readMaybe durStr :: Maybe DuracionS, readMaybe genStr :: Maybe GeneroC) of
            (Just dur, Just gen) | dur >= 0 -> Just (titulo, artista, dur, gen)
            _                               -> Nothing
    _ -> Nothing        

{-
cargarCatalogoDesdeFichero:
1. Lee el contenido entero de un fichero de texto.
2. Divide el contenido en lineas.
3. Para cada línea, parsea los datos y construir una Canción válida.
Deberá manejar posibles errores de formato en el fichero, (ej. usando Maybe para
indicar fallos en el parseo o error con mensajes claros)
-}

cargarCatalogoDesdeFichero :: FilePath -> IO [Cancion]



{-
imprimirCatalogoConsola:
Tomará la lista de contenido parseada y la imprimirá en pantalla de forma legible, 
utilizando la función printCancion y putStrLn y putStr.
Deberá incluir un encabezado y numeración para cada elemento.
-}
imprimirCatalogoConsola :: [Cancion] -> IO ()

{-
mainCatalogo: Función principal que:
1. Solicitará al usuario el nombre del fichero de entrada.
2. Cargar y parsear el catálogo de dicho fichero.
3. Demostrar al menos dos funcionalidades de filtrado / ordenación  de la Fase 1.
-}
mainCatalogo :: IO ()
