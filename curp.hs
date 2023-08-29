import Data.Char

--Función que recibe un String y devuelve su segundo caracter
segundo:: String -> Char
segundo [] = error ("Deberías pasar una cadena con al menos dos caracteres")
segundo [x] = error ("Deberías pasar una cadena con al menos dos caracteres")
segundo (x:(y:ys)) = y

{-
Función que calcula el curp de una persona.
Los parámetros son:
Nombre, Apellido Paterno, Apellido Materno, 
  Día de nacimiento, Mes de nacimiento, Año de nacimiento,
  Char con el sexo biológico: 'M' para mujer, 'H' para hombre,
  Nombre del estado en el que nació.	
-}
getCURP :: String -> String -> String -> Int -> Int -> Int -> Char -> String -> String
getCURP nombre apellidoP apellidoM dia mes anio sexo estado = curpAux ([(head apellidoP)] ++ [(segundo apellidoP)]++ [(head apellidoM)] ++ [(head nombre)] ++ (mesCURP (mod anio 100)) ++ (mesCURP mes) ++ (show dia) ++ [sexo] ++ (estadoCURP estado)++"06")


curpAux :: String -> String
curpAux curp = map toUpper curp



mesCURP :: Int -> String
mesCURP mes = if mes < 10 then ('0': x) else x
 where x = (show mes)

estadoCURP:: String -> String
estadoCURP x | x == "Aguascalientes" = "AS"
             | x == "Baja California" = "BC"
             | x == "Baja California Sur" = "BS"
             | x == "Campeche" = "CC"
             | x == "Estado De México" = "EM"
             | otherwise = "XX"

--Roberto Martínez Vázquez 21 8 2004 H Estado de México
--MAVR040821HEMRZB01

--Faltan las segundas consonantes del curp





