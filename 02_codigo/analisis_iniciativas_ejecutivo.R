### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/paquetes_setup_tema.R")

### Importar datos ----
bd_iniciativas <- 
  read_excel("01_datos/bd_iniciativas_primeros_14_meses.xlsx",
             col_types = c("text", "text", "text", 
                           "text", "text", "text", "text", "text", 
                           "date", "text", "text", "text", "text", 
                           "text")) %>% 
  clean_names()


# Base de datos construida con información de http://sil.gobernacion.gob.mx/portal