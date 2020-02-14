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

### Renombrar variables ----
bd_iniciativas <- 
  bd_iniciativas %>% 
  rename(estatus_primer_año = estatus_al_final_del_primer_ano_de_la_administracion,
         estatus_mes_14 = estatus_al_final_del_mes_14_de_la_administracion)

### Transformar el tipo de la variable fecha_de_presentacion para que sea date ----
bd_iniciativas <- 
  bd_iniciativas %>% 
  mutate(fecha_de_presentacion = dmy(fecha_de_presentacion)) 