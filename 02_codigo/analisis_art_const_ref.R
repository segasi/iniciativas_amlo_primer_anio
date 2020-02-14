### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/paquetes_setup_tema.R")

### Importar datos ----
art_ref <- 
  read_excel("01_datos/articulos_constitucion_reformados.xlsx") %>% 
  mutate(fecha_publicacion = as_date(fecha_publicacion))

# Base de datos construida con base en la información de esta página: http://www.diputados.gob.mx/LeyesBiblio/ref/cpeum_crono.htm


### Calcular número de artículos reformados en cada admin ----

# Esto comprende los primeros 14 meses de cada administración
art_ref %>% 
  group_by(presidente) %>% 
  summarise(numero = n()) %>% 
  ungroup()

