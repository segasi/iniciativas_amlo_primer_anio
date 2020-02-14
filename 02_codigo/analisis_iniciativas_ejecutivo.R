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

### Generar variables para definir estatus simplificado de las iniciativas al (i) año y (ii) mes 14 ----
bd_iniciativas <- 
  bd_iniciativas %>% 
  mutate(estatus_al_año = case_when(str_detect(estatus_primer_año, "Publicado") ~ "Publicado en el DOF",
                                    str_detect(estatus_primer_año, "Pendiente") ~ "Pendiente",
                                    str_detect(estatus_primer_año, "Devuelto") ~ "Pendiente",
                                    TRUE ~ estatus_primer_año),
         estatus_al_año = fct_relevel(estatus_al_año, "Publicado en el DOF", "Aprobado, por publicar en el DOF", "Pendiente", "Desechado"),
         estatus_al_mes_14 = case_when(str_detect(estatus_mes_14, "Publicado") ~ "Publicado en el DOF",
                                       str_detect(estatus_mes_14, "Pendiente") ~ "Pendiente",
                                       str_detect(estatus_mes_14, "Devuelto") ~ "Pendiente",
                                       TRUE ~ estatus_mes_14),
         estatus_al_mes_14 = fct_relevel(estatus_al_mes_14, "Publicado en el DOF", "Aprobado, por publicar en el DOF", "Pendiente", "Desechado"))


### Generar versión corta del nombre de cada presidente y reordenar niveles tanto de presidente_corto como de presidente ----
bd_iniciativas <- 
  bd_iniciativas %>% 
  mutate(presidente_corto = case_when(presidente == "Vicente Fox Quezada" ~ "Fox",
                                      presidente == "Felipe Calderón Hinojosa" ~ "Calderón", 
                                      presidente == "Enrique Peña Nieto" ~ "Peña Nieto",
                                      presidente == "Andrés Manuel López Obrador" ~ "López Obrador"), 
         presidente_corto = fct_relevel(presidente_corto, "Fox", "Calderón", "Peña Nieto", "López Obrador"),
         presidente = fct_relevel(presidente, "Vicente Fox Quezada", "Felipe Calderón Hinojosa", "Enrique Peña Nieto", "Andrés Manuel López Obrador"))

### Reordenar niveles de subclasificacion ----
bd_iniciativas <- 
  bd_iniciativas %>% 
  mutate(subclasificacion = fct_relevel(subclasificacion, "Ley Secundaria", "Reforma Constitucional"))


### Gráfica del número de iniciativas presentadas por cada presidente en los primeros 14 meses de gobierno ----

# Datos
bd_iniciativas %>% 
  group_by(presidente_acro, subclasificacion) %>% 
  summarise(numero = n()) %>% 
  ungroup() %>% 
  group_by(presidente_acro) %>% 
  mutate(total = sum(numero)) %>% 
  ungroup()

# Gráfica
bd_iniciativas %>% 
  group_by(presidente_acro, subclasificacion) %>% 
  summarise(numero = n()) %>% 
  ungroup() %>% 
  group_by(presidente_acro) %>% 
  mutate(total = sum(numero),
         presidente_acro_grafica = str_c(presidente_acro, "\n (", total, ")")) %>% 
  ungroup() %>%
  mutate(presidente_acro_grafica = fct_relevel(presidente_acro_grafica, "Fox\n (42)", "Calderón\n (29)", "Peña Nieto\n (41)", "López Obrador\n (16)")) %>% 
  ggplot(aes(x = presidente_acro_grafica, y = numero, fill = subclasificacion)) +
  geom_col() +
  geom_hline(yintercept = seq(5, 40, 5), color = "white", linetype = 3) +
  
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 40, 5), limits = c(-1, 42), expand = c(0, 0)) +
  scale_fill_manual(values = c("steelblue", "salmon")) +
  labs(title = str_wrap("Número de iniciativas de reforma constitucional y a leyes secundarias presentadas por los últimos cuatro presidentes de México en los primeros 14 meses de su gobierno", width = 70),
       x = NULL,
       y = "Número\n",
       caption = "\n   @segasi / Fuente: Sistema de Información Legislativa de SEGOB.\n\n   Datos al 31 de enero de 2002, 2008, 2014 y 2020, respectivamente.",
       fill = NULL) +
  tema +
  theme(panel.grid = element_blank(),
        legend.position = c(0.885, 0.9)) +
  ggsave("03_graficas/numero_iniciativas_primeros_14_meses_por_tipo.png", width = 14, height = 9, dpi = 200)


