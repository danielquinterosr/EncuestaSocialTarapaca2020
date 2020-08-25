#Importacion y depuracion de datos

library(readr)
Encuesta_Social_Tarapaca_2020_el_impacto_del_Covid_19 <- read_csv("Encuesta Social Tarapacá 2020: el impacto del Covid-19.csv")
View(Encuesta_Social_Tarapaca_2020_el_impacto_del_Covid_19)

est2020 <- Encuesta_Social_Tarapaca_2020_el_impacto_del_Covid_19

library(dplyr) #dplyr hacer múltiples transformaciones sobre los datos, agregando funciones con el operador "%>%" (pipe) 
#vamos a filtrar únicamente a las personas que respondieron y aceptaron el Consentimiento Informado
est2020 <- est2020 %>% 
  filter(`Para garantizar que has comprendido y aceptado las condiciones de participación en el estudio, por favor indica tu preferencia`=="Sí, he comprendido las condiciones y acepto participar")

est2020$a1.trabajo <- factor(est2020$`A1. Indique si durante la semana pasada, es decir, entre lunes y domingo, trabajó remuneradamente al menos una hora en alguna de las siguientes actividades:`,
                             levels = c("Trabajo dependiente",
                                        "Trabajo independiente o por cuenta propia",
                                        "Empresa o negocio (individual o familiar)",
                                        "No trabajé remuneradamente durante la semana pasada"))

est2020$a2.contrato <- factor(est2020$`A2. Si está trabajando ¿qué tipo de contrato posee en la actualidad?`,
                              levels = c("Contrato indefinido",
                                         "Contrato plazo fijo",
                                         "Planta o Contrata (estatuto administrativo)",
                                         "Contrato a honorarios",
                                         "Independiente (dueño o socio de empresas, sociedades u otros negocios)",
                                         "No tengo contrato de trabajo"))

est2020$a3.empleador <- factor(est2020$`A3. El negocio, empresa o institución donde trabajó la semana pasada era...`,
                               levels = c("Estatal",
                                          "Privada",
                                          "Hogar particular"))

est2020$a4.areatrabajo <- factor(est2020$`A4. Favor indique el área económica que mejor describe al rubro de la empresa o negocio para el cual trabajó la semana pasada`,
                                 levels = c("Minería",
                                            "Servicios",
                                            "Transporte de carga y/o pasajeros"))

est2020$a5.precovid <- factor(est2020$`A5. Antes de la pandemia Covid-19, ¿se encontraba usted trabajando remuneradamente en alguna de las siguientes actividades?`,
                              levels = c("Trabajo dependiente",
                                         "Trabajo independiente o por cuenta propia",
                                         "Empresa o negocio (individual o familiar)",
                                         "No me encontraba trabajando remuneradamente"))


est2020$a6.motivo <- est2020$`A6. Si perdió su fuente laboral durante la pandemia ¿Cuál fue el motivo principal por el cual terminó?`

est2020$a7.busca <- factor(est2020$`A7. Durante la última semana ¿ha buscado empleo u otra actividad remunerada?`,
                           levels = c("Si",
                                      "No"))

est2020$a8.modalidad <- factor(est2020$`A8. ¿Cuál de las siguientes alternativas describe de mejor forma su modalidad actual de trabajo?`,
                               levels = c("Completamente presencial",
                                          "Mayormente presencial",
                                          "Mayormente con teletrabajo",
                                          "Completamente con teletrabajo"))

est2020$a9.carga <- factor(est2020$`A9. Pensando en la carga laboral ¿usted diría que durante la pandemia ésta...?`,
                           levels = c("Ha disminuido considerablemente",
                                      "Ha disminuido levemente",
                                      "Se ha mantenido igual",
                                      "Ha aumentado levemente",
                                      "Ha aumentado considerablemente"))

est2020$a10.situacion <- factor(est2020$`A10. En general ¿Cómo calificarías tu actual situación laboral?`,
                                levels = c("Muy mala",
                                           "Mala",
                                           "Regular",
                                           "Buena",
                                           "Muy buena"))

est2020$a11.desempeño <- factor(est2020$`A11. ¿Qué tan de acuerdo se siente usted con la siguiente frase?: "la pandemia ha perjudicado significativamente mi desempeño laboral"`,
                                levels = c("Absolutamente en desacuerdo",
                                           "Muy en desacuerdo",
                                           "En desacuerdo",
                                           "De acuerdo",
                                           "Muy de acuerdo",
                                           "Absolutamente de acuerdo"))

#B. Educación
est2020$b1.estudio <- est2020$`B1. Indique si se encuentra usted cursando actualmente alguno de los siguientes niveles de estudio:`
est2020$b1.estudio[est2020$b1.estudio == "Diplomado"] <- "Postgrado"
est2020$b1.estudio[est2020$b1.estudio == "Diplomado * magister"] <- "Postgrado"
est2020$b1.estudio[est2020$b1.estudio == "Doctorado"] <- "Postgrado"
est2020$b1.estudio[est2020$b1.estudio == "Post grado"] <- "Postgrado"
est2020$b1.estudio[est2020$b1.estudio == "Educación universitaria terminada"] <- "Actualmente no me encuentro estudiando" #eliminar respuestas de b2
est2020$b1.estudio[est2020$b1.estudio == "Titulado"] <- "Actualmente no me encuentro estudiando" #eliminar respuestas de b2
est2020$b1.estudio[est2020$b1.estudio == "Ninguno"] <- "Actualmente no me encuentro estudiando" #eliminar respuestas de b2
est2020$b1.estudio[est2020$b1.estudio == "No puedo me falta mi certificado de estudio de mi pais"] <- "Actualmente no me encuentro estudiando" #eliminar respuestas de b2
est2020$b1.estudio[est2020$b1.estudio == "Egresada. En proceso de titulación."] <- "Educación universitaria"
est2020$b1.estudio[est2020$b1.estudio == "Postgrado (Mg)"] <- "Postgrado"
est2020$b1.estudio[est2020$b1.estudio == "Magister"] <- "Postgrado"
est2020$b1.estudio[est2020$b1.estudio == "preuniversitario"] <- "Otra"
est2020$b1.estudio[est2020$b1.estudio == "Acreditación del servicio de salud"] <- "Otra"
est2020$b1.estudio[est2020$b1.estudio == "Curso de especializaciones"] <- "Otra"
est2020$b1.estudio[est2020$b1.estudio == "Curso online"] <- "Otra"
est2020$b1.estudio[est2020$b1.estudio == "Cursos de oficios"] <- "Otra"
est2020$b1.estudio <- factor(est2020$b1.estudio,
                             levels = c("Educación media",
                                        "Educación técnica",
                                        "Educación universitaria",
                                        "Postgrado",
                                        "Otra",
                                        "Actualmente no me encuentro estudiando"))

est2020$b2.material <- factor(est2020$`B2. ¿Qué tan de acuerdo se encuentra usted respecto de las siguientes afirmaciones? [Dispongo de todo el material de apoyo necesario para cada curso o asignatura]`,
                              levels = c("Muy en desacuerdo", 
                                         "En desacuerdo", 
                                         "De acuerdo", 
                                         "Muy de acuerdo"))

est2020$b2.dejar <- factor(est2020$`B2. ¿Qué tan de acuerdo se encuentra usted respecto de las siguientes afirmaciones? [He pensado seriamente en dejar los estudios debido a las condiciones actuales]`,
                           levels = c("Muy en desacuerdo", 
                                      "En desacuerdo", 
                                      "De acuerdo", 
                                      "Muy de acuerdo"))

est2020$b2.adapta <- factor(est2020$`B2. ¿Qué tan de acuerdo se encuentra usted respecto de las siguientes afirmaciones? [He tenido dificultades para adaptarme a la modalidad de clases virtuales]`,
                            levels = c("Muy en desacuerdo", 
                                       "En desacuerdo", 
                                       "De acuerdo", 
                                       "Muy de acuerdo"))

est2020$b2.internet <- factor(est2020$`B2. ¿Qué tan de acuerdo se encuentra usted respecto de las siguientes afirmaciones? [Mi conexión a internet me permite estudiar sin mayores problemas]`,
                              levels = c("Muy en desacuerdo", 
                                         "En desacuerdo", 
                                         "De acuerdo", 
                                         "Muy de acuerdo"))

est2020$b2.institucion <- factor(est2020$`B2. ¿Qué tan de acuerdo se encuentra usted respecto de las siguientes afirmaciones? [Mi escuela, instituto o universidad ha hecho todo lo posible por adaptarse a las actuales condiciones]`,
                                 levels = c("Muy en desacuerdo", 
                                            "En desacuerdo", 
                                            "De acuerdo", 
                                            "Muy de acuerdo"))

est2020$b2.profes <- factor(est2020$`B2. ¿Qué tan de acuerdo se encuentra usted respecto de las siguientes afirmaciones? [Mis profesores/as han podido adaptarse de buena manera a la modalidad virtual]`,
                            levels = c("Muy en desacuerdo", 
                                       "En desacuerdo", 
                                       "De acuerdo", 
                                       "Muy de acuerdo"))

est2020$b2.compu <- factor(est2020$`B2. ¿Qué tan de acuerdo se encuentra usted respecto de las siguientes afirmaciones? [Tengo un computador en buen estado para poder estudiar]`,
                           levels = c("Muy en desacuerdo", 
                                      "En desacuerdo", 
                                      "De acuerdo", 
                                      "Muy de acuerdo"))


#C. Situación familiar
est2020$c1.barrer <- factor(est2020$`C1. Pensando en la última semana ¿cuántas horas dedicó en total a las siguientes tareas domésticas? [Barrer, aspirar o trapear]`,
                          levels = c("0 horas",
                                     "1-2 horas",
                                     "3-5 horas",
                                     "6-10 horas",
                                     "11-20 horas",
                                     "21 o más horas"))

est2020$c1.basura <- factor(est2020$`C1. Pensando en la última semana ¿cuántas horas dedicó en total a las siguientes tareas domésticas? [Botar la basura]`,
                            levels = c("0 horas",
                                       "1-2 horas",
                                       "3-5 horas",
                                       "6-10 horas",
                                       "11-20 horas",
                                       "21 o más horas"))

est2020$c1.cocinar <- factor(est2020$`C1. Pensando en la última semana ¿cuántas horas dedicó en total a las siguientes tareas domésticas? [Cocinar]`,
                             levels = c("0 horas",
                                        "1-2 horas",
                                        "3-5 horas",
                                        "6-10 horas",
                                        "11-20 horas",
                                        "21 o más horas"))

est2020$c1.adultos <- factor(est2020$`C1. Pensando en la última semana ¿cuántas horas dedicó en total a las siguientes tareas domésticas? [Cuidar adultos mayores]`,
                             levels = c("0 horas",
                                        "1-2 horas",
                                        "3-5 horas",
                                        "6-10 horas",
                                        "11-20 horas",
                                        "21 o más horas"))

est2020$c1.mascotas <- factor(est2020$`C1. Pensando en la última semana ¿cuántas horas dedicó en total a las siguientes tareas domésticas? [Cuidar mascotas]`,
                              levels = c("0 horas",
                                         "1-2 horas",
                                         "3-5 horas",
                                         "6-10 horas",
                                         "11-20 horas",
                                         "21 o más horas"))

est2020$c1.niñxs <- factor(est2020$`C1. Pensando en la última semana ¿cuántas horas dedicó en total a las siguientes tareas domésticas? [Cuidar niños(as)]`,
                           levels = c("0 horas",
                                      "1-2 horas",
                                      "3-5 horas",
                                      "6-10 horas",
                                      "11-20 horas",
                                      "21 o más horas"))

est2020$c1.loza <- factor(est2020$`C1. Pensando en la última semana ¿cuántas horas dedicó en total a las siguientes tareas domésticas? [Lavar loza]`,
                          levels = c("0 horas",
                                     "1-2 horas",
                                     "3-5 horas",
                                     "6-10 horas",
                                     "11-20 horas",
                                     "21 o más horas"))

est2020$c1.ropa <- factor(est2020$`C1. Pensando en la última semana ¿cuántas horas dedicó en total a las siguientes tareas domésticas? [Lavar, colgar o planchar ropa]`,
                          levels = c("0 horas",
                                     "1-2 horas",
                                     "3-5 horas",
                                     "6-10 horas",
                                     "11-20 horas",
                                     "21 o más horas"))

est2020$c1.baños <- factor(est2020$`C1. Pensando en la última semana ¿cuántas horas dedicó en total a las siguientes tareas domésticas? [Limpiar baños]`,
                           levels = c("0 horas",
                                      "1-2 horas",
                                      "3-5 horas",
                                      "6-10 horas",
                                      "11-20 horas",
                                      "21 o más horas"))

est2020$c1.ordenar <- factor(est2020$`C1. Pensando en la última semana ¿cuántas horas dedicó en total a las siguientes tareas domésticas? [Ordenar la casa]`,
                             levels = c("0 horas",
                                        "1-2 horas",
                                        "3-5 horas",
                                        "6-10 horas",
                                        "11-20 horas",
                                        "21 o más horas"))

est2020$c1.compras <- factor(est2020$`C1. Pensando en la última semana ¿cuántas horas dedicó en total a las siguientes tareas domésticas? [Realizar compra de alimentos]`,
                             levels = c("0 horas",
                                        "1-2 horas",
                                        "3-5 horas",
                                        "6-10 horas",
                                        "11-20 horas",
                                        "21 o más horas"))

est2020$c2.internet <- factor(est2020$`C2. ¿Cómo calificarías los siguientes aspectos de tu vivienda actual? [Acceso a internet]`,
                              levels = c("Malo",
                                         "Regular",
                                         "Bueno",
                                         "Excelente"))

est2020$c2.patio <- factor(est2020$`C2. ¿Cómo calificarías los siguientes aspectos de tu vivienda actual? [Acceso a patio, terraza u otro espacio al aire libre]`,
                           levels = c("Malo",
                                      "Regular",
                                      "Bueno",
                                      "Excelente"))

est2020$c2.nhabs <- factor(est2020$`C2. ¿Cómo calificarías los siguientes aspectos de tu vivienda actual? [Cantidad de habitaciones]`,
                               levels = c("Malo",
                                          "Regular",
                                          "Bueno",
                                          "Excelente"))

est2020$c2.equipos <- factor(est2020$`C2. ¿Cómo calificarías los siguientes aspectos de tu vivienda actual? [Equipamiento (muebles, artículos de hogar, etc.)]`,
                            levels = c("Malo",
                                       "Regular",
                                       "Bueno",
                                       "Excelente"))

est2020$c2.thabs <- factor(est2020$`C2. ¿Cómo calificarías los siguientes aspectos de tu vivienda actual? [Tamaño de las habitaciones]`,
                           levels = c("Malo",
                                      "Regular",
                                      "Bueno",
                                      "Excelente"))

est2020$c2.ubicacion <- factor(est2020$`C2. ¿Cómo calificarías los siguientes aspectos de tu vivienda actual? [Ubicación y entorno]`,
                               levels = c("Malo",
                                          "Regular",
                                          "Bueno",
                                          "Excelente"))

est2020$c3.npersonas <- factor(est2020$`C3. Además de usted ¿con cuántas personas vive habitualmente?`,
                               levels = c("Vivo solo/a",
                                          "1",
                                          "2",
                                          "3",
                                          "4",
                                          "5 o más"))

est2020$c4.habitantes <- est2020$`C4. ¿Cuál es la relación que tiene usted con los demás habitantes de su vivienda? Marque todas las que corresponda`

est2020$c4.abuelxs <- ifelse(grepl("Abuelo | abuela", est2020$c4.habitantes),1,0)
est2020$c4.amigxs <- ifelse(grepl("Amigo | amiga", est2020$c4.habitantes),1,0)
est2020$c4.arrendatarixs <- ifelse(grepl("Arrendata", est2020$c4.habitantes),1,0)
est2020$c4.pareja <- ifelse(grepl("Cónyuge, pareja o conviviente", est2020$c4.habitantes),1,0)
est2020$c4.hermanxs <- ifelse(grepl("Hermano | hermana", est2020$c4.habitantes),1,0)
est2020$c4.padres <- ifelse(grepl("Padre | madre", est2020$c4.habitantes),1,0)
est2020$c4.hijxs <- ifelse(grepl("Hijo | hija", est2020$c4.habitantes),1,0)

#para poder crear una variable que agrupe todas las otras respuestas, es posible encadenar múltiples "ifelse(CONDICION,VERDADERO,FALSO)"
est2020$c4.otrxs <- ifelse(grepl("Tio", est2020$c4.habitantes),1,
                           ifelse(grepl("Allegados",est2020$c4.habitantes),1,
                           ifelse(grepl("Cuñada,suegros,sobrino",est2020$c4.habitantes),1,
                           ifelse(grepl("primo y sobrino y sobrina nieta",est2020$c4.habitantes),1,
                           ifelse(grepl("Sobrino",est2020$c4.habitantes),1, 
                           ifelse(grepl("hijastro",est2020$c4.habitantes),1, 
                           ifelse(grepl("Nieta",est2020$c4.habitantes),1, 
                           ifelse(grepl("Primos",est2020$c4.habitantes),1, 
                           ifelse(grepl("suegro, suegra",est2020$c4.habitantes),1, 
                           ifelse(grepl("Allegado",est2020$c4.habitantes),1, 
                           ifelse(grepl("Sobrinos",est2020$c4.habitantes),1, 
                           ifelse(grepl("Tíos",est2020$c4.habitantes),1, 
                           ifelse(grepl("Sobrino",est2020$c4.habitantes),1, 
                           ifelse(grepl("Cuñado",est2020$c4.habitantes),1, 
                           ifelse(grepl("Sobrina",est2020$c4.habitantes),1,
                           ifelse(grepl("Tía",est2020$c4.habitantes),1,
                           ifelse(grepl("Sobrina nieta",est2020$c4.habitantes),1,
                           ifelse(grepl("Tío, padre, hermano",est2020$c4.habitantes),1,0))))))))))))))))))

est2020$c5.aumento <- factor(est2020$`C5. ¿Qué tan de acuerdo se encuentra usted respecto de las siguientes afirmaciones? [Durante la cuarentena han aumentado significativamente las tareas domésticas]`,
                             levels = c("Muy en desacuerdo", 
                                        "En desacuerdo", 
                                        "De acuerdo", 
                                        "Muy de acuerdo"))

est2020$c5.mujeres <- factor(est2020$`C5. ¿Qué tan de acuerdo se encuentra usted respecto de las siguientes afirmaciones? [El trabajo doméstico es tarea principalmente de mujeres]`,
                             levels = c("Muy en desacuerdo", 
                                        "En desacuerdo", 
                                        "De acuerdo", 
                                        "Muy de acuerdo"))

est2020$c5.gusta <- factor(est2020$`C5. ¿Qué tan de acuerdo se encuentra usted respecto de las siguientes afirmaciones? [Hago las tareas domésticas porque me gusta hacerlas]`,
                           levels = c("Muy en desacuerdo", 
                                      "En desacuerdo", 
                                      "De acuerdo", 
                                      "Muy de acuerdo"))

est2020$c5.apoyo <- factor(est2020$`C5. ¿Qué tan de acuerdo se encuentra usted respecto de las siguientes afirmaciones? [Siempre tengo apoyo de los demás integrantes del hogar para hacer las tareas domésticas]`,
                           levels = c("Muy en desacuerdo", 
                                      "En desacuerdo", 
                                      "De acuerdo", 
                                      "Muy de acuerdo"))

est2020$c5.tiempo <- factor(est2020$`C5. ¿Qué tan de acuerdo se encuentra usted respecto de las siguientes afirmaciones? [Siento que las tareas domésticas me demandan mucho tiempo]`,
                            levels = c("Muy en desacuerdo", 
                                       "En desacuerdo", 
                                       "De acuerdo", 
                                       "Muy de acuerdo"))

est2020$c5.unicx <- factor(est2020$`C5. ¿Qué tan de acuerdo se encuentra usted respecto de las siguientes afirmaciones? [Soy el/la único/a que se encarga de las tareas domésticas]`,
                            levels = c("Muy en desacuerdo", 
                                       "En desacuerdo", 
                                       "De acuerdo", 
                                       "Muy de acuerdo"))

est2020$c6.nhijxs <- factor(est2020$`C6. ¿Cuántos(as) hijos(as) tiene usted?`,
                            levels = c("0",
                                       "1",
                                       "2",
                                       "3",
                                       "4",
                                       "5 o más"))

est2020$c7.recreacion <- factor(est2020$`C7. Pensando en las distintas dimensiones de la vida familiar ¿Usted diría que...? [El espacio de recreación en familia]`,
                                levels = c("Disminuyó",
                                           "Se mantuvo igual",
                                           "Aumentó"))

est2020$c7.union <- factor(est2020$`C7. Pensando en las distintas dimensiones de la vida familiar ¿Usted diría que...? [El sentido de unión familiar]`,
                           levels = c("Disminuyó",
                                      "Se mantuvo igual",
                                      "Aumentó"))

est2020$c7.tiempo <- factor(est2020$`C7. Pensando en las distintas dimensiones de la vida familiar ¿Usted diría que...? [El tiempo disponible para la familia]`,
                            levels = c("Disminuyó",
                                       "Se mantuvo igual",
                                       "Aumentó"))

est2020$c7.comunicacion <- factor(est2020$`C7. Pensando en las distintas dimensiones de la vida familiar ¿Usted diría que...? [La calidad de la comunicación familiar]`,
                                  levels = c("Disminuyó",
                                             "Se mantuvo igual",
                                             "Aumentó"))

est2020$c7.confianza <- factor(est2020$`C7. Pensando en las distintas dimensiones de la vida familiar ¿Usted diría que...? [La confianza dentro del núcleo familiar]`,
                               levels = c("Disminuyó",
                                          "Se mantuvo igual",
                                          "Aumentó"))

est2020$c7.preocupacion <- factor(est2020$`C7. Pensando en las distintas dimensiones de la vida familiar ¿Usted diría que...? [La preocupación por no lograr cubrir las necesidades básicas de su grupo familiar ]`,
                                  levels = c("Disminuyó",
                                             "Se mantuvo igual",
                                             "Aumentó"))

est2020$c7.conflictos <- factor(est2020$`C7. Pensando en las distintas dimensiones de la vida familiar ¿Usted diría que...? [Las peleas, discusiones o conflictos familiares]`,
                                levels = c("Disminuyó",
                                           "Se mantuvo igual",
                                           "Aumentó"))

est2020$d1.angustia <- factor(est2020$`D1. ¿Con qué frecuencia ha sentido usted las siguientes emociones durante la actual pandemia del Covid-19?  [Angustia]`,
                              levels = c("Nunca",
                                         "Casi nunca",
                                         "A veces",
                                         "Frecuentemente",
                                         "Siempre"))

est2020$d1.ansiedad <- factor(est2020$`D1. ¿Con qué frecuencia ha sentido usted las siguientes emociones durante la actual pandemia del Covid-19?  [Ansiedad]`,
                              levels = c("Nunca",
                                         "Casi nunca",
                                         "A veces",
                                         "Frecuentemente",
                                         "Siempre"))

est2020$d1.estres <- factor(est2020$`D1. ¿Con qué frecuencia ha sentido usted las siguientes emociones durante la actual pandemia del Covid-19?  [Estrés]`,
                            levels = c("Nunca",
                                       "Casi nunca",
                                       "A veces",
                                       "Frecuentemente",
                                       "Siempre"))

est2020$d1.frustracion <- factor(est2020$`D1. ¿Con qué frecuencia ha sentido usted las siguientes emociones durante la actual pandemia del Covid-19?  [Frustración]`,
                                 levels = c("Nunca",
                                            "Casi nunca",
                                            "A veces",
                                            "Frecuentemente",
                                            "Siempre"))

est2020$d1.incertidumbre <- factor(est2020$`D1. ¿Con qué frecuencia ha sentido usted las siguientes emociones durante la actual pandemia del Covid-19?  [Incertidumbre]`,
                                   levels = c("Nunca",
                                              "Casi nunca",
                                              "A veces",
                                              "Frecuentemente",
                                              "Siempre"))

est2020$d1.enojo <- factor(est2020$`D1. ¿Con qué frecuencia ha sentido usted las siguientes emociones durante la actual pandemia del Covid-19?  [Ira o enojo]`,
                           levels = c("Nunca",
                                      "Casi nunca",
                                      "A veces",
                                      "Frecuentemente",
                                      "Siempre"))

est2020$d1.miedo <- factor(est2020$`D1. ¿Con qué frecuencia ha sentido usted las siguientes emociones durante la actual pandemia del Covid-19?  [Miedo]`,
                           levels = c("Nunca",
                                      "Casi nunca",
                                      "A veces",
                                      "Frecuentemente",
                                      "Siempre"))

est2020$d1.soledad <- factor(est2020$`D1. ¿Con qué frecuencia ha sentido usted las siguientes emociones durante la actual pandemia del Covid-19?  [Soledad]`,
                             levels = c("Nunca",
                                        "Casi nunca",
                                        "A veces",
                                        "Frecuentemente",
                                        "Siempre"))

est2020$d1.tristeza <- factor(est2020$`D1. ¿Con qué frecuencia ha sentido usted las siguientes emociones durante la actual pandemia del Covid-19?  [Tristeza]`,
                              levels = c("Nunca",
                                         "Casi nunca",
                                         "A veces",
                                         "Frecuentemente",
                                         "Siempre"))

est2020$d2.atencion <- factor(est2020$`D2. ¿Cree usted que necesita atención de algún especialista en salud mental por alguna(s) de las situaciones anteriores?`,
                              levels = c("No",
                                         "Tal vez",
                                         "Sí"))

est2020$d3.farmacos <- factor(est2020$`D3. Durante la semana pasada ¿consumió usted algún fármaco de salud mental como ansiolíticos, antidepresivos u otros?`,
                              levels = c("No",
                                         "Sí"))

est2020$d4.tratamiento <- factor(est2020$`D4. Si se encontraba en tratamiento por alguna enfermedad o condición de salud mental antes de la pandemia ¿ha podido seguir su tratamiento con normalidad?`,
                                 levels = c("No estaba en ningún tratamiento antes de la pandemia",
                                            "Si, con total normalidad",
                                            "Si, pero con algunas dificultades",
                                            "No he podido continuar el tratamiento"))

est2020$e1.pareja <- factor(est2020$`E1. Pensando en los últimos 3 meses ¿usted está o ha estado en alguna de las siguientes relaciones de pareja?`,
                            levels = c("No he estado en una relación de pareja durante los últimos tres meses",
                                       "Pololeo o noviazgo",
                                       "Convivente Civil",
                                       "Matrimonio",
                                       "Otra relación afectiva"))

est2020$e2.cambio <- factor(est2020$`E2. En términos generales ¿usted diría que su relación de pareja ha empeorado, ha mejorado o se ha mantenido igual?`,
                            levels = c("Ha mejorado mucho",
                                       "Ha mejorado levemente",
                                       "Se ha mantenido igual",
                                       "Ha empeorado levemente",
                                       "Ha empeorado mucho"))

est2020$e3.enojos <- factor(est2020$`E3. Pensando en su actual relación de pareja ¿Con qué frecuencia diría usted que ha experimentado alguna(s) de las siguientes situaciones durante la pandemia? [Cuando se enoja, golpea paredes, puertas o rompe objetos]`,
                       levels = c("Nunca",
                                  "Casi nunca",
                                  "A veces",
                                  "Frecuentemente",
                                  "Casi siempre",
                                  "Siempre"))

est2020$e3.lesionarme <- factor(est2020$`E3. Pensando en su actual relación de pareja ¿Con qué frecuencia diría usted que ha experimentado alguna(s) de las siguientes situaciones durante la pandemia? [Ha amenazado con herirme o lesionarme]`,
                       levels = c("Nunca",
                                  "Casi nunca",
                                  "A veces",
                                  "Frecuentemente",
                                  "Casi siempre",
                                  "Siempre"))

est2020$e3.golpes <- factor(est2020$`E3. Pensando en su actual relación de pareja ¿Con qué frecuencia diría usted que ha experimentado alguna(s) de las siguientes situaciones durante la pandemia? [Me golpea o me empuja]`,
                       levels = c("Nunca",
                                  "Casi nunca",
                                  "A veces",
                                  "Frecuentemente",
                                  "Casi siempre",
                                  "Siempre"))

est2020$e3.gritos <- factor(est2020$`E3. Pensando en su actual relación de pareja ¿Con qué frecuencia diría usted que ha experimentado alguna(s) de las siguientes situaciones durante la pandemia? [Me grita]`,
                       levels = c("Nunca",
                                  "Casi nunca",
                                  "A veces",
                                  "Frecuentemente",
                                  "Casi siempre",
                                  "Siempre"))

est2020$e3.terceros <- factor(est2020$`E3. Pensando en su actual relación de pareja ¿Con qué frecuencia diría usted que ha experimentado alguna(s) de las siguientes situaciones durante la pandemia? [Me han amenazado con herir o lesionar a alguien que me importa]`,
                       levels = c("Nunca",
                                  "Casi nunca",
                                  "A veces",
                                  "Frecuentemente",
                                  "Casi siempre",
                                  "Siempre"))

est2020$e3.humilla <- factor(est2020$`E3. Pensando en su actual relación de pareja ¿Con qué frecuencia diría usted que ha experimentado alguna(s) de las siguientes situaciones durante la pandemia? [Me humilla frente a otras personas]`,
                       levels = c("Nunca",
                                  "Casi nunca",
                                  "A veces",
                                  "Frecuentemente",
                                  "Casi siempre",
                                  "Siempre"))

est2020$e3.insulta <- factor(est2020$`E3. Pensando en su actual relación de pareja ¿Con qué frecuencia diría usted que ha experimentado alguna(s) de las siguientes situaciones durante la pandemia? [Me insulta]`,
                       levels = c("Nunca",
                                  "Casi nunca",
                                  "A veces",
                                  "Frecuentemente",
                                  "Casi siempre",
                                  "Siempre"))

est2020$e3.respeto <- factor(est2020$`E3. Pensando en su actual relación de pareja ¿Con qué frecuencia diría usted que ha experimentado alguna(s) de las siguientes situaciones durante la pandemia? [Me trata con respeto]`,
                       levels = c("Nunca",
                                  "Casi nunca",
                                  "A veces",
                                  "Frecuentemente",
                                  "Casi siempre",
                                  "Siempre"))

est2020$e3.peligro <- factor(est2020$`E3. Pensando en su actual relación de pareja ¿Con qué frecuencia diría usted que ha experimentado alguna(s) de las siguientes situaciones durante la pandemia? [Siento que mi vida está en peligro]`,
                       levels = c("Nunca",
                                  "Casi nunca",
                                  "A veces",
                                  "Frecuentemente",
                                  "Casi siempre",
                                  "Siempre"))

est2020$e4.conversa <- factor(est2020$`E4. ¿Ha conversado respecto a estas situaciones con familiares, amistades o personas de confianza?`,
                              levels = c("No",
                                         "Sí"))

est2020$e5.denuncia1 <- factor(est2020$`E5. ¿Ha denunciado usted alguna de estas situaciones?`,
                              levels = c("No",
                                         "Sí"))

est2020$e6.institucion <- est2020$`E6. ¿En qué institución ha denunciado estas situaciones? Marque todas las que corresponda`
est2020$e6.carchi <- ifelse(grepl("Carabineros de Chile",est2020$e6.institucion),1,0)
#Sólo hay una denuncia en la base de datos.

est2020$e7.violencia <- factor(est2020$`E7. Durante los últimos seis meses ¿ha presenciado o se ha enterado de alguna mujer que sea víctima de violencia física, psicológica o sexual en su relación de pareja?`,
                               levels = c("No",
                                          "Sí"))

est2020$e8.denuncia2 <- factor(est2020$`E8. ¿Denunció usted esta situación?`, 
                              levels = c("No",
                                         "Sí"))

#F. Opinión pública

est2020$f1.moviliza <- factor(est2020$`F1. Pensando en la situación actual del país y la región ¿qué tan de acuerdo se encuentra respecto de las siguientes afirmaciones? [Cuando termine la pandemia, las personas volverán a manifestarse]`,
                              levels = c("Absolutamente en desacuerdo",
                                         "Muy en desacuerdo",
                                         "En desacuerdo",
                                         "De acuerdo",
                                         "Muy de acuerdo",
                                         "Absolutamente de acuerdo"))

est2020$f1.gobierno <- factor(est2020$`F1. Pensando en la situación actual del país y la región ¿qué tan de acuerdo se encuentra respecto de las siguientes afirmaciones? [El gobierno ha sabido gestionar adecuadamente la crisis sanitaria]`,
                      levels = c("Absolutamente en desacuerdo",
                                 "Muy en desacuerdo",
                                 "En desacuerdo",
                                 "De acuerdo",
                                 "Muy de acuerdo",
                                 "Absolutamente de acuerdo"))

est2020$f1.justicia <- factor(est2020$`F1. Pensando en la situación actual del país y la región ¿qué tan de acuerdo se encuentra respecto de las siguientes afirmaciones? [El sistema de justicia en Chile es igual para todos/as]`,
                              levels = c("Absolutamente en desacuerdo",
                                         "Muy en desacuerdo",
                                         "En desacuerdo",
                                         "De acuerdo",
                                         "Muy de acuerdo",
                                         "Absolutamente de acuerdo"))

est2020$f1.ddhh <- factor(est2020$`F1. Pensando en la situación actual del país y la región ¿qué tan de acuerdo se encuentra respecto de las siguientes afirmaciones? [En Chile se respetan los derechos humanos]`,
                          levels = c("Absolutamente en desacuerdo",
                                     "Muy en desacuerdo",
                                     "En desacuerdo",
                                     "De acuerdo",
                                     "Muy de acuerdo",
                                     "Absolutamente de acuerdo"))

est2020$f1.migra <- factor(est2020$`F1. Pensando en la situación actual del país y la región ¿qué tan de acuerdo se encuentra respecto de las siguientes afirmaciones? [La migración es un aporte para el país y la región]`,
                           levels = c("Absolutamente en desacuerdo",
                                      "Muy en desacuerdo",
                                      "En desacuerdo",
                                      "De acuerdo",
                                      "Muy de acuerdo",
                                      "Absolutamente de acuerdo"))

est2020$f1.policias <- factor(est2020$`F1. Pensando en la situación actual del país y la región ¿qué tan de acuerdo se encuentra respecto de las siguientes afirmaciones? [Las policías en Chile me entregan una sensación de confianza y seguridad]`,
                              levels = c("Absolutamente en desacuerdo",
                                         "Muy en desacuerdo",
                                         "En desacuerdo",
                                         "De acuerdo",
                                         "Muy de acuerdo",
                                         "Absolutamente de acuerdo"))

est2020$f1.apoyos <- factor(est2020$`F1. Pensando en la situación actual del país y la región ¿qué tan de acuerdo se encuentra respecto de las siguientes afirmaciones? [Los apoyos que ha entregado el Estado durante la pandemia han sido suficientes]`,
                            levels = c("Absolutamente en desacuerdo",
                                       "Muy en desacuerdo",
                                       "En desacuerdo",
                                       "De acuerdo",
                                       "Muy de acuerdo",
                                       "Absolutamente de acuerdo"))

est2020$f1.expulsion <- factor(est2020$`F1. Pensando en la situación actual del país y la región ¿qué tan de acuerdo se encuentra respecto de las siguientes afirmaciones? [Los extranjeros que cometen delitos deben ser expulsados del país]`,
                               levels = c("Absolutamente en desacuerdo",
                                          "Muy en desacuerdo",
                                          "En desacuerdo",
                                          "De acuerdo",
                                          "Muy de acuerdo",
                                          "Absolutamente de acuerdo"))

est2020$f1.pensiones <- factor(est2020$`F1. Pensando en la situación actual del país y la región ¿qué tan de acuerdo se encuentra respecto de las siguientes afirmaciones? [Se debe cambiar el actual sistema de pensiones por un sistema de reparto]`,
                               levels = c("Absolutamente en desacuerdo",
                                          "Muy en desacuerdo",
                                          "En desacuerdo",
                                          "De acuerdo",
                                          "Muy de acuerdo",
                                          "Absolutamente de acuerdo"))

est2020$f1.apruebo <- factor(est2020$`F1. Pensando en la situación actual del país y la región ¿qué tan de acuerdo se encuentra respecto de las siguientes afirmaciones? [Si el plebiscito por una Nueva Constitución fuera el próximo domingo, estoy absolutamente seguro/a que votaría por la opción "Apruebo"]`,
                             levels = c("Absolutamente en desacuerdo",
                                        "Muy en desacuerdo",
                                        "En desacuerdo",
                                        "De acuerdo",
                                        "Muy de acuerdo",
                                        "Absolutamente de acuerdo"))


#G. Caracaterísticas generales

est2020$sexo <- est2020$Sexo
est2020$sexo[est2020$sexo == "Objetore de género"] <- "Otro"
est2020$sexo[est2020$sexo == "Prefiero no decirlo"] <- "Otro"
est2020$sexo <- factor(est2020$sexo,
                       levels = c("Mujer",
                                  "Hombre",
                                  "Otro"))

est2020$edad <- as.numeric(est2020$Edad)

est2020$comunares <- est2020$`Cuál ha sido su comuna habitual de residencia durante los últimos seis meses?`
est2020$comunares[est2020$comunares == "Arica"] <- "Otro"
est2020$comunares[est2020$comunares == "La Cruz"] <- "Otro"
est2020$comunares[est2020$comunares == "La Tirana (pueblo)"] <- "Otro"
est2020$comunares[est2020$comunares == "Tocopilla"] <- "Otro"
est2020$comunares <- factor(est2020$comunares,
                         levels = c("Alto Hospicio",
                                    "Huara",
                                    "Iquique",
                                    "Pozo Almonte",
                                    "Otro"))

est2020$paisnac <- factor(est2020$`¿En qué país nació?`,
                       levels = c("Bolivia",
                                  "Chile",
                                  "Ecuador",
                                  "Perú",
                                  "Venezuela"))

est2020$regionnac <- est2020$`¿En qué ciudad nació? (favor evite usar abreviaciones)`
est2020$regionnac[est2020$regionnac == "A Hospicio"] <- "Tarapaca"
est2020$regionnac[est2020$regionnac == "antofagasta"] <- "Antofagasta"
est2020$regionnac[est2020$regionnac == "Antofagasta"] <- "Antofagasta"
est2020$regionnac[est2020$regionnac == "Arica"] <- "Arica"
est2020$regionnac[est2020$regionnac == "Buin"] <- "Metropolitana"
est2020$regionnac[est2020$regionnac == "Calama"] <- "Antofagasta"
est2020$regionnac[est2020$regionnac == "Chañaral"] <- "Atacama"
est2020$regionnac[est2020$regionnac == "Chile"] <- "Otra"
est2020$regionnac[est2020$regionnac == "Chillan"] <- "Ñuble"
est2020$regionnac[est2020$regionnac == "Chuquicamata"] <- "Antofagasta"
est2020$regionnac[est2020$regionnac == "concepción"] <- "Bio Bio"
est2020$regionnac[est2020$regionnac == "Concepción"] <- "Bio Bio"
est2020$regionnac[est2020$regionnac == "Copiapo"] <- "Atacama"
est2020$regionnac[est2020$regionnac == "Copiapó"] <- "Atacama"
est2020$regionnac[est2020$regionnac == "Coquimbo"] <- "Coquimbo"
est2020$regionnac[est2020$regionnac == "Curico"] <- "Maule"
est2020$regionnac[est2020$regionnac == "Guayaquil"] <- "Otra"
est2020$regionnac[est2020$regionnac == "Huanuco"] <- "Otra"
est2020$regionnac[est2020$regionnac == "Illapel"] <- "Valparaiso"
est2020$regionnac[est2020$regionnac == "Ipiales"] <- "Otra"
est2020$regionnac[est2020$regionnac == "Iquiqud"] <- "Tarapaca"
est2020$regionnac[est2020$regionnac == "iquique"] <- "Tarapaca"
est2020$regionnac[est2020$regionnac == "Iquique"] <- "Tarapaca"
est2020$regionnac[est2020$regionnac == "IQUIQUE"] <- "Tarapaca"
est2020$regionnac[est2020$regionnac == "Linares"] <- "Maule"
est2020$regionnac[est2020$regionnac == "Llanquihue"] <- "Los Lagos"
est2020$regionnac[est2020$regionnac == "Los angeles"] <- "Bio Bio"
est2020$regionnac[est2020$regionnac == "Montero - Santa cruz"] <- "Otra"
est2020$regionnac[est2020$regionnac == "Pedro de Valdivia"] <- "Bio Bio"
est2020$regionnac[est2020$regionnac == "Pozo Almonte"] <- "Tarapaca"
est2020$regionnac[est2020$regionnac == "Puerto Ordaz"] <- "Otra"
est2020$regionnac[est2020$regionnac == "Punta Arenas"] <- "Magallanes"
est2020$regionnac[est2020$regionnac == "Purén"] <- "Araucania"
est2020$regionnac[est2020$regionnac == "Quillota"] <- "Valparaiso"
est2020$regionnac[est2020$regionnac == "Rancagua"] <- "Ohiggins"
est2020$regionnac[est2020$regionnac == "Región de Los Ríos,  Río Bueno"] <- "Los Rios"
est2020$regionnac[est2020$regionnac == "Salamanca"] <- "Coquimbo"
est2020$regionnac[est2020$regionnac == "San felipe"] <- "Valparaiso"
est2020$regionnac[est2020$regionnac == "Sanpedro  de Atacama (pueblo)"] <- "Antofagasta"
est2020$regionnac[est2020$regionnac == "Santa Cruz"] <- "Otra"
est2020$regionnac[est2020$regionnac == "Santa Cruz de la Sierra"] <- "Otra"
est2020$regionnac[est2020$regionnac == "santiago"] <- "Metropolitana"
est2020$regionnac[est2020$regionnac == "Santiago"] <- "Metropolitana"
est2020$regionnac[est2020$regionnac == "SANTIAGO"] <- "Metropolitana"
est2020$regionnac[est2020$regionnac == "santiago RM"] <- "Metropolitana"
est2020$regionnac[est2020$regionnac == "Stgo."] <- "Metropolitana"
est2020$regionnac[est2020$regionnac == "Talca"] <- "Maule"
est2020$regionnac[est2020$regionnac == "Talcahuano"] <- "Bio Bio"
est2020$regionnac[est2020$regionnac == "Tarapaca"] <- "Tarapaca"
est2020$regionnac[est2020$regionnac == "Tocopilla"] <- "Antofagasta"
est2020$regionnac[est2020$regionnac == "Tomé"] <- "Bio Bio"
est2020$regionnac[est2020$regionnac == "Valparaíso"] <- "Valparaiso"
est2020$regionnac[est2020$regionnac == "Valparaiso"] <- "Valparaiso"
est2020$regionnac[est2020$regionnac == "Valpo"] <- "Valparaiso"
est2020$regionnac[est2020$regionnac == "Victoria"] <- "Araucania"
est2020$regionnac[est2020$regionnac == "Victoria novena región"] <- "Araucania"
est2020$regionnac[est2020$regionnac == "Viña del mar"] <- "Valparaiso"
est2020$regionnac[est2020$regionnac == "Viña del Mar"] <- "Valparaiso"

est2020$regionnac <- factor(est2020$regionnac,
                            levels = c("Arica",
                                       "Tarapaca",
                                       "Antofagasta",
                                       "Atacama",
                                       "Coquimbo",
                                       "Valparaiso",
                                       "Metropolitana",
                                       "Ohiggins",
                                       "Maule",
                                       "Bio Bio",
                                       "Ñuble",
                                       "Araucania",
                                       "Los Rios",
                                       "Los Lagos",
                                       "Aysen",
                                       "Magallanes",
                                       "Otra"))

est2020$estudios <- est2020$`¿Cuál de las siguientes categorías describe de mejor forma su nivel de estudios?`
est2020$estudios[est2020$estudios=="Magister"] <- "Postgrado"
est2020$estudios[est2020$estudios=="Magíster o Doctorado"] <- "Postgrado"
est2020$estudios[est2020$estudios=="Técnica Completa + Universidad Incompleta"] <- "Técnica completa"#
est2020$estudios[est2020$estudios=="Cursando educación universitaria"] <- "Universitaria incompleta"
est2020$estudios[est2020$estudios=="Cursando la Universidad"] <- "Universitaria incompleta"
est2020$estudios[est2020$estudios=="Estudiando"] <- NA
est2020$estudios <- factor(est2020$estudios,
                           levels = c("Básica incompleta",
                                      "Básica completa",
                                      "Media incompleta",
                                      "Media completa",
                                      "Técnica incompleta",
                                      "Técnica completa",
                                      "Universitaria incompleta",
                                      "Universitaria completa",
                                      "Postgrado"))

est2020$ingresos <- factor(est2020$`Considerando sólo tus ingresos, indica el rango que más se acerque al monto del mes pasado:`,
                           levels = c("No tuve ingresos",
                                      "Menos de $150.000",
                                      "$150.000 - $300.000",
                                      "$300.000 - $450.000",
                                      "$450.000 - $750.000",
                                      "$750.000 - $1.500.000",
                                      "Más de $1.500.000"))

est2020$cambioingreso <- factor(est2020$`Comparando tus ingresos actuales respecto a la situación antes de la pandemia ¿usted diría que estos?`,
                                levels = c("Han aumentado fuertemente",
                                           "Han aumentado levemente",
                                           "Se han mantenido igual",
                                           "Han disminuido levemente",
                                           "Han disminuido fuertemente"))

#Recodificar otras categorias (table(est2020$`Por favor indique si durante los últimos meses ha recibido alguno(s) de los siguientes beneficio estatales. Marque todas las que corresponda`))
est2020$bonos <- est2020$`Por favor indique si durante los últimos meses ha recibido alguno(s) de los siguientes beneficio estatales. Marque todas las que corresponda`
est2020$bonos[est2020$bonos == "1 solo bono"] <- "Otro"
est2020$bonos[est2020$bonos == "10% afp"] <- NA
est2020$bonos[est2020$bonos == "Abonotetoca"] <- NA #jajaja
est2020$bonos[est2020$bonos == "Afp"] <- NA
est2020$bonos[est2020$bonos == "bono 500.000"] <- "Clase media ($500.000)"
est2020$bonos[est2020$bonos == "Bono 500.000"] <- "Clase media ($500.000)"
est2020$bonos[est2020$bonos == "bono clase media"] <- "Clase media ($500.000)"
est2020$bonos[est2020$bonos == "Bono clase media"] <- "Clase media ($500.000)"
est2020$bonos[est2020$bonos == "Bono Clase media"] <- "Clase media ($500.000)"
est2020$bonos[est2020$bonos == "Credito del servicio impuesto interno"] <- NA
est2020$bonos[est2020$bonos == "Ingreso familiar de emergencia;Bono Covid-19;Caja familiar (mercadería);Bono clase media"] <- "Ingreso familiar de emergencia;Bono Covid-19;Caja familiar (mercadería);Clase media ($500.000)"
est2020$bonos[est2020$bonos == "nada"] <- NA
est2020$bonos[est2020$bonos == "Nada"] <- NA
est2020$bonos[est2020$bonos == "Ningún beneficio"] <- NA
est2020$bonos[est2020$bonos == "Ninguno"] <- NA
est2020$bonos[est2020$bonos == "NINGUNO"] <- NA
est2020$bonos[est2020$bonos == "Ningunos"] <- NA
est2020$bonos[est2020$bonos == "no"] <- NA
est2020$bonos[est2020$bonos == "No"] <- NA
est2020$bonos[est2020$bonos == "No he recibido nada"] <- NA
est2020$bonos[est2020$bonos == "No he recibido ninguno"] <- NA
est2020$bonos[est2020$bonos == "No recibí ningún bono ni caja"] <- NA
est2020$bonos[est2020$bonos == "Nunca he recibido algo del gobierno"] <- NA
est2020$bonos[est2020$bonos == "Prestamo SII, Devolucion impr y bono 500 mil"] <- "Clase media ($500.000)"
est2020$bonos[est2020$bonos == "Respecto a la pregunta anterior. Antes era empaque, ahora trabajo de TENS, me aprovecho de la pandemia y gano dinero. Por eso salgo todos los dias de casa.."] <- NA

est2020$bonos.caja <- ifelse(grepl("Caja familiar", est2020$bonos),1,0)
est2020$bonos.ife <- ifelse(grepl("Ingreso familiar de emergencia", est2020$bonos),1,0)
est2020$bonos.minimo <- ifelse(grepl("Ingreso mínimo garantizado", est2020$bonos),1,0)
est2020$bonos.covid <- ifelse(grepl("Bono Covid-19", est2020$bonos),1,0)
est2020$bonos.media <- ifelse(grepl("Clase media", est2020$bonos),1,0)
est2020$bonos.none <- with(est2020,ifelse(bonos.caja==0 & bonos.ife==0 & bonos.minimo==0 & bonos.covid==0 & bonos.media==0,1,0))

est2020$salir <- as.numeric(est2020$`En promedio ¿cuántos días a la semana ha salido de su casa durante la cuarentena?`)

est2020 <- est2020[c(104:218)]

write.csv2(est2020,"/Users/danielquinterosr/Google Drive/UNAP/Cursos/Estadistica/2020 TS Metodologia CUANTI/Encuesta TS-Covid/ESTCovid19/est2020.csv", row.names = TRUE, fileEncoding = "UTF-8")






