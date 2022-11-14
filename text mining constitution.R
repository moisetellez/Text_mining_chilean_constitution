########################################################

###### Chilean constitution text mining #######

########################################################

#Creator: Moises Tellez

#Date: 2022-08-20

#Libraries
{
library(data.table)
library(pdftools)
library(dplyr)
library(tidyr)
library(stopwords)
library(tidytext)
library(stringr)
library(ggplot2)
library(wordcloud) #To generate wordcloud
library(RColorBrewer) #For wordcloud colors
library(ggraph)
library(igraph)
}


setwd("~/R/Proyecto text mining")



#####################################################################################
# 1st analysis: Current constitution
#####################################################################################


#abrir texto a analizar (separara el texto por paginas)
texto <-pdf_text("constitucion_politica.pdf")


#Juntamos todas las paginas en un solo gran texto
texto<-paste(texto, collapse = '')
length(texto)


#Separando el texto en frases (separacion por punto)
vector = c()
for(i in 1:length(texto)){
  temp<-(strsplit(texto[[i]], "\\.")[[1]])
  print(temp)
  vector <- c(vector, temp)
}


#Pasando el texto a formato table (tibble)
texto<-tibble(vector)



################ PRIMER ANALISIS: TOKEN DE PALABRAS ################

#Pasando el texto a tokens (palabras)
texto2 <- texto %>% 
  unnest_tokens(word,vector,drop = FALSE) #el primer "word" indica que la separacion sera a nivel palabra

texto2$vector<- as.character(texto2$vector)


#Nos creamos un lexicon de stopwords en espa?ol 
lexiconSW<-stopwords("es")
lexiconSW <- append(lexiconSW,c("1","2","3","4","5","n","19","art?culo","45","7","letra","12","6","24","113")) #con append se pueden agregar stopwords
lexiconSW<- tibble(lexiconSW)
lexiconSW$lexiconSW <-as.character(lexiconSW$lexiconSW)
lexiconSW<- lexiconSW %>% filter(lexiconSW!="estado") #Eliminar stopwords

#sacando las stopwords al texto
texto3<- anti_join(texto2,lexiconSW, by= c("word"="lexiconSW"))


#Generando tabla de fecuencias de palabras
frec_texto<-texto3 %>% 
  group_by(word) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

frec_texto %>% 
  top_n(20,n) %>%   ##Se selecciona el top, usando la variable n
  mutate(word = reorder(word, n)) %>% #Ordena de mayor a menor las palabras
  ggplot(aes(x= n,y=word)) +
  geom_col()+
  labs(y = NULL)+
  ggtitle("Most used words - Current constitution")


#Generamos el wordcloud
wordcloud(words = frec_texto$word, freq = frec_texto$n,
          max.words = 400, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))



############ Bigramas #################
#Es la relacion de dos palabras


review_bigrams <- texto %>%  ##Usamos la tabla sin eliminar por stop words
  unnest_tokens(bigram, vector, token = "ngrams", n = 2) %>%  # ahora, separamos cada 2 token
  separate(bigram, c("word1", "word2"), sep = " ") %>%  # separamos word por bigrama
  filter(!word1 %in% lexiconSW$lexiconSW) %>% 
  filter(!word2 %in% lexiconSW$lexiconSW) %>% 
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>%
  mutate(frase = paste(word1,word2,sep=" " )) %>% 
  group_by(word1,word2) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))


# Generando red semantica

set.seed(175)
review_bigrams %>%
  filter(n >= 12) %>%   #Filtrar hasta que frecuencia se quiere ver en el mapa
  graph_from_data_frame() %>%  ###Desde aqui en adelante, no tocar nada
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle('Semantic network - Current constitution')


#Grafico barras
review_bigrams %>% 
  ungroup() %>% 
  top_n(20,n) %>%   ##Se selecciona el top, usando la variable n
  mutate(frase = paste(word1,word2,sep=" " )) %>% 
  select(frase,n) %>% 
  mutate(frase = reorder(frase, n)) %>% #Ordena de mayor a menor las palabras
  ggplot(aes(x= n,y=frase)) +
  geom_col()+
  labs(y = NULL)+
  ggtitle("Word pair - Current constitution")







#####################################################################################
# 2nd analysis: Proposal for new constitution
#####################################################################################

#abrir texto a analizar (separara el texto por paginas)
texto <-pdf_text("Propuesta_de_la_Nueva_Constitución.pdf")


#Juntamos todas las paginas en un solo gran texto
texto<-paste(texto, collapse = '')
length(texto)


#Separando el texto en frases (separacion por punto)
vector = c()
for(i in 1:length(texto)){
  temp<-(strsplit(texto[[i]], "\\.")[[1]])
  print(temp)
  vector <- c(vector, temp)
}


#Pasando el texto a formato table (tibble)
texto<-tibble(vector)

#Limpiando texto
texto <-texto %>% 
  mutate(a_eliminar = if_else(str_detect(vector,"CAPÍTULO I – PRINCIPIOS GENERALES"),1,
                              if_else(str_detect(vector,"CONSTITUCIÓN POLÍTICA DE LA REPÚBLICA"),1, 
                                      if_else(str_detect(vector,"CAPÍTULO II – DERECHOS FUNDAMENTALES Y GARANTÍAS"),1,
                                              if_else(str_detect(vector,"CAPÍTULO III – NATURALEZA Y MEDIOAMBIENTE"),1, 
                                                      if_else(str_detect(vector,"CAPÍTULO IV – PARTICIPACIÓN DEMOCRÁTICA "),1, 
                                                              if_else(str_detect(vector,"CAPÍTULO V BUEN GOBIERNO Y FUNCIÓN PÚBLICA"),1, 
                                                                      if_else(str_detect(vector,"CAPÍTULO V BUEN GOBIERNO Y FUNCIÓN PÚBLICA"),1, 
                                                                              if_else(str_detect(vector,"CAPÍTULO VI ESTADO REGIONAL Y ORGANIZACIÓN TERRITORIAL"),1, 
                                                                                      if_else(str_detect(vector,"CAPÍTULO VII PODER LEGISLATIVO"),1, 
                                                                                              if_else(str_detect(vector,"CAPÍTULO VIII PODER EJECUTIVO"),1, 
                                                                                                      if_else(str_detect(vector,"CAPÍTULO IX SISTEMAS DE JUSTICIA"),1, 
                                                                                                              if_else(str_detect(vector,"CAPÍTULO X ÓRGANOS AUTÓNOMOS CONSTITUCIONALES"),1, 
                                                                                                                      if_else(str_detect(vector,"CAPÍTULO XI REFORMA Y REEMPLAZO DE LA CONSTITUCIÓN"),1,
                                                                                                                              if_else(str_detect(vector,"DISPOSICIONES TRANSITORIAS"),1, 
                                                                                                                                      if_else(str_detect(vector,"CAPÍTULO XI REFORMA Y REEMPLAZO DE LA CONSTITUCIÓN"),1, 
                                                                                                                                              if_else(str_detect(vector,"CAPÍTULO XI REFORMA Y REEMPLAZO DE LA CONSTITUCIÓN"),1,
                                                                                                                                                      
                                                                                                                                                      
                                                                                                                              
                                                                      
                                                              
                            0))))))))))))))))) %>% 
  filter(a_eliminar==0) %>% 
  select(vector)

################ PRIMER ANALISIS: TOKEN DE PALABRAS ################

#Pasando el texto a tokens (palabras)
texto2 <- texto %>% 
  unnest_tokens(word,vector,drop = FALSE) #el primer "word" indica que la separacion sera a nivel palabra

texto2$vector<- as.character(texto2$vector)


#Nos creamos un lexicon de stopwords en espa?ol 
lexiconSW<-stopwords("es")
lexiconSW <- append(lexiconSW,c("1","2","3","4","5","n","19","artículo","45","7","letra","12","6","24","113")) #con append se pueden agregar stopwords
lexiconSW<- tibble(lexiconSW)
lexiconSW$lexiconSW <-as.character(lexiconSW$lexiconSW)
lexiconSW<- lexiconSW %>% filter(lexiconSW!="estado") #Eliminar stopwords

#sacando las stopwords al texto
texto3<- anti_join(texto2,lexiconSW, by= c("word"="lexiconSW"))


#Generando tabla de fecuencias de palabras
frec_texto<-texto3 %>% 
  group_by(word) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

frec_texto %>% 
  top_n(20,n) %>%   ##Se selecciona el top, usando la variable n
  mutate(word = reorder(word, n)) %>% #Ordena de mayor a menor las palabras
  ggplot(aes(x= n,y=word)) +
  geom_col()+
  labs(y = NULL)+
  ggtitle("Most used words - Constitution proposal")


#Generamos el wordcloud
wordcloud(words = frec_texto$word, freq = frec_texto$n,
          max.words = 400, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))



############ Bigramas #################
#Es la relacion de dos palabras


review_bigrams <- texto %>%  ##Usamos la tabla sin eliminar por stop words
  unnest_tokens(bigram, vector, token = "ngrams", n = 2) %>%  # ahora, separamos cada 2 token
  separate(bigram, c("word1", "word2"), sep = " ") %>%  # separamos word por bigrama
  filter(!word1 %in% lexiconSW$lexiconSW) %>% 
  filter(!word2 %in% lexiconSW$lexiconSW) %>% 
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>%
  mutate(frase = paste(word1,word2,sep=" " )) %>% 
  group_by(word1,word2) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))


# Generando red semantica

set.seed(175)
review_bigrams %>%
  filter(n >= 12) %>%   #Filtrar hasta que frecuencia se quiere ver en el mapa
  graph_from_data_frame() %>%  ###Desde aqui en adelante, no tocar nada
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle('Semantic network - Constitution proposal')


#Grafico barras
review_bigrams %>% 
  ungroup() %>% 
  top_n(20,n) %>%   ##Se selecciona el top, usando la variable n
  mutate(frase = paste(word1,word2,sep=" " )) %>% 
  select(frase,n) %>% 
  mutate(frase = reorder(frase, n)) %>% #Ordena de mayor a menor las palabras
  ggplot(aes(x= n,y=frase)) +
  geom_col()+
  labs(y = NULL)+
  ggtitle("Word pair - Constitution proposal")








