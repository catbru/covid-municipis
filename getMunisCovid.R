library(tidyverse)
library(stringr)
library(zoo)
library(httr)
library(rvest)


url <- "https://wabi-north-europe-api.analysis.windows.net/public/reports/querydata?synchronous=true"


# S'han de fer dues peticions:
# peticio 1
body1 = "{\"version\":\"1.0.0\",\"queries\":[{\"Query\":{\"Commands\":[{\"SemanticQueryDataShapeCommand\":{\"Query\":{\"Version\":2,\"From\":[{\"Name\":\"m\",\"Entity\":\"M_Mesures_TEST\"},{\"Name\":\"d\",\"Entity\":\"dPersona\"}],\"Select\":[{\"Column\":{\"Expression\":{\"SourceRef\":{\"Source\":\"d\"}},\"Property\":\"ComarcaNom\"},\"Name\":\"dPersona.ComarcaNom\"},{\"Column\":{\"Expression\":{\"SourceRef\":{\"Source\":\"d\"}},\"Property\":\"Municipi\"},\"Name\":\"dPersona.Municipi\"},{\"Measure\":{\"Expression\":{\"SourceRef\":{\"Source\":\"m\"}},\"Property\":\"Resultats Negatius\"},\"Name\":\"M_Mesures_TEST.Resultats Negatius\"},{\"Measure\":{\"Expression\":{\"SourceRef\":{\"Source\":\"m\"}},\"Property\":\"Resultats Positius\"},\"Name\":\"M_Mesures_TEST.Resultats Positius\"}]},\"Binding\":{\"Primary\":{\"Groupings\":[{\"Projections\":[0],\"Subtotal\":1},{\"Projections\":[1,2,3],\"Subtotal\":1}]},\"DataReduction\":{\"DataVolume\":3,\"Primary\":{\"Window\":{\"Count\":500}}},\"Version\":1}}}]},\"QueryId\":\"\",\"ApplicationContext\":{\"DatasetId\":\"cb0a4136-7a64-47f1-8270-be5320ef5bf3\",\"Sources\":[{\"ReportId\":\"34dedced-6c95-4e56-83e5-26c024f7927b\"}]}}],\"cancelQueries\":[],\"modelId\":10535817}"
munis1 <- POST(url, body = body1, encode = "json") %>% content(as = 'text')

names1 <- munis1 %>% 
  str_match_all('"D0":(.*?)]') %>% # agafem la llista de municipis
  unlist() %>% 
  paste(collapse=",") %>% 
  str_split(',') %>%  # separem els resultats
  unlist() %>% 
  str_remove_all('\\[') %>% # netegem
  str_remove_all('D0') %>% 
  str_remove_all(':') %>% 
  str_remove_all('"') %>% 
  str_remove_all('\\[') %>% 
  str_remove_all('/') %>% 
  str_remove_all('\\]') %>% 
  as_tibble() %>% # passem a taula i fem join de municipis i resultats
  distinct() %>% 
  mutate(municipi = as.character(1:length(.$value)-1)) %>% 
  full_join( munis1 %>% 
               str_remove_all('DM2(.*?)C') %>% # treiem comarques
               str_match_all('\"C\":(.*?)\\}') %>% # deixem només resultats de municipis
               unlist() %>% 
               str_remove_all('"') %>% # netegem
               str_remove_all('\\[') %>% 
               str_remove_all('\\}') %>% 
               str_remove_all(']') %>% 
               str_remove_all('C:') %>% 
               str_trim() %>% 
               as_tibble() %>%  # convertim a taula
               mutate(value = ifelse(str_detect(value,'R:4'),str_remove(value,'R:4'),value), # resituem valors a la columna que toca
                      value = ifelse(str_detect(value,'R:2'),str_replace(str_remove(value,',R:2'),',',',,'),value),
                      value = ifelse(str_detect(value,'R:6'),paste0(str_remove(value,',R:6'),',,'),value)) %>% 
               separate(value,into = c('municipi','negatius','positius'),sep = ',') %>% 
               mutate(negatius = ifelse(negatius == '',NA,negatius),
                      positius = ifelse(positius == '',NA,positius),
                      positius = na.locf(positius),
                      negatius = na.locf(negatius)) %>% # omplim els valors que falten (falten si és igual a l'anterior)
               distinct()) %>% 
  mutate(value = ifelse(is.na(value),municipi,value)) %>% 
  distinct() %>% 
  select(value, negatius, positius) %>% 
  rename(municipi = value)
  

# peticio 2
ultimMuni <- names1$municipi[length(names1$municipi)]
ultimaComarca <- munis1 %>% 
  str_match_all('"G0":(.*?)",') %>% 
  unlist() %>% 
  str_remove_all('"') %>% # netegem
  str_remove_all('\\[') %>% 
  str_remove_all('\\}') %>% 
  str_remove_all(']') %>% 
  str_remove_all('G0:') %>% 
  str_remove_all(',') %>% 
  unique() %>% 
  .[length(.)]


body2 <- "{\"version\":\"1.0.0\",\"queries\":[{\"Query\":{\"Commands\":[{\"SemanticQueryDataShapeCommand\":{\"Query\":{\"Version\":2,\"From\":[{\"Name\":\"m\",\"Entity\":\"M_Mesures_TEST\"},{\"Name\":\"d\",\"Entity\":\"dPersona\"}],\"Select\":[{\"Column\":{\"Expression\":{\"SourceRef\":{\"Source\":\"d\"}},\"Property\":\"ComarcaNom\"},\"Name\":\"dPersona.ComarcaNom\"},{\"Column\":{\"Expression\":{\"SourceRef\":{\"Source\":\"d\"}},\"Property\":\"Municipi\"},\"Name\":\"dPersona.Municipi\"},{\"Measure\":{\"Expression\":{\"SourceRef\":{\"Source\":\"m\"}},\"Property\":\"Resultats Negatius\"},\"Name\":\"M_Mesures_TEST.Resultats Negatius\"},{\"Measure\":{\"Expression\":{\"SourceRef\":{\"Source\":\"m\"}},\"Property\":\"Resultats Positius\"},\"Name\":\"M_Mesures_TEST.Resultats Positius\"}]},\"Binding\":{\"Primary\":{\"Groupings\":[{\"Projections\":[0],\"Subtotal\":1},{\"Projections\":[1,2,3],\"Subtotal\":1}]},\"DataReduction\":{\"DataVolume\":3,\"Primary\":{\"Window\":{\"Count\":500,\"RestartTokens\":[[false],[\"'Pla d''Urgell'\"],[false],[\"'Castellnou de Seana'\"]]}}},\"Version\":1}}}]},\"QueryId\":\"\"}],\"cancelQueries\":[],\"modelId\":10535817}"
body2 <- paste0("{\"version\":\"1.0.0\",\"queries\":[{\"Query\":{\"Commands\":[{\"SemanticQueryDataShapeCommand\":{\"Query\":{\"Version\":2,\"From\":[{\"Name\":\"m\",\"Entity\":\"M_Mesures_TEST\"},{\"Name\":\"d\",\"Entity\":\"dPersona\"}],\"Select\":[{\"Column\":{\"Expression\":{\"SourceRef\":{\"Source\":\"d\"}},\"Property\":\"ComarcaNom\"},\"Name\":\"dPersona.ComarcaNom\"},{\"Column\":{\"Expression\":{\"SourceRef\":{\"Source\":\"d\"}},\"Property\":\"Municipi\"},\"Name\":\"dPersona.Municipi\"},{\"Measure\":{\"Expression\":{\"SourceRef\":{\"Source\":\"m\"}},\"Property\":\"Resultats Negatius\"},\"Name\":\"M_Mesures_TEST.Resultats Negatius\"},{\"Measure\":{\"Expression\":{\"SourceRef\":{\"Source\":\"m\"}},\"Property\":\"Resultats Positius\"},\"Name\":\"M_Mesures_TEST.Resultats Positius\"}]},\"Binding\":{\"Primary\":{\"Groupings\":[{\"Projections\":[0],\"Subtotal\":1},{\"Projections\":[1,2,3],\"Subtotal\":1}]},\"DataReduction\":{\"DataVolume\":3,\"Primary\":{\"Window\":{\"Count\":500,\"RestartTokens\":[[false],[\"'",str_replace_all(ultimaComarca, "'","''"),"'\"],[false],[\"'",str_replace_all(ultimMuni, "'","''"),"'\"]]}}},\"Version\":1}}}]},\"QueryId\":\"\"}],\"cancelQueries\":[],\"modelId\":10535817}")
munis2 <- POST(url, body = body2, encode = "json") %>% content(as = 'text')

names2 <- munis2 %>% 
  str_match_all('"D0":(.*?)]') %>% # agafem la llista de municipis
  unlist() %>% 
  paste(collapse=",") %>% 
  str_split(',') %>%  # separem els resultats
  unlist() %>% 
  str_remove_all('\\[') %>% # netegem
  str_remove_all('D0') %>% 
  str_remove_all(':') %>% 
  str_remove_all('"') %>% 
  str_remove_all('\\[') %>% 
  str_remove_all('/') %>% 
  str_remove_all('\\]') %>% 
  as_tibble() %>% # passem a taula i fem join de municipis i resultats
  distinct() %>% 
  mutate(municipi = as.character(1:length(.$value)-1)) %>% 
  full_join( munis2 %>% 
               str_remove_all('DM2(.*?)C') %>% # treiem comarques
               str_match_all('\"C\":(.*?)\\}') %>% # deixem només resultats de municipis
               unlist() %>% 
               str_remove_all('"') %>% # netegem
               str_remove_all('\\[') %>% 
               str_remove_all('\\}') %>% 
               str_remove_all(']') %>% 
               str_remove_all('C:') %>% 
               str_trim() %>% 
               as_tibble() %>%  # convertim a taula
               mutate(value = ifelse(str_detect(value,'R:4'),str_remove(value,'R:4'),value), # resituem valors a la columna que toca
                      value = ifelse(str_detect(value,'R:2'),str_replace(str_remove(value,',R:2'),',',',,'),value),
                      value = ifelse(str_detect(value,'R:6'),paste0(str_remove(value,',R:6'),',,'),value)) %>% 
               separate(value,into = c('municipi','negatius','positius'),sep = ',') %>% 
               mutate(negatius = ifelse(negatius == '',NA,negatius),
                      positius = ifelse(positius == '',NA,positius),
                      positius = na.locf(positius),
                      negatius = na.locf(negatius)) %>% # omplim els valors que falten (falten si és igual a l'anterior)
               distinct()) %>% 
  mutate(value = ifelse(is.na(value),municipi,value)) %>% 
  distinct() %>% 
  select(value, negatius, positius) %>% 
  rename(municipi = value)

tots <- rbind(names1,names2) %>% 
  mutate(nom_lower = tolower(municipi)) %>% 
  filter(!is.na(negatius) & !is.na(positius))

## corregim alguns nomes de municipi a mà per poder fer el match
tots$nom_lower[tots$nom_lower == "saus camallera i llampaies"] <-  "camallera i llampaies saus"
tots$nom_lower[tots$nom_lower == "vinyols i  els arcs"] <- "vinyols i els arcs"
tots$nom_lower[tots$nom_lower == "cruïlles monells i sant sadurní de l'heura"] <-  "monells i sant sadurní de l'heura cruïlles"
tots$nom_lower[tots$nom_lower == "catellví de rosanes"] <-  "castellví de rosanes"
tots$nom_lower[tots$nom_lower == "sant adrià del besòs"] <-  "sant adrià de besòs"
tots$nom_lower[tots$nom_lower == "mortellà i martinet"] <-  "montellà i martinet"
tots$nom_lower[tots$nom_lower == "torres del segre"] <-  "torres de segre"
tots$nom_lower[tots$nom_lower == "brunyola"] <-  "brunyola i sant martí sapresa"
tots$nom_lower[tots$nom_lower == "fogars de tordera"] <-  "fogars de la selva"
tots$nom_lower[tots$nom_lower == "vespella"] <-  "vespella de gaià"
tots$nom_lower[tots$nom_lower == "santa perpétua de mogoda"] <-  "santa perpètua de mogoda"

rm(names1,names2)

# hi afegim els codis de municipi i comarca de Idescat
codis <- read_html('https://www.idescat.cat/codis/?id=50&n=9') %>% 
  html_table() %>% 
  .[[1]] %>% 
  group_by(Nom) %>% 
  mutate(municipi = str_trim(str_replace(paste(rev(unlist(str_split(Nom, ','))), collapse = ' '),"' ", "'")),
         nom_lower = tolower(municipi))

munis <- full_join(codis,tots, by = c('nom_lower')) %>% 
  as_tibble() %>% 
  select(Codi, Nom, `Codi comarca`, `Nom comarca`, negatius, positius)

munis[is.na(munis)] <- '0'

munis <- munis %>% 
  mutate(negatius = as.numeric(negatius),
         positius = as.numeric(positius))

rm(tots, codis, body1, body2, munis1, munis2, url)

write.csv2(munis, 'munisCovid.csv', row.names = FALSE)

