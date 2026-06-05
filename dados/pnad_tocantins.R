library(PNADcIBGE)
library(tidyverse)

pnad_completa <- get_pnadc(2019, 4, design = FALSE)

dados_recortados <- pnad_completa |> 
  filter(
    UF == "Tocantins",       
    !is.na(VD4016),          
    VD4016 > 0,              
    !is.na(V1028)            
  ) |> 
  select(UF, UPA, V1028, V2007, V2009, VD4016)

dir.create("dados", showWarnings = FALSE)
write_csv(dados_recortados, "tocantins.csv")
