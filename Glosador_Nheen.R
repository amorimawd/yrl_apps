# Glosador Nheengatu (2024)

library(tidyverse)
library(readr)
library(readxl)
library(stringdist)

# Bibliotecas a serem carregadas e/ou instaladas
pacotes <- c("devtools","stringr", "dplyr", 
             "tidyr", "readr", "textgRid")

# Instalar pacotes caso ainda não instalados
pacotesinstalados <- pacotes %in% rownames(installed.packages())

invisible(if (any(pacotesinstalados == FALSE)) {
  install.packages(pacotes[!pacotesinstalados], verbose=FALSE, quiet=TRUE)
})

#  devtools::install_github("patrickreidy/textgRid", verbose=FALSE, quiet=TRUE)


# Carregar pacotes
invisible(lapply(pacotes, library, character.only = TRUE))

# Definir wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Listar TextGrid
listatg <- list.files(path=".", pattern=".TextGrid", all.files=T,
                      full.names=FALSE)

# Abrir TextGrid
tg <-TextGrid(listatg[1],encoding = NULL)

# Abrir planilha nheengatu

Dic <- read_delim("./Dicionario.csv", 
                  delim = ",", escape_double = FALSE, trim_ws = TRUE)

# Salvar camada glosas P1

glosas <- tg[[3]]@labels

glosas <- gsub('[[:punct:] ]+',' ', glosas) # remover pontuação

glosas <- trimws(glosas) # remover espaços

# Apagar rótulos glosas P1

tg[[3]]@labels <- rep("", length(tg[[3]]@labels))

# Glosar camada glosas P1


for (j in 1:length(tg[[3]]@labels)){
  if (glosas[j] != ""){
  for (k in 1:length(str_split(glosas[j], " ")[[1]])){
    tg[[3]]@labels[j] <- paste0(tg[[3]]@labels[j], " ", Dic$GlossesPT_Word[
      grep(
        str_replace_all(
          paste("^",gsub('[[:punct:] ]+',' ', str_split(glosas[j], " ")[[1]][k]), "$"), 
          " ", ""), 
        Dic$Label, ignore.case=TRUE, value=F)
    ][1])
    
    
  }
  
}
}

# Substituir NA por ?

for (na in 1:length(tg[[3]]@labels)) {
  tg[[3]]@labels[na] <- gsub("(NA )|( NA)", " ? ", tg[[3]]@labels[na])
  tg[[3]]@labels[na] <- trimws(tg[[3]]@labels[na])
  tg[[3]]@labels[na] <- gsub("  ", " ", tg[[3]]@labels[na])
}


######## Glosas P2 ##########

# Salvar camada glosas P2

glosas <- tg[[9]]@labels

glosas <- gsub('[[:punct:] ]+',' ', glosas) # remover pontuação

glosas <- trimws(glosas) # remover espaços

# Apagar rótulos glosas P1

tg[[9]]@labels <- rep("", length(tg[[9]]@labels))

# Glosar camada glosas P1

for (j in 1:length(tg[[9]]@labels)){
  if (glosas[j] != ""){
  for (k in 1:length(str_split(glosas[j], " ")[[1]])){
    tg[[9]]@labels[j] <- paste0(tg[[9]]@labels[j], " ", Dic$GlossesPT_Word[
      grep(
        str_replace_all(
          paste("^",gsub('[[:punct:] ]+',' ', str_split(glosas[j], " ")[[1]][k]), "$"), 
          " ", ""), 
        Dic$Label, ignore.case=TRUE, value=F)
    ][1])
    
    
  }
  
}
}

# Substituir NA por ?

for (i in 1:300){
  print(tg[[9]]@labels[i])
  print(i)
}


for (na in 1:length(tg[[9]]@labels)) {
  tg[[9]]@labels[na] <- gsub("(NA )|( NA)", " ? ", tg[[9]]@labels[na]) 
  tg[[9]]@labels[na] <- trimws(tg[[9]]@labels[na])
  tg[[9]]@labels[na] <- gsub("  ", " ", tg[[9]]@labels[na])
}


# Ciar novo Textgrid

writeTextGrid(tg, path = paste0(tools::file_path_sans_ext(listatg[1]),"__glosa", ".TextGrid"))










