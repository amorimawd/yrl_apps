# Dicionário Nheengatu (2023)

libs <- c("devtools","stringr", "stringi", "dplyr", 
             "tidyr", "readr", "textgRid")

libsInstalled <- libs %in% rownames(installed.packages())

invisible(if (any(libsInstalled == FALSE)) {
  install.packages(pacotes[!libsInstalled], verbose=FALSE, quiet=TRUE)
})

# devtools::install_github("patrickreidy/textgRid", verbose=FALSE, quiet=TRUE)

invisible(lapply(libs, library, character.only = TRUE))


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


tgLista <- list.files(path="./textgrids/", pattern=".TextGrid", all.files=T,
                      full.names=FALSE)


tg <- data.frame()


for (i_file in 1:length(tgLista)){
  
  tgPath <- paste0("./textgrids/", tgLista[i_file])
  
  tgDataframe <- as.data.frame(TextGrid(tgPath, encoding = NULL))
  
  tgDataframe$File <- tgLista[i_file]
 
  tg <-rbind(tg, tgDataframe)
  
  rm(tgDataframe)
  
}

if (length(grep("commen", ignore.case=TRUE, tg$TierName)) != 0) {
  tg <- tg[-grep("commen", ignore.case=TRUE, tg$TierName),]
}




tg <- tg[order(tg$File, tg$StartTime, tg$TierNumber), ]

  
TierNames <-    c("Speaker1", "Morphemes1", "Glosses_1_PT", 
                 "Glosses_1_EN", "Translation_1_PT", "Translation_1_EN",
                 
                 "Speaker2", "Morphemes2", "Glosses_2_PT", 
                 "Glosses_2_EN", "Translation_2_PT", "Translation_2_EN")
  
  
  for (row in 1:nrow(tg)){
    for (tier in 1:max(tg$TierNumber)){
     if (tg$TierNumber[row] == tier){
      tg$TierName[row] <- TierNames[tier]
    }
  }
}


tg <- data.frame(tg, 
                 Morphemes = NA, 
                 GlossesPT_Sentence = NA,
                 GlossesPT_Word = NA,
                 GlossesEN_Sentence = NA,
                 GlossesEN_Word = NA,
                 TranslationPT = NA,
                 TranslationEN = NA,
                 ID = 1:nrow(tg)
                 )


for (row in 1:nrow(tg)){
  if (tg$TierNumber[row] == 2|
      tg$TierNumber[row] == 8){
    
      tg$Morphemes[row]           <- tg$Label[row]
      tg$GlossesPT_Sentence[row]  <- tg$Label[row+1]
      tg$GlossesEN_Sentence[row]  <- tg$Label[row+2]
      tg$TranslationPT[row]       <- tg$Label[row+3]
      tg$TranslationEN[row]       <- tg$Label[row+4]
  }
}


tg <- tg[tg$TierNumber == 2|tg$TierNumber == 8,]

tg <- tg[which(tg$Label != ""),]


for (row in 1:nrow(tg)){
  if (tg$Morphemes[row] == ""){
    tg$Morphemes[row] <- tg$Label[row]
    
  }
}

# tg <- tg[-which(tg$GlossesPT_Sentence==""),]

tg_length <- nrow(tg)

tg$TierName <- as.character(tg$TierName)
tg$TierNumber <- as.character(tg$TierNumber)


# Loop para comprimento do df geral
for (i in 1:tg_length){
  # Loop para comprimento do label
  for (j in 1:length(str_split(tg$Label[i], " ")[[1]])){
    # Salvar cada palavra em Label de acordo com o indice
    extrair <- str_split(tg$Label[i], " ")[[1]]
    # Salvar cada palavra em Glos_FULL de acordo com o indice
    glosa <- str_split(tg$GlossesPT_Sentence[i], " ")[[1]]
    glosa2 <- str_split(tg$GlossesEN_Sentence[i], " ")[[1]]
    # Criar DF
    tg <- rbind(tg, c(tg$TierNumber[i],
                      tg$TierName[i],
                      tg$TierType[i],
                      tg$Index[i],
                      tg$StartTime[i],
                      tg$EndTime[i],
                      extrair[j],
                      tg$File[i],
                      tg$Morphemes[i],
                      tg$GlossesPT_Sentence[i],
                      glosa[j],
                      tg$GlossesEN_Sentence[i],
                      glosa2[j],
                      tg$TranslationPT[i],
                      tg$TranslationEN[i],
                      tg$ID[i]       
                      
                      
    ))
    
    
  }
}


tg <- tg[-c(1:tg_length),]

tg <- tg[-which(tg$Label == ""),]

tg <- tg[,c(1:7, 11, 13, 9, 10, 12, 14, 15, 8, 16)]

tg$Label <- gsub(" ","", gsub('[[:punct:] ]+',' ',tg$Label))

row.names(tg)<-NULL

write_csv(tg, "Dicionario.csv")
