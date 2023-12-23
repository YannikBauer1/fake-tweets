library(stringr)
library(dplyr)

data <- read.csv("FN-Dataset-18k.csv")
data <- as_tibble(data)

dFalse <- filter(data,questionable_domain==FALSE)
dTrue <- filter(data,questionable_domain==TRUE)



#-----------------------------------------------------------------------------#
#------------------------hashtags analysis------------------------------------#
#-----------------------------------------------------------------------------#

hT <- str_extract_all(select(dTrue,"title"),"#\\S+")
hT <- as_tibble(table(hT))
names(hT) <- c("hashtag","falseNews")
hT

hF <- str_extract_all(select(dFalse,"title"),"#\\S+")
hF <- as_tibble(table(hF))
names(hF) <- c("hashtag","trueNews")
hF

hashtags <- merge(hF,hT)
hashtags[order(-hashtags$trueNews),]
hashtags[order(-hashtags$falseNews),]
hashtags <- mutate(hashtags,percFalseNews=hashtags$falseNews/(hashtags$trueNews+hashtags$falseNews)) %>% 
  mutate(hashtags,percTrueNews=hashtags$trueNews/(hashtags$trueNews+hashtags$falseNews))
hashtags[order(-hashtags$percFalseNews),] 
  # maximo percFalseNews é 85%
  # há 7 hashtags que tem percFalseNews >=80%
  # mas aparecem todos poucas vezes (4,5,6)
hashtags[order(-hashtags$percTrueNews),]
  # há muitos hashtags com percTrueNews >=90%
  # há muitos hashtags com percTrueNews >=80%

# Como para falseNews nao há percFalseNews acima de 90% decidi usar como limit
# para criar os classes 80%, mas podemos mudar como nos achamos melhor
percDecisionHashtags <- 0.8
HNQ <- filter(hashtags,percTrueNews>=percDecisionHashtags)
HNQ <- HNQ$hashtag
HNQ
HQ <- filter(hashtags,percFalseNews>=percDecisionHashtags)
HQ <- HQ$hashtag
HQ

containHNQ <- function(string){
  l1 <- c()
  for (j in string) {
    s = FALSE
    hash <- str_extract_all(j,"#\\S+")
    for (h in hash){
      if (identical(h,character(0))){
        break
      } else {
        for (hh in h){
          if (hh %in% HNQ) {
            s = TRUE
            break
          }
        }
      }
    }
    l1 <- c(l1,s)
  }
  return(l1)
}
containHQ <- function(string){
  l1 <- c()
  for (j in string) {
    s = FALSE
    hash <- str_extract_all(j,"#\\S+")
    for (h in hash){
      if (identical(h,character(0))){
        break
      } else {
        for (hh in h){
          if (hh %in% HQ) {
            s = TRUE
            break
          }
        }
      }
    }
    l1 <- c(l1,s)
  }
  return(l1)
}

data <- data %>% mutate(HQ=ifelse(containHQ(data$title)==TRUE,1,0))
data <- data %>% mutate(HNQ=ifelse(containHNQ(data$title)==TRUE,1,0))




#-----------------------------------------------------------------------------#
#------------------------mentioned analysis-----------------------------------#
#-----------------------------------------------------------------------------#

mT <- str_extract_all(select(dTrue,"title"),"@\\S+")
mT <- as_tibble(table(mT))
names(mT) <- c("mentioned","falseNews")
mT

mF <- str_extract_all(select(dFalse,"title"),"@\\S+")
mF <- as_tibble(table(mF))
names(mF) <- c("mentioned","trueNews")
mF

mentioned <- merge(mF,mT)
mentioned[order(-mentioned$trueNews),]
mentioned[order(-mentioned$falseNews),]
mentioned <- mutate(mentioned,percFalseNews=falseNews/(trueNews+falseNews))
mentioned <- mutate(mentioned,percTrueNews=trueNews/(trueNews+falseNews))
mentioned
mentioned[order(-mentioned$percFalseNews),]
  # temos muitos mentioned com PercFalseNews >=90% que também aparecem muitas 
  # vezes
mentioned[order(-mentioned$percTrueNews),]
  # temos muitos mentioned com PercTrueNews >=90% que também aparecem muitas 
  # vezes

 
# nos dois temos muitas mentioned com >=90%
# por isso podemos usar 90% ou 80% como nos hashtags para a percentagem de desicao
percDecisionMentioned <- 0.8
MNQ <- filter(mentioned,percTrueNews>=percDecisionMentioned)
MNQ <- MNQ$mentioned
MNQ
MQ <- filter(mentioned,percFalseNews>=percDecisionMentioned)
MQ <- MQ$mentioned
MQ

containMNQ <- function(string){
  l1 <- c()
  for (j in string) {
    s = FALSE
    ment <- str_extract_all(j,"@\\S+")
    for (h in ment){
      if (identical(h,character(0))){
        break
      } else {
        for (hh in h){
          if (hh %in% MNQ) {
            s = TRUE
            break
          }
        }
      }
    }
    l1 <- c(l1,s)
  }
  return(l1)
}
containMQ <- function(string){
  l1 <- c()
  for (j in string) {
    s = FALSE
    ment <- str_extract_all(j,"@\\S+")
    for (h in ment){
      if (identical(h,character(0))){
        break
      } else {
        for (hh in h){
          if (hh %in% MQ) {
            s = TRUE
            break
          }
        }
      }
    }
    l1 <- c(l1,s)
  }
  return(l1)
}

data <- data %>% mutate(MQ=ifelse(containMQ(data$title)==TRUE,1,0))
data <- data %>% mutate(MNQ=ifelse(containMNQ(data$title)==TRUE,1,0))


