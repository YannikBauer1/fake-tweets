library(stringr)
library(dplyr)
library(ggplot2)

data <- read.csv("FN-Dataset-18k.csv")
data <- as_tibble(data)
data <- select(data, -id)

abc <- str_extract_all(select(data,"title"),"https\\S+")
abc <- as_tibble(table(abc))
abc$n
sum(unlist(abc$n))


data <- mutate(data, nChar=nchar(data$description))
data <- mutate(data, nWords=str_count(data$description," ")+1)


dFalse <- filter(data,questionable_domain==FALSE)
dTrue <- filter(data,questionable_domain==TRUE)


data %>% group_by(questionable_domain) %>% count()
f <- 14950
t <- 3000





#-----------------------------------------------------------------------------#
#------------------------analysis das variaveis do dataset--------------------#
#-----------------------------------------------------------------------------#


ggplot(data, aes(x=questionable_domain,y=user_friends_count)) + geom_boxplot()
data %>% group_by(questionable_domain) %>% 
  summarise(min = min(user_friends_count),q1 = quantile(user_friends_count, 0.25),median = median(user_friends_count),
            mean = mean(user_friends_count),q3 = quantile(user_friends_count, 0.75),max = max(user_friends_count))
# q1, median, mean e q3 estão maiores para os False News mas nao há overlaps 
# entre os quartils



ggplot(data, aes(x=questionable_domain,y=user_followers_count)) + geom_boxplot()
data %>% group_by(questionable_domain) %>% 
  summarise(min = min(user_followers_count),q1 = quantile(user_followers_count, 0.25),median = median(user_followers_count),
            mean = mean(user_followers_count),q3 = quantile(user_followers_count, 0.75),max = max(user_followers_count))
# a destribuicao é parecida com o user_friends_count



ggplot(data, aes(x=questionable_domain,y=user_favourites_count)) + geom_boxplot()
data %>% group_by(questionable_domain) %>% 
  summarise(min = min(user_favourites_count),q1 = quantile(user_favourites_count, 0.25),median = median(user_favourites_count),
            mean = mean(user_favourites_count),q3 = quantile(user_favourites_count, 0.75),max = max(user_favourites_count))
# o user_favourites_count parece ter ainda menos influencia do que
# user_friends_count e user_followers_count



ggplot(data, aes(x=questionable_domain,y=favorite_count)) + geom_boxplot()
data %>% group_by(questionable_domain) %>% 
  summarise(min = min(favorite_count),q1 = quantile(favorite_count, 0.25),median = median(favorite_count),
            mean = mean(favorite_count),q3 = quantile(favorite_count, 0.75),max = max(favorite_count))
# o favorite_count nao parece ter muito significado para que uma News e false ou true



ggplot(data, aes(x=questionable_domain,y=retweet_count)) + geom_boxplot()
data %>% group_by(questionable_domain) %>% 
  summarise(min = min(retweet_count),q1 = quantile(retweet_count, 0.25),median = median(retweet_count),
            mean = mean(retweet_count),q3 = quantile(retweet_count, 0.75),max = max(retweet_count))
# o retweet count também nao parece ter grande significado



ggplot(data, aes(x=user_verified)) + geom_bar() + facet_wrap(~questionable_domain)
data %>% group_by(questionable_domain,user_verified) %>% count() %>% 
  mutate(perc=ifelse(questionable_domain==FALSE,n/f,n/t))
# a perc dos user_verified nos FalseNews é muito pequeno (0,8%), nos TrueNews
# também nao é muito alto, mas é 6% o que é mais alto que nos FalseNews
# por isso podia ser interessante ficar com esta variavel



ggplot(data, aes(x=contains_profanity)) + geom_bar() + facet_wrap(~questionable_domain)
data %>% group_by(questionable_domain,contains_profanity) %>% count() %>% 
  mutate(perc=ifelse(questionable_domain==FALSE,n/f,n/t))
# a perc para conter profanity nos TrueNews é um pouco mais que 9%, para os 
# falseNews é um pouco mais que 4%, assim parece ter menos influencia dos
# user_verified




#-----------------------------------------------------------------------------#
#------------------------analysis tamanho do tweet----------------------------#
#-----------------------------------------------------------------------------#


ggplot(data, aes(x=questionable_domain,y=nChar)) + geom_boxplot()
data %>% group_by(questionable_domain) %>% 
  summarise(min = min(nChar),q1 = quantile(nChar, 0.25),median = median(nChar),
            mean = mean(nChar),q3 = quantile(nChar, 0.75),max = max(nChar))
ggplot(data, aes(x=questionable_domain,y=nWords)) + geom_boxplot()
data %>% group_by(questionable_domain) %>% 
  summarise(min = min(nWords),q1 = quantile(nWords, 0.25),median = median(nWords),
            mean = mean(nWords),q3 = quantile(nWords, 0.75),max = max(nWords))
# no nChar dá para ver que o median das TrueNews é quase igual ao q3 das FalseNews
# e o median das False news é quase igual ao q1 das TrueNews
# no nWords o valor median das FalseNews é igual ao q1 das TrueNews e só há
# uma palavra de diferenca entre o median das TrueNews e o q3 das FalseNews
# isto significa que o numero de palavras por tweet e também o numero de Char por 
# tweet pode nos dar uma ideia se é falseNews ou TrueNews porque TrueNews 
# normalmente estão mais longo

# Assim é melhor ficar com a coluna nWords que parece ter mais poder que o nChar
data <- select(data, -nChar)




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

hashtags <- merge(hF,hT, by="hashtag", all=T)
hashtags[is.na(hashtags)] <- 0
hashtags[order(-hashtags$trueNews),]
hashtags[order(-hashtags$falseNews),]
hashtags <- mutate(hashtags,percFalseNews=hashtags$falseNews/(hashtags$trueNews+hashtags$falseNews)) %>% 
  mutate(hashtags,percTrueNews=hashtags$trueNews/(hashtags$trueNews+hashtags$falseNews))

# assumimos que é preciso um hashtag acontecer pelo menos 5 para analizar
hashF <- hashtags %>% filter(hashtags$falseNews>5, percFalseNews>0.5) %>% .[order(-.$percFalseNews, -.$falseNews),] 
hashF
# só há 8 hashtags com percFalseNews > 90% que aparecem mais que 5 vezes
# há 1 entre 80% e 90%
# todos os hashtags aparecem poucas vezes, o maximo é 12
# decidimos ficar com os seguintes minimos para a classe de HQ
percDecisionHashtagsF <- 0.9
minVezesHashtagsF <- 10

HQ <- filter(hashtags,percFalseNews>=percDecisionHashtagsF, falseNews>=minVezesHashtagsF)
HQ <- HQ$hashtag
HQ

# assumimos que é preciso um hashtag acontecer pelo menos 10 para analizar
hashT <- hashtags %>% filter(hashtags$trueNews>10, percTrueNews>0.5) %>% .[order(-.$percTrueNews, -.$trueNews),]
hashT
hist(hashT$percTrueNews)
# há mais que 70 hashtags com percTrueNews > 90%
# o resto não nos parece ser interessante
# #SmartNews, #COVID19 #Trump são os hashtags com mais poder
# como há 5 vezes mais tweets com trueNews que com falseNews decidimos ficar com
# um minVezes maior que nos falseNews
percDecisionHashtagsT <- 0.9
minVezesHashtagsT <- 20

HNQ <- filter(hashtags,percTrueNews>=percDecisionHashtagsT, trueNews>=minVezesHashtagsT)
HNQ <- HNQ$hashtag
HNQ

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

# !!!!! Resumo

# !!!!! Como HQ só tem 2 hashtags que também só aparecem 12 vezes cada um pode
# !!!!! ser que esta coluna não vai ser muito util


# !!!!! Os hashtags em HNQ já aparecem mais vezes, se calhar com um
# !!!!! minVezesHashtagsT maior que 20 podemos obter melhor resultados

# !!!!! Assim outra ideia era só considerar os 3 hashtags mais poderosos

percDecisionHashtagsT2 <- 0.9
minVezesHashtagsT2 <- 50
HNQ2 <- filter(hashtags,percTrueNews>=percDecisionHashtagsT2, trueNews>=minVezesHashtagsT2)
HNQ2 <- HNQ2$hashtag
HNQ2
containHNQ2 <- function(string){
  l1 <- c()
  for (j in string) {
    s = FALSE
    hash <- str_extract_all(j,"#\\S+")
    for (h in hash){
      if (identical(h,character(0))){
        break
      } else {
        for (hh in h){
          if (hh %in% HNQ2) {
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
data <- data %>% mutate(HNQ2=ifelse(containHNQ2(data$title)==TRUE,1,0))
# HNQ2 só com hashtags mais poderosos
# só escolher um dos dois ou nenhum para o prediction

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

mentioned <- merge(mF,mT, by="mentioned", all=T)
mentioned[is.na(mentioned)] <- 0
mentioned[order(-mentioned$trueNews),]
mentioned[order(-mentioned$falseNews),]
mentioned <- mutate(mentioned,percFalseNews=falseNews/(trueNews+falseNews))
mentioned <- mutate(mentioned,percTrueNews=trueNews/(trueNews+falseNews))
mentioned


# decidimos considerar so mentioned que aparecem mais que 10 vezes nos falseNews
mentionedF <- mentioned %>% filter(falseNews>10, percFalseNews>0.5) %>% .[order(-.$percFalseNews, -.$falseNews),]
mentionedF
# há muitos hashtags que aparecem numa percFalseNews > 90% e com um count falseNews
# mais ou menos alto
# o @gatewqypundit é muito poderoso, aparece mais que 1000 vezes em falseNews
# decidimos ficar com as seguintes limites para a classe MQ
percDecisionMentionedF <- 0.9
minVezesMentionedF <- 20
MQ <- filter(mentioned,percFalseNews>=percDecisionMentionedF, falseNews>=minVezesMentionedF)
MQ <- MQ$mentioned
MQ



mentionedT <- mentioned %>% filter(trueNews>20, percTrueNews>0.5) %>% .[order(-.$percTrueNews, -.$trueNews),]
mentionedT
# há muitos mentined com percTrueNews > 90% e trueNews count alto
# ficamos com as seguintes limites
percDecisionMentionedT <- 0.9
minVezesMentionedT <- 50
MNQ <- filter(mentioned,percTrueNews>=percDecisionMentionedT, trueNews>=minVezesMentionedT)
MNQ <- MNQ$mentioned
MNQ

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


# !!!! Resumo

# !!!! nos FalseNews @gatewqypundit é muito poderoso como este aparecem em 1/3
# !!!! dos falseNews
# !!!! para a classe MQ podemos se calhar aumentar o minVezes

# !!!! a classe MNQ parece bem como tem um minVezes de 50 o que nos parece ser
# !!!! um bom limit, mas também podimos experimentar com mais que 50

percDecisionMentionedF2 <- 0.9
minVezesMentionedF2 <- 50
MQ2 <- filter(mentioned,percFalseNews>=percDecisionMentionedF2, falseNews>=minVezesMentionedF2)
MQ2 <- MQ2$mentioned
MQ2
containMQ2 <- function(string){
  l1 <- c()
  for (j in string) {
    s = FALSE
    ment <- str_extract_all(j,"@\\S+")
    for (h in ment){
      if (identical(h,character(0))){
        break
      } else {
        for (hh in h){
          if (hh %in% MQ2) {
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
data <- data %>% mutate(MQ2=ifelse(containMQ2(data$title)==TRUE,1,0))
# só com mentioned mais poderosos

percDecisionMentionedF3 <- 0.9
minVezesMentionedF3 <- 100
MQ3 <- filter(mentioned,percFalseNews>=percDecisionMentionedF3, falseNews>=minVezesMentionedF3)
MQ3 <- MQ3$mentioned
MQ3
containMQ3 <- function(string){
  l1 <- c()
  for (j in string) {
    s = FALSE
    ment <- str_extract_all(j,"@\\S+")
    for (h in ment){
      if (identical(h,character(0))){
        break
      } else {
        for (hh in h){
          if (hh %in% MQ3) {
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
data <- data %>% mutate(MQ3=ifelse(containMQ3(data$title)==TRUE,1,0))
# só com mentioned muito poderosos

percDecisionMentionedT2 <- 0.9
minVezesMentionedT2 <- 100
MNQ2 <- filter(mentioned,percTrueNews>=percDecisionMentionedT2, trueNews>=minVezesMentionedT2)
MNQ2 <- MNQ2$mentioned
MNQ2
containMNQ2 <- function(string){
  l1 <- c()
  for (j in string) {
    s = FALSE
    ment <- str_extract_all(j,"@\\S+")
    for (h in ment){
      if (identical(h,character(0))){
        break
      } else {
        for (hh in h){
          if (hh %in% MNQ2) {
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
data <- data %>% mutate(MNQ2=ifelse(containMNQ2(data$title)==TRUE,1,0))


#-----------------------------------------------------------------------------#
#------------------------Url analysis-----------------------------------------#
#-----------------------------------------------------------------------------#

uT <- str_extract_all(select(dTrue,"title"),"https\\S+")
uT <- as_tibble(table(uT))
names(uT) <- c("url","falseNews")
uT

uF <- str_extract_all(select(dFalse,"title"),"https\\S+")
uF <- as_tibble(table(uF))
names(uF) <- c("url","trueNews")
uF

url <- merge(uF,uT, by="url", all=T)
url[is.na(url)] <- 0
url[order(-url$trueNews),]
url[order(-url$falseNews),]
url <- mutate(url,percFalseNews=falseNews/(trueNews+falseNews))
url <- mutate(url,percTrueNews=trueNews/(trueNews+falseNews))
url


urlF <- url %>% filter(falseNews>2, percFalseNews>0.5) %>% .[order(-.$percFalseNews, -.$falseNews),]
urlF
# o maximo de vezes que um url aparece nos falseNews com percFalseNews > 90%
# é 4 o que não tem significado em 3000 tweets
# por isso nao vale a pena criar uma coluna para os questionable urls
percDecisionUrlF <- 0.9
minVezesUrlF <- 3
UQ <- filter(url,percFalseNews>=percDecisionUrlF, falseNews>=minVezesUrlF)
UQ <- UQ$url
UQ

urlT <- url %>% filter(trueNews>5, percTrueNews>0.5) %>% .[order(-.$percTrueNews, -.$trueNews),]
urlT
# temos muitos url com PercTrueNews = 100% que também aparecem mais ou menos muito
# maximo é 44
# top 13 é aparecem mais que 10 vezes
# assim ficamos com as seguintes limites
percDecisionUrlT <- 0.9
minVezesUrlT <- 10
UNQ <- filter(url,percTrueNews>=percDecisionUrlT, trueNews>=minVezesUrlT)
UNQ <- UNQ$url
UNQ

containUNQ <- function(string){
  l1 <- c()
  for (j in string) {
    s = FALSE
    ment <- str_extract_all(j,"https\\S+")
    for (h in ment){
      if (identical(h,character(0))){
        break
      } else {
        for (hh in h){
          if (hh %in% UNQ) {
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
containUQ <- function(string){
  l1 <- c()
  for (j in string) {
    s = FALSE
    ment <- str_extract_all(j,"https\\S+")
    for (h in ment){
      if (identical(h,character(0))){
        break
      } else {
        for (hh in h){
          if (hh %in% UQ) {
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

data <- data %>% mutate(UQ=ifelse(containUQ(data$title)==TRUE,1,0))
data <- data %>% mutate(UNQ=ifelse(containUNQ(data$title)==TRUE,1,0))

# !!!! Resumo

# !!!! se calhar podemos aumentar o minVezes para UrlT, 10 parece pouco em 15000
# !!!! mas como o maximo é 44 podemos considerar nao usar a classe UNQ



write.csv(data,file="my.csv",row.names = FALSE)
