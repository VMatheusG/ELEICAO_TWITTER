# Pacotes
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(twitteR)
library(pryr)
library(wordcloud)
library(viridisLite)
library(tm)
library(qdap)
library(microbenchmark)
library(stringr)
library(tidytext)
library(ggfortify)
options(dplyr.width = Inf)
########## lexicon PT ##############
library(lexiconPT)
source("funcoes.R",encoding = "UTF-8")
lexPT <- oplexicon_v2.1

# Carregando os dados
filtro_dados <- readRDS("dados/twitter.rds") %>%
            as_tibble() %>%
             filter(isRetweet==F) %>%
            arrange(desc(retweetCount)) %>% 
  select(text,retweetCount,screenName)

Ptolower <- possibly(tolower,NA)
filtro_dados$text <- map_chr(filtro_dados$text, Ptolower)
# padronizando os dados


clean <- function(vetor){
  clean_v <- map_chr(vetor, removePunctuation) %>% 
    map_chr(rm_accent) %>% 
    map_chr(removeNumbers) %>% 
    map_chr(removeWords,
          words = c(stopwords("pt"),"votar","pior","maior","primeiro","dar","ser","sabe","melhor","mundo","mesmo","ver","vao","debateband","ir","sao","so","paulo","debatenaband","debate","vai","pra","candidatos")) %>% 
    map_chr(stripWhitespace)
  return(clean_v)
  
}



filtro_dados$text <- clean(filtro_dados$text)


# padronizando os dados
filtro_dados %<>%  
  filter(!is.na(text)) %>% 
  mutate( linenumber = row_number(),
         text = str_replace_all(text,"(?:bolsonaronaband|estoucombolsonaro|jairbolsonaro|jair bolsonaro|bolsolixo|bozo|capitaonaband|bostonaro|bolsonaronaro|bolsnaro|boronaso)","bolsonaro"),
         text = str_replace_all(text,"bolsonaronaro","bolsonaro"),
         text = str_replace_all(text,"(?:cabo daciolo|cabo)","daciolo"), 
         text = str_replace_all(text,"(?:lula oficial|lulaoficial|lula12018)","lula"),
         text = str_replace_all(text,"(?:guilherme boulos|guilhermeboulos|boulosnaband|guilherme)","boulos"),
         text = str_replace_all(text,"(?:ciro gomes|cirogomes)","ciro"),
         text = str_replace_all(text,"(?:alvoro dias|alvorodias|dias|alvaro)","alvaro"),
         text = str_replace_all(text,"(?:haddad_fernando|fernando haddad)","haddad"),
         text = str_replace_all(text,"(?:alquimim|alquimin|alckimin|alckimim|geraldonaband|geraldo|alkmin|alckmim|geraldinho)","alckmin"),
         text = str_replace_all(text,"(?:joao|joaoamoedo)","amoedo"),
         text = str_replace_all(text,"(?:meireles|henrique meireles|henrique)","meirelles"),
         text = str_replace_all(text,"\\sk+",""),
         text = str_replace_all(text,"https[:graph:]*",""),
         Bolsonaro = str_detect(text,"bolsonaro"),
         Ciro = str_detect(text,"ciro"),
         Haddad = str_detect(text,"haddad"),
         Daciolo = str_detect(text,"daciolo"),
         Alckmin = str_detect(text,"alckmin"),
         Marina = str_detect(text,"marina"),
         Boulos = str_detect(text,"boulos"),
         Lula = str_detect(text,"lula"),
         Amoedo = str_detect(text,"amoedo"),
         Meirelles = str_detect(text,"meirelles"),
         Neutro = !str_detect(text,"(?:bolsonaro|haddad|daciolo|ciro|alckmin|lula|boulos|marina|alvaro|meirelles|amoedo|joao|joaoamoedo|meireles|eymael)"))

gather_dados <-   filtro_dados %>% 
  select(-Neutro) %>% 
  gather(candidato,P,-text,-retweetCount,-screenName,-linenumber) %>% 
  filter(P == TRUE) %>% 
  mutate(P = NULL)


# passado de texto para palavras
table(gather_dados$candidato)
round(prop.table(table(gather_dados$candidato))*100,1)


tidy_dados <- gather_dados %>% 
  unnest_tokens(word, text) %>% 
  inner_join(lexPT,by=c("word"="term")) %>% 
  group_by(candidato,word) %>% 
  summarise(n = n(),polarity = sum(polarity)) %>% 
  arrange(desc(n)) 


teste <- gather_dados %>% 
  mutate(text = str_replace_all(text,"(?:bolsonaro|amoedo|marina|silva|ciro|daciolo|lula|haddad|alckmin|boulos|naband|\\sta\\s|debatecom|fernando|nao|\\sai\\s|\\sq\\s|band|\\sja\\s|\\sfala\\s|porque|\\sr\\s|\\sndo\\s|\\spergun\\s|meirelles|\\sau\\s|\\spro\\s|\\ster\\s|\\stal\\s|\\svc\\s|alvaro|gomes|\\sate\\s)","")) %>% 
  unnest_tokens(word, text) %>% 
  select(candidato,word) %>% 
  count(candidato,word) %>% 
  spread(candidato,n,fill = 0) %>% 
  filter_if(is.numeric,any_vars(.>100)) %>% 
  filter(word != "r") %>% 
  data.frame(row.names = "word")

fit <- princomp(scale(log(as.matrix(teste+1))))
summary(fit)
scores <- data.frame(fit$scores) %>% 
  tibble::rownames_to_column(var = "rownames")
loadings <- data.frame(fit$loadings[,c(1:9)]) %>% 
  tibble::rownames_to_column(var = "rownames")
top2 <- loadings %>% filter()
##comp1 x comp2

ggbiplot(plot.data = scores[,c(2,3,1)],loadings.data = loadings[,c(2,3,1)], loadings = TRUE, loadings.label = TRUE,
        label= T,shape = FALSE) + theme_bw()
ggsave("c1xc2.png",path = "imgs/",scale =2)

##comp1 x comp3
ggbiplot(plot.data = scores[,c(2,4,1)],loadings.data = loadings[,c(2,4,1)], loadings = TRUE, loadings.label = TRUE,
         label= T,shape = FALSE) + theme_bw()
ggsave("c1xc3.png",path = "imgs/",scale =2)
##comp1 x comp4
ggbiplot(plot.data = scores[,c(2,5,1)],loadings.data = loadings[,c(2,5,1)], loadings = TRUE, loadings.label = TRUE,
         label= T,shape = FALSE) + theme_bw()
ggsave("c1xc4.png",path = "imgs/",scale =2)
##comp2 x comp3

ggbiplot(plot.data = scores[,c(3,4,1)],loadings.data = loadings[,c(3,4,1)], loadings = TRUE, loadings.label = TRUE,
         label= T,shape = FALSE) + theme_bw()
ggsave("c2xc3.png",path = "imgs/",scale =2)
# polaridade <- 
# tidy_dados %>% 
#   count(candidato,word,polarity) %>% 
#   rename(n =nn) %>% 
#   mutate(score = polarity*n) %>% 
#   arrange(desc(n))

# saveRDS(tidy_dados,"dados/tidy_dados.RDS")
######### funcao para fazer wordcloud polarizadora ########


########## nuvem de palavras por candidado ####

df <- tidy_dados  %>% 
  select(candidato,word,n) %>% 
  filter(candidato %in% c("Bolsonaro","Daciolo","Ciro")) %>% 
  spread(candidato,n,fill = 0) %>% 
  data.frame(row.names = "word")
# Plot comparison cloud
png("imgs/comparison_cloud.png",width=400*1.7,height=350*1.7,res=150)
comparison.cloud(df, max.words = 70, title.size = .9)
dev.off()

## palavras geral

df <- tidy_dados  %>%
  group_by(word) %>% 
  summarise(nn = sum(n))
wordcloud(df$word,df$nn,max.words = 70)

########### porcentagem de positivo e negativo por candidato #############
df <- tidy_dados  %>% 
  select(candidato,word,polarity) %>% 
  filter(candidato %in% c("Bolsonaro","Daciolo","Ciro"),polarity != 0) %>% 
  mutate(sinal = ifelse(polarity>0,"positivo","negativo")) %>% 
  group_by(candidato,sinal) %>% 
  summarise(soma = sum(polarity)) %>% 
  mutate(soma = ifelse(soma<0,-1*soma,soma)) %>% 
  group_by(candidato) %>% 
  mutate(porcentagem = round(100*soma/sum(soma),1))


ggplot(df, aes(candidato, porcentagem, fill = sinal)) +  
  geom_col()
ggsave("perc_neg_pos_cand.png",path = "imgs/")
############################ palavras positivas e palavras negativas por candidato #########################

df <- tidy_dados  %>% 
  select(candidato,word,polarity) %>% 
  filter(candidato %in% c("Bolsonaro","Daciolo","Ciro"),polarity != 0) %>% 
  mutate(polarity = ifelse(word %in% c("limpar","pagar"),polarity*-1,polarity),sinal = ifelse(polarity>0,"positivo","negativo")) %>% 
  arrange(candidato,sinal) %>% 
  group_by(candidato,sinal) %>% 
  mutate(id = row_number()) %>% 
  filter(id < 8)


ggplot(df, aes(reorder(word, polarity), polarity, fill = sinal)) +
  geom_col() + 
  ylab("polaridade")+ 
  xlab(NULL) +
  facet_grid(.~candidato,scales = "free",space = "free") +
  theme_bw() +
  # Rotate text and vertically justify
  theme(axis.text.x = element_text(angle = 90, vjust =-0.1))+
  theme(legend.position="top")
ggsave("freq_palav_sent_cand.png",path = "imgs/")

