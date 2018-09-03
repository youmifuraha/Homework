text = readLines("declare.txt")
head(text)

library(wordcloud)
library(RColorBrewer)
library(stringr)
library(dplyr)
library(KoNLP)

useSejongDic()

text
text = str_trim(text, side = "right")
text = gsub("[[:punct:]]", "", text)

nouns = extractNoun(text)
wordcount = table(unlist(nouns))
wordcount

df = as.data.frame(wordcount, stringsAsFactors = F)
head(df)
df
View(df)
df = rename(df,
            word = Var1,
            freq = Freq)

head(df)
View(df)

# 2글자 이상 단어 추출
noun.list = sapply(df, USE.NAMES = F, FUN = extractNoun)
noun.list = lapply(noun.list, function (x) return(x[str_length(x) >= 2]))

head(noun.list)

library(dplyr)

top20 = df %>% 
  arrange(desc(freq)) %>% 
  head(20)

top20

library(wordcloud)

pal = brewer.pal(8, "Dark2")
set.seed(1234)
wordcloud(words = df$word,
          freq = df$freq,
          min.freq = 10,
          max.words = 50,
          random.order = F,
          rot.per = .1,
          scale = c(6, 0.2),
          colors = pal)



library(wordcloud2)

allnoun = unlist(noun.list)
head(allnoun,20)

allnoun.table = table(allnoun)
allnoun.table = sort(allnoun.table,decreasing = T)
head(allnoun.table,30)
allnoun

wordcloud2(allnoun.table,minSize = 5)

#ㅠ.ㅠ