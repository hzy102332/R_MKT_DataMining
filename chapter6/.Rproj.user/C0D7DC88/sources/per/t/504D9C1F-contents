library(tidyverse)
library(tidytext)
library(wordcloud2)
library(stringr)



text = '1 I like rita !!!, ^^;2 Start @ # from here, goes to everywhere';
# remove pounctations
text = str_replace_all(string = text, 
                       pattern = "[:punct:]",
                       replacement = "")
text = str_replace_all(string = text, 
                       pattern = "[0-9]",
                       replacement = "")
text = str_replace_all(string = text, 
                       pattern = "[:^:]",
                       replacement = "")
text = tolower(text)
text = str_squish(text)
text 


halo = readLines("data/1.txt");
en = readLines("data/3en.txt");

text = str_replace_all(string = en, 
                       pattern = "[:punct:]",
                       replacement = "");
text = str_replace_all(string = text, 
                       pattern = "[0-9]",
                       replacement = "");
text = tolower(text);
text = str_squish(text);
en = str_replace_all(string = text, 
                       pattern = "[:^:]",
                       replacement = "");

cn = readLines("data/3cn.txt",encoding = "UTF-8");
text = str_replace_all(string = cn, 
                       pattern = "[:punct:]",
                       replacement = "");
text = str_replace_all(string = text, 
                       pattern = "[0-9]",
                       replacement = "");
text = tolower(text);
text = str_squish(text);
cn = str_replace_all(string = text, 
                       pattern = "[:^:]",
                       replacement = "");
en = tibble(en);
word_space = en %>%
    unnest_tokens(input = en,
                  output = word,
                  token = "words")%>%
    filter(str_count(word)>1)

word_space
top20 = word_space %>% 
    count(word,sort = T)

ggplot(top20,
       aes())
