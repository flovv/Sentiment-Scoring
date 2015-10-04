
#############################



readAndflattenSentiWS <- function(filename) { 
  words = readLines(filename, encoding="UTF-8")
  words <- sub("\\|[A-Z]+\t[0-9.-]+\t?", ",", words)
  words <- unlist(strsplit(words, ","))
  words <- tolower(words)
  return(words)
}

tryTolower = function(x)
{
  # create missing value
  # this is where the returned value will be
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}


pos.words <- c(scan("Senti/positive-words.txt",what='character', comment.char=';', quiet=T), 
               readAndflattenSentiWS("Senti/SentiWS_v1.8c_Positive.txt"))
neg.words <- c(scan("Senti/negative-words.txt",what='character', comment.char=';', quiet=T), 
               readAndflattenSentiWS("Senti/SentiWS_v1.8c_Negative.txt"))


# bring in the sentiment analysis algorithm
# we got a vector of sentences. plyr will handle a list or a vector as an "l" 
# we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{ 
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words) 
  {
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tryTolower(sentence)
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, 
  pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

# and to see if it works, there should be a score...either in German or in English
sample = c("ich liebe dich. du bist wunderbar","I hate you. Die!");
sample
test.sample = score.sentiment(sample, pos.words, neg.words);
test.sample

## simple 

tweets_full <- read.csv("data/EntryReport_Obi_twitter.csv", sep=";")
tweets <- as.character(tweets_full$Entry.Text)

out <- score.sentiment(tweets, pos.words, neg.words)

###############################################################################################################
################################################################ WITH EMOTIcONS! ###############################
###############################################################################################################

emoticon.positive <- c(        "\\:\\-\\)","\\:\\)","\\=\\)",
                               "\\:\\-D","\\:D","8\\-D","8D","x\\-D","xD","X\\-D","XD",
                               "\\:\\'\\-\\(","\\:\\'\\(\\)",
                               "\\:\\'\\-\\)","\\:\\'\\)",
                               "\\:\\*","\\;\\-\\)","\\;\\)",
                               "\\%\\-\\)",
                               "\\<3","\\<\\/3" )

emoticon.negative <- c(  "\\:\\-\\(","\\:\\(",
                         "\\:\\-\\|","\\:\\|")

FindMatches <- function(text, emoticon){
  if(length(grep(x=text, pattern=emoticon)) ==1){
    return(1)
    break
  }
  else{
    return(0)
  }
}

NumberOfEmoticons <- function(txt, emoticon.list){

  l<- llply(emoticon.list, FindMatches, text=txt)  
  return(sum(unlist(l)))
}




score.sentiment = function(sentences, pos.words, neg.words, pos.Emo, neg.Emo, .progress='none')
{ 
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words, pos.Emo, neg.Emo) 
  {
    
    ############################################## EMOTICONS!!!
    
    pos.emo.matches = NumberOfEmoticons(sentence, pos.Emo)
    neg.emo.matches = NumberOfEmoticons(sentence, neg.Emo)
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tryTolower(sentence)
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches) + 3*pos.emo.matches - 3*neg.emo.matches
    return(score)
  }, 
  pos.words, neg.words, pos.Emo, neg.Emo, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


sample = c(":)", "shit what happend :D"  ,"ich liebe dich. du bist wunderbar","I hate you. Die!");
sample
test.sample = score.sentiment(sample, pos.words, neg.words, emoticon.positive, emoticon.negative);

test.sample

