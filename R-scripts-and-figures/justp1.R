setwd("/home/ross/workspace/mygithub/phylofigure-captions/caption-per-file")
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("wordcloud")
library(tm)
library(foreign)
library(SnowballC)
library(wordcloud)

#guide http://onepager.togaware.com/TextMiningO.pdf
cname3 <- file.path(".", "3923-yes-pone")
length(dir(cname3))
dir(cname3)
ponepos <- Corpus(DirSource(cname3))
ponepos

inspect(ponepos[3])

#Cleaning data
ponepos <- tm_map(ponepos, content_transformer(tolower))
ponepos <- tm_map(ponepos, removeNumbers)
#DOESNT WORK WELL ponepos <- tm_map(ponepos, removePunctuation)


#lemmatization
toString <- content_transformer(function(x, from, to) gsub(from, to, x))
ponepos <- tm_map(ponepos, toString, "[[:punct:]]", " ")
ponepos <- tm_map(ponepos, toString, "maximum likelihood", "MAXIMUMLIKELIHOOD")
ponepos <- tm_map(ponepos, toString, " ml ", " MAXIMUMLIKELIHOOD ")
ponepos <- tm_map(ponepos, toString,  " acids " ,  " acid " )
ponepos <- tm_map(ponepos, toString,  " alignments " ,  " alignment " )
ponepos <- tm_map(ponepos, toString,  " analyses " ,  " analysis " )
ponepos <- tm_map(ponepos, toString,  " arrows " ,  " arrow " )
ponepos <- tm_map(ponepos, toString,  " asterisks " ,  " asterisk " )
ponepos <- tm_map(ponepos, toString,  " bacterial " ,  " bacteria " )
ponepos <- tm_map(ponepos, toString,  " bootstrapped " ,  " bootstrap " )
ponepos <- tm_map(ponepos, toString,  " bootstraps " ,  " bootstrap " )
ponepos <- tm_map(ponepos, toString,  " bootstrapping " ,  " bootstrap " )
ponepos <- tm_map(ponepos, toString,  " branches " ,  " branch " )
ponepos <- tm_map(ponepos, toString,  " changes " ,  " change " )
ponepos <- tm_map(ponepos, toString,  " characters " ,  " character " )
ponepos <- tm_map(ponepos, toString,  " circles " ,  " circle " )
ponepos <- tm_map(ponepos, toString,  " clades " ,  " clade " )
ponepos <- tm_map(ponepos, toString,  " clones " ,  " clone " )
ponepos <- tm_map(ponepos, toString,  " clustalw " ,  " clustal " )
ponepos <- tm_map(ponepos, toString,  " comparisons " ,  " comparison " )
ponepos <- tm_map(ponepos, toString,  " containing " ,  " contain " )
ponepos <- tm_map(ponepos, toString,  " corresponding " ,  " correspond " )
ponepos <- tm_map(ponepos, toString,  " distances " ,  " distance " )
ponepos <- tm_map(ponepos, toString,  " domains " ,  " domain " )
ponepos <- tm_map(ponepos, toString,  " estimates " ,  " estimated " )
ponepos <- tm_map(ponepos, toString,  " evolutionary " ,  " evolution " )
ponepos <- tm_map(ponepos, toString,  " genomic " ,  " genome " )
ponepos <- tm_map(ponepos, toString,  " genomes " ,  " genome " )
ponepos <- tm_map(ponepos, toString,  " genotypes " ,  " genotype " )
ponepos <- tm_map(ponepos, toString,  " geographical " ,  " geographic " )
ponepos <- tm_map(ponepos, toString,  " haplotypes " ,  " haplotype " )
ponepos <- tm_map(ponepos, toString,  " labeled " ,  " labelled " )
ponepos <- tm_map(ponepos, toString,  " lengths " ,  " length " )
ponepos <- tm_map(ponepos, toString,  " letters " ,  " letter " )
ponepos <- tm_map(ponepos, toString,  " levels " ,  " level " )
ponepos <- tm_map(ponepos, toString,  " locations " ,  " location " )
ponepos <- tm_map(ponepos, toString,  " motifs " ,  " motif " )
ponepos <- tm_map(ponepos, toString,  " models " ,  " model " )
ponepos <- tm_map(ponepos, toString,  " mutations " ,  " mutation " )
ponepos <- tm_map(ponepos, toString,  " neighbourjoining " ,  " neighborjoining " )
ponepos <- tm_map(ponepos, toString,  " nucleotides " ,  " nucleotide " )
ponepos <- tm_map(ponepos, toString,  " numbers " ,  " number " )
ponepos <- tm_map(ponepos, toString,  " orders " ,  " order " )
ponepos <- tm_map(ponepos, toString,  " outgroups " ,  " outgroup " )
ponepos <- tm_map(ponepos, toString,  " parameters " ,  " parameter " )
ponepos <- tm_map(ponepos, toString,  " patterns " ,  " pattern " )
ponepos <- tm_map(ponepos, toString,  " percentages " ,  " percentage " )
ponepos <- tm_map(ponepos, toString,  " phylogenetically " ,  " phylogenetic " )
ponepos <- tm_map(ponepos, toString,  " phylogenic " ,  " phylogenetic " )
ponepos <- tm_map(ponepos, toString,  " phylogenies " ,  " phylogeny " )
ponepos <- tm_map(ponepos, toString,  " phylograms " ,  " phylogeny " )
ponepos <- tm_map(ponepos, toString,  " phylogram " ,  " phylogeny " )
ponepos <- tm_map(ponepos, toString,  " plants " ,  " plant " )
ponepos <- tm_map(ponepos, toString,  " points " ,  " point " )
ponepos <- tm_map(ponepos, toString,  " populations " ,  " population " )
ponepos <- tm_map(ponepos, toString,  " positions " ,  " position " )
ponepos <- tm_map(ponepos, toString,  " probabilities " ,  " probability " )
ponepos <- tm_map(ponepos, toString,  " proteins " ,  " protein " )
ponepos <- tm_map(ponepos, toString,  " rates " ,  " rate " )
ponepos <- tm_map(ponepos, toString,  " regions " ,  " region " )
ponepos <- tm_map(ponepos, toString,  " relationships " ,  " relationship " )
ponepos <- tm_map(ponepos, toString,  " related " ,  " relationship " )
ponepos <- tm_map(ponepos, toString,  " replicates " ,  " replicate " )
ponepos <- tm_map(ponepos, toString,  " replications " ,  " replicate " )
ponepos <- tm_map(ponepos, toString,  " rooted " ,  " root " )
ponepos <- tm_map(ponepos, toString,  " samples " ,  " sample " )
ponepos <- tm_map(ponepos, toString,  " scaled " ,  " scale " )
ponepos <- tm_map(ponepos, toString,  " sequences " ,  " sequence " )
ponepos <- tm_map(ponepos, toString,  " sequenced " ,  " sequence " )
ponepos <- tm_map(ponepos, toString,  " sites " ,  " site " )
ponepos <- tm_map(ponepos, toString,  " states " ,  " state " )
ponepos <- tm_map(ponepos, toString,  " structures " ,  " structure " )
ponepos <- tm_map(ponepos, toString,  " substitutions " ,  " substitution " )
ponepos <- tm_map(ponepos, toString,  " subtypes " ,  " subtype " )
ponepos <- tm_map(ponepos, toString,  " supports " ,  " support " )
ponepos <- tm_map(ponepos, toString,  " taxa " ,  " taxon " )
ponepos <- tm_map(ponepos, toString,  " topologies " ,  " topology " )
ponepos <- tm_map(ponepos, toString,  " trees " ,  " tree " )
ponepos <- tm_map(ponepos, toString,  " types " ,  " type " )
ponepos <- tm_map(ponepos, toString,  " values " ,  " value " )
ponepos <- tm_map(ponepos, toString,  " variants " ,  " variant " )
ponepos <- tm_map(ponepos, toString,  " vertebrates " ,  " vertebrate " )
ponepos <- tm_map(ponepos, toString,  " viruses " ,  " virus " )


inspect(ponepos[3])

inspect(ponepos[16])
shtop <- stopwords("english")
customshtop <- c("according","across","additional","all","along","also",
                 "associated","available","based","belonging","black","blue","red",
                 "green","yellow","brown","can","clarity","closely","common","complete",
                 "conducted","details","detailed","denote","denoted",
                 "differences","different","displayed","dots","either",
                 "each","figure","followed","following","follows","for","greater",
                 "grey","high","highlighted","iii","less","left","right",
                 "orange","pink","see","set","small","table","there","these","this",
                 "text","used","without")
#Remove custom stopwords
ponepos <- tm_map(ponepos, removeWords, c(shtop,customshtop))
ponepos <- tm_map(ponepos, stripWhitespace)
inspect(ponepos[16])

#Stemming with Snowball C
#ponepos <- tm_map(ponepos, stemDocument)
#inspect(ponepos[16])

dtmponepos <- DocumentTermMatrix(ponepos) #slowstep
dtm2 <- removeSparseTerms(dtmponepos, sparse=0.99)
str(dtm2)

tfidf <- weightTfIdf(dtm2, normalize = TRUE)
tfidf

freq <- colSums(as.matrix(tfidf))
length(freq)
ord <- order(freq)
# Most frequent terms
freq[tail(ord)]

m <- as.matrix(tfidf)
str(tfidf)

mdtm2 <- as.matrix(dtm2)
write.csv(cbind(colSums(m),colSums(mdtm2)), file="wordfreq-p1-custom-wostem.csv")

wordFreq <- sort(colSums(m), decreasing=TRUE)
write.csv(colSums(m), file="wordFreq-tfidf-p1-custom-wostem.csv")
#Remove 'FIGURE'
wordFreq <- wordFreq[-1]
set.seed(375) # to make it reproducible
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
png("wordcloud_tfidf_ponepos.png", width=12,height=8, units='in', res=300)
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=5, max.words=500 , random.order=F, colors=grayLevels)
dev.off()
warnings()

#Lemmatisation (or lemmatization) in linguistics 
#is the process of grouping together the different 
#inflected forms of a word so they can be analysed 
#as a single item