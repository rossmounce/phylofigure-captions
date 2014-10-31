setwd("/home/ross/workspace/mygithub/processing-zootaxa/figure-caption-work")
library(tm)
library(foreign)
#install.packages("SnowballC")
library(SnowballC)
library(wordcloud)

#guide http://onepager.togaware.com/TextMiningO.pdf
cname1 <- file.path(".", "393-yes-zootaxa")
length(dir(cname1))
dir(cname1)
ztpos <- Corpus(DirSource(cname1))
ztpos
cname2 <- file.path(".", "16739-no-zootaxa")
length(dir(cname2))
dir(cname2)
ztneg <- Corpus(DirSource(cname2))
ztneg
cname3 <- file.path(".", "3923-yes-pone")
length(dir(cname3))
dir(cname3)
ponepos <- Corpus(DirSource(cname3))
ponepos

inspect(ztpos[16])

#Corpus names: 
# ztpos = Zootaxa phylogeny figure captions
# ztneg = Zootaxa figure captions NOT associated with phylogeny figure image
# ponepos = PLOS ONE phylogeny figure captions

# replace / @ | with a space
#ztpos <- tm_map(ztpos, toSpace, "/|@|nn|")
#ztneg <- tm_map(ztneg, toSpace, "/|@|nn|")
#ponepos <- tm_map(ponepos, toSpace, "/|@|nn|")

#Cleaning data
ztpos <- tm_map(ztpos, removeNumbers)
ztneg <- tm_map(ztneg, removeNumbers)
ponepos <- tm_map(ponepos, removeNumbers)
ztpos <- tm_map(ztpos, removePunctuation) 
ztneg <- tm_map(ztneg, removePunctuation) 
ponepos <- tm_map(ponepos, removePunctuation)
ztpos <- tm_map(ztpos, removeWords, stopwords("english"))
ztneg <- tm_map(ztneg, removeWords, stopwords("english"))
ponepos <- tm_map(ponepos, removeWords, stopwords("english"))
ztpos <- tm_map(ztpos, stripWhitespace)
ztneg <- tm_map(ztneg, stripWhitespace)
ponepos <- tm_map(ponepos, stripWhitespace)

inspect(ztpos[16])
ztpos

#Stemming with Snowball C
ztpos <- tm_map(ztpos, stemDocument)
ztneg <- tm_map(ztneg, stemDocument)
ponepos <- tm_map(ponepos, stemDocument)

inspect(ztpos[16])

dtmztpos <- DocumentTermMatrix(ztpos) #slowstep
dtmztneg <- DocumentTermMatrix(ztneg) #slowstep
dtmponepos <- DocumentTermMatrix(ponepos) #slowstep
#dtm2 <- removeSparseTerms(dtm, sparse=0.95)
str(dtmztpos)
dtmztpos$Terms


freq <- colSums(as.matrix(dtmztpos))
length(freq)
ord <- order(freq)
# Most frequent terms
freq[tail(ord)]

m <- as.matrix(dtmztpos)
write.csv(m, file="dtmztpos.csv")

wordFreq <- sort(colSums(m), decreasing=TRUE)
#Remove 'FIGURE'
wordFreq <- wordFreq[-1]
set.seed(375) # to make it reproducible
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
png("wordcloud_zootaxa_phylo.png", width=12,height=8, units='in', res=300)
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=5, max.words=500 , random.order=F, colors=grayLevels)
dev.off()
warnings()



#dtm.mat <- as.matrix(dtm) 
#rownames(dtm.mat) <- paste0("row", seq(0,nrow(dtm.mat)-1))
#rownames(dtm.mat2) <- paste0("row", seq(0,nrow(dtm.mat2)-1))
#dtm.mat
#nnzero(dtm.mat) #gives number of non-zero cells in matrix
#meta(textcorp)
#dtm.mat2 <- as.matrix(dtm2)
#dtm.mat2
#str(dtm.mat2)
#termFrequency <- colSums(as.matrix(dtm.mat2))
#termFrequency
#length(termFrequency)
#sort(termFrequency)
#plot(sort(termFrequency, decreasing=TRUE))

#wordcloud
#m <- as.matrix(dtm.mat2)
#wordFreq <- sort(colSums(m), decreasing=TRUE)
#wordFreq
#set.seed(375) # to make it reproducible
#grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
#png("wordcloud_2006_papers.png", width=12,height=8, units='in', res=300)
#wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=200, random.order=F, colors=grayLevels)
#dev.off()
#warnings()


d.mrbay <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "mrbay") ])
d.paup <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "paup") ])
d.tnt <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "tnt") ])
d.winclada <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "winclada") ])
d.mega <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "mega") ])
d.phylip <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "phylip") ])
d.hennig <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "hennig") ])
d.phyml <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "phyml") ])
d.garl <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "garl") ])
d.raxml <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "raxml") ])
d.poi <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "poi") ]) #POY -> poi apparently!
d.nona <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "nona") ])
d.charact <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "charact") ])
d.matrix <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "matrix") ])
d.parsimonstem <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "parsimonstem") ])
d.tree <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "tree") ])
d.bremer <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "bremer") ])
d.support <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "support") ])
d.align <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "align") ])
d.bayesian <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "bayesian") ])
d.likelihood <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "likelihood") ])
d.neighbor <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "neighbor") ])
d.maximum <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "maximum") ])
d.cladiststem <-as.matrix(dtm[,which(dimnames(dtm)$Terms == "cladiststem") ])
d.phylogstem <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "phylogstem") ])
d.treebas <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "treebas") ])
d.drya <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "drya") ])
d.morphobank <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "morphobank") ])
d.zoobank <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "zoobank") ])
d.genbank <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "genbank") ])
d.root <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "root") ])
d.polymorph <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "polymorph") ])
d.multist <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "multist") ])
d.order <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "order") ])
d.unord <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "unord") ])
d.node <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "node") ])
d.consist <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "consist") ])
d.branch <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "branch") ])
d.bootstrap <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "bootstrap") ])
d.jackknif <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "jackknif") ])
d.tbr <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "tbr") ])
d.gtr <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "gtr") ])
d.hky <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "hky") ])
d.swofford <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "swofford") ])
d.goloboff <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "goloboff") ])
d.acctran <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "acctran") ])
d.deltran <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "deltran") ])
d.apomorph <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "apomorph") ])
d.plesiomorph <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "plesiomorph") ])
d.sister <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "sister") ]) noisy , not diagnostic
d.synapomorphi <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "synapomorphi") ])
d.outgroup <- as.matrix(dtm[,which(dimnames(dtm)$Terms == "outgroup") ])


fff <- c("phylogstem","cladiststem","mrbay","paup","tnt","winclada","mega","phylip","hennig","phyml","garl","raxml","poi","nona","charact","matrix","parsimonstem","tree","bremer","support","align","bayesian","likelihood","neighbor","maximum","treebas","drya","morphobank","zoobank","genbank","root","polymorph","multist","order","unord","node","consist","branch","bootstrap","jackknif","tbr","gtr","swofford","goloboff","acctran","deltran","plesiomorph","apomorph","synapomorphi","outgroup","plesiomorph","sister")
length(fff)
phylo_names <- cbind(d.phylogstem,d.cladiststem,d.mrbay,d.paup,d.tnt,d.winclada,d.mega,d.phylip,d.hennig,d.phyml,d.garl,d.raxml,d.poi,d.nona,d.charact,d.matrix,d.parsimonstem,d.tree,d.bremer,d.support,d.align,d.bayesian,d.likelihood,d.neighbor,d.maximum,d.treebas,d.drya,d.morphobank,d.zoobank,d.genbank,d.root,d.polymorph,d.multist,d.order,d.unord,d.node,d.consist,d.branch,d.bootstrap,d.jackknif,d.tbr,d.gtr,d.swofford,d.goloboff,d.acctran,d.deltran,d.plesiomorph,d.apomorph,d.synapomorphi,d.outgroup,d.plesiomorph,d.sister)
eee <- c("zt01104p021","zt01104p034","zt01104p045","zt01104p057","zt01104p068","zt01105p016","zt01105p025","zt01105p035","zt01105p048","zt01105p068","zt01106p024","zt01106p033","zt01106p043","zt01106p068","zt01107p047","zt01107p068","zt01108p021","zt01108p035","zt01108p051","zt01108p068","zt01109p014","zt01109p025","zt01109p037","zt01109p047","zt01109p055","zt01109p068","zt01110p015","zt01110p025","zt01110p037","zt01110p045","zt01110p057","zt01110p068","zt01111p019","zt01111p058","zt01111p067","zt01111p068","zt01112p064","zt01113p020","zt01113p032","zt01113p040","zt01113p049","zt01113p064","zt01114p052","zt01114p059","zt01114p068","zt01115p029","zt01115p059","zt01115p068","zt01116p027","zt01116p041","zt01116p054","zt01116p068","zt01117p019","zt01117p035","zt01117p068","zt01118p042","zt01118p056","zt01118p068","zt01119p027","zt01119p058","zt01119p068","zt01120p033","zt01120p039","zt01120p049","zt01120p055","zt01120p068","zt01121p052","zt01121p068","zt01122p023","zt01122p045","zt01122p055","zt01122p068","zt01123p019","zt01123p037","zt01123p055","zt01123p068","zt01124p040","zt01124p046","zt01124p053","zt01124p068","zt01125p037","zt01125p043","zt01125p056","zt01125p068","zt01126p019","zt01126p033","zt01126p051","zt01126p061","zt01126p068","zt01127p071","zt01128p033","zt01128p047","zt01128p056","zt01128p064","zt01129p022","zt01129p036","zt01129p045","zt01129p060","zt01129p068","zt01130p033","zt01130p041","zt01130p055","zt01130p068","zt01131p032","zt01131p048","zt01131p058","zt01131p068","zt01132p030","zt01132p049","zt01132p061","zt01132p068","zt01133p037","zt01133p043","zt01133p059","zt01133p068","zt01134p028","zt01134p049","zt01134p057","zt01134p068","zt01135p027","zt01135p048","zt01135p055","zt01135p068","zt01136p038","zt01136p048","zt01136p064","zt01137p036","zt01137p052","zt01137p061","zt01137p068","zt01138p044","zt01138p051","zt01138p068","zt01139p017","zt01139p026","zt01139p033","zt01139p062","zt01139p068","zt01140p029","zt01140p051","zt01140p068","zt01141p054","zt01141p061","zt01141p068","zt01142p033","zt01142p041","zt01142p050","zt01142p055","zt01142p068","zt01143p150","zt01144p094","zt01145p110","zt01146p152","zt01147p034","zt01147p059","zt01147p068","zt01148p025","zt01148p045","zt01148p068","zt01149p088","zt01150p017","zt01150p030","zt01150p042","zt01150p052","zt01150p068","zt01151p026","zt01151p039","zt01151p046","zt01151p053","zt01151p068","zt01152p043","zt01152p058","zt01152p068","zt01153p016","zt01153p026","zt01153p032","zt01153p042","zt01153p050","zt01153p061","zt01153p068","zt01154p026","zt01154p033","zt01154p039","zt01154p048","zt01154p068","zt01155p023","zt01155p033","zt01155p040","zt01155p050","zt01155p060","zt01155p068","zt01156p050","zt01156p064","zt01156p068","zt01157p074","zt01158p038","zt01158p053","zt01158p067","zt01159p068","zt01160p020","zt01160p028","zt01160p036","zt01160p047","zt01160p055","zt01160p068","zt01161p049","zt01161p064","zt01161p068","zt01162p018","zt01162p031","zt01162p043","zt01162p052","zt01162p064","zt01162p068","zt01163p047","zt01163p059","zt01163p068","zt01164p033","zt01164p050","zt01164p055","zt01164p061","zt01164p068","zt01165p031","zt01165p046","zt01165p055","zt01165p068","zt01166p020","zt01166p033","zt01166p048","zt01166p055","zt01166p068","zt01167p016","zt01167p030","zt01167p045","zt01167p060","zt01167p068","zt01168p020","zt01168p030","zt01168p041","zt01168p049","zt01168p058","zt01168p068","zt01169p032","zt01169p046","zt01169p059","zt01169p068","zt01170p026","zt01170p056","zt01170p068","zt01171p016","zt01171p029","zt01171p037","zt01171p045","zt01171p068","zt01172p019","zt01172p029","zt01172p041","zt01172p048","zt01172p067","zt01173p037","zt01173p056","zt01173p062","zt01173p068","zt01174p025","zt01174p039","zt01174p048","zt01174p062","zt01174p068","zt01175p029","zt01175p035","zt01175p042","zt01175p054","zt01175p068","zt01176p016","zt01176p025","zt01176p040","zt01176p052","zt01176p058","zt01176p068","zt01177p019")
ggg <- c("zt01177p026","zt01177p037","zt01177p049","zt01177p055","zt01177p068","zt01178p209","zt01179p096","zt01180p140","zt01181p102","zt01182p130","zt01183p026","zt01183p041","zt01183p056","zt01183p068","zt01184p027","zt01184p042","zt01184p055","zt01184p068","zt01185p019","zt01185p035","zt01185p051","zt01185p059","zt01185p068","zt01186p056","zt01186p068","zt01187p036","zt01187p046","zt01187p059","zt01187p068","zt01188p022","zt01188p036","zt01188p047","zt01188p054","zt01188p062","zt01188p068","zt01189p037","zt01189p053","zt01189p068","zt01190p050","zt01190p057","zt01190p068","zt01191p019","zt01191p034","zt01191p047","zt01191p059","zt01191p068","zt01192p076","zt01193p039","zt01193p047","zt01193p057","zt01193p068","zt01194p032","zt01194p047","zt01194p055","zt01194p068","zt01195p029","zt01195p037","zt01195p060","zt01195p068","zt01196p032","zt01196p061","zt01196p068","zt01197p037","zt01197p043","zt01197p054","zt01197p063","zt01198p020","zt01198p051","zt01198p068","zt01199p017","zt01199p047","zt01199p060","zt01199p068","zt01200p012","zt01200p027","zt01200p041","zt01200p060","zt01200p068","zt01201p045","zt01201p062","zt01201p068","zt01202p019","zt01202p031","zt01202p037","zt01202p052","zt01202p059","zt01202p068","zt01203p038","zt01203p056","zt01203p068","zt01204p030","zt01204p036","zt01204p040","zt01204p052","zt01204p059","zt01204p068","zt01205p030","zt01205p054","zt01205p068","zt01206p022","zt01206p046","zt01206p061","zt01206p068","zt01207p064","zt01208p024","zt01208p035","zt01208p056","zt01208p068","zt01209p047","zt01209p060","zt01209p068","zt01210p025","zt01210p038","zt01210p052","zt01210p067","zt01211p019","zt01211p034","zt01211p051","zt01211p068","zt01212p244","zt01213p162","zt01214p107","zt01215p096","zt01216p114","zt01217p076","zt01218p080","zt01219p045","zt01219p057","zt01219p068","zt01220p018","zt01220p034","zt01220p046","zt01220p053","zt01220p061","zt01220p068","zt01221p023","zt01221p028","zt01221p039","zt01221p062","zt01221p068","zt01222p052","zt01222p068","zt01223p018","zt01223p022","zt01223p046","zt01223p053","zt01223p064","zt01223p068","zt01224p022","zt01224p031","zt01224p044","zt01224p058","zt01224p068","zt01225p019","zt01225p030","zt01225p038","zt01225p056","zt01225p068","zt01226p050","zt01226p060","zt01226p068","zt01227p029","zt01227p055","zt01227p062","zt01227p068","zt01228p024","zt01228p034","zt01228p060","zt01228p068","zt01229p032","zt01229p040","zt01229p048","zt01229p057","zt01229p068","zt01230p054","zt01230p062","zt01230p068","zt01231p042","zt01231p051","zt01231p068","zt01232p057","zt01232p068","zt01233p052","zt01233p068","zt01234p063","zt01235p048","zt01235p061","zt01235p068","zt01236p022","zt01236p036","zt01236p052","zt01236p068","zt01237p018","zt01237p025","zt01237p044","zt01237p060","zt01237p068","zt01238p022","zt01238p033","zt01238p061","zt01238p068","zt01239p017","zt01239p034","zt01239p048","zt01239p068","zt01240p055","zt01240p068","zt01241p036","zt01241p049","zt01241p059","zt01241p068","zt01242p020","zt01242p036","zt01242p052","zt01242p068","zt01243p060","zt01244p032","zt01244p039","zt01244p055","zt01244p068","zt01245p051","zt01245p058","zt01245p068","zt01246p013","zt01246p054","zt01246p068","zt01247p012","zt01247p024","zt01247p042","zt01247p058","zt01247p068","zt01248p020","zt01248p026","zt01248p034","zt01248p044","zt01248p068","zt01249p022","zt01249p036","zt01249p045","zt01249p059","zt01249p068","zt01250p035","zt01250p051","zt01250p068","zt01251p070","zt01252p035","zt01252p047","zt01252p061","zt01252p068","zt01253p050","zt01253p059","zt01253p068","zt01254p028","zt01254p043","zt01254p068","zt01255p015","zt01255p028","zt01255p035","zt01255p055","zt01255p061","zt01255p068","zt01256p009","zt01256p019","zt01256p047","zt01256p057","zt01256p068","zt01257p025","zt01257p048","zt01257p055","zt01257p068","zt01258p015","zt01258p031","zt01258p045","zt01258p056","zt01258p068")
iii <- c("zt01259p023","zt01259p031","zt01259p038","zt01259p054","zt01259p059","zt01259p068","zt01260p025","zt01260p035","zt01260p046","zt01260p055","zt01260p066","zt01260p068","zt01261p090","zt01262p080","zt01263p120","zt01264p100","zt01265p080","zt01266p063","zt01267p047","zt01267p057","zt01267p068","zt01268p038","zt01268p057","zt01268p068","zt01269p029","zt01269p041","zt01269p049","zt01269p056","zt01269p068","zt01270p017","zt01270p033","zt01270p043","zt01270p056","zt01270p068","zt01271p027","zt01271p035","zt01271p056","zt01271p068","zt01272p044","zt01272p059","zt01272p068","zt01273p007","zt01273p054","zt01273p064","zt01274p068","zt01275p020","zt01275p029","zt01275p041","zt01275p060","zt01275p068","zt01276p031","zt01276p038","zt01276p045","zt01276p053","zt01276p068","zt01277p021","zt01277p027","zt01277p038","zt01277p063","zt01277p068","zt01278p047","zt01278p056","zt01278p068","zt01279p042","zt01279p051","zt01279p068","zt01280p068","zt01281p019","zt01281p039","zt01281p054","zt01281p067","zt01282p015","zt01282p028","zt01282p038","zt01282p048","zt01282p058","zt01282p068","zt01283p024","zt01283p036","zt01283p045","zt01283p061","zt01283p068","zt01284p027","zt01284p051","zt01284p060","zt01284p068","zt01285p019","zt01285p029","zt01285p050","zt01285p064","zt01285p068","zt01286p014","zt01286p021","zt01286p031","zt01286p041","zt01286p055","zt01286p068","zt01287p140","zt01288p241","zt01289p114","zt01290p138","zt01291p101","zt01292p040","zt01293p078","zt01294p027","zt01294p059","zt01294p068","zt01295p027","zt01295p039","zt01295p060","zt01295p068","zt01296p017","zt01296p028","zt01296p043","zt01296p053","zt01296p061","zt01296p068","zt01297p016","zt01297p022","zt01297p035","zt01297p045","zt01297p055","zt01297p068","zt01298p015","zt01298p027","zt01298p036","zt01298p047","zt01298p060","zt01298p068","zt01299p033","zt01299p043","zt01299p055","zt01299p068","zt01300p029","zt01300p035","zt01300p050","zt01300p068","zt01301p060","zt01302p020","zt01302p030","zt01302p042","zt01302p059","zt01302p068","zt01303p033","zt01303p043","zt01303p068","zt01304p020","zt01304p029","zt01304p048","zt01304p059","zt01304p068","zt01305p019","zt01305p032","zt01305p039","zt01305p050","zt01305p067","zt01306p023","zt01306p039","zt01306p050","zt01306p056","zt01306p067","zt01307p033","zt01307p039","zt01307p062","zt01307p068","zt01308p029","zt01308p047","zt01308p062","zt01308p068","zt01309p023","zt01309p035","zt01309p044","zt01309p054","zt01309p068","zt01310p035","zt01310p051","zt01310p068","zt01311p012","zt01311p050","zt01311p064","zt01312p024","zt01312p035","zt01312p047","zt01312p058","zt01312p068","zt01313p038","zt01313p055","zt01313p068","zt01314p030","zt01314p039","zt01314p051","zt01314p068","zt01315p056","zt01315p068","zt01316p031","zt01316p043","zt01316p056","zt01316p068","zt01317p019","zt01317p033","zt01317p040","zt01317p047","zt01317p056","zt01317p068","zt01318p040","zt01318p058","zt01318p068","zt01319p014","zt01319p028","zt01319p041","zt01319p058","zt01319p068","zt01320p014","zt01320p027","zt01320p041","zt01320p048","zt01320p055","zt01320p068","zt01321p108","zt01322p131","zt01323p118","zt01324p357","zt01325p054","zt01325p073","zt01325p098","zt01325p115","zt01325p145","zt01325p156","zt01325p190","zt01325p198","zt01325p210","zt01325p217","zt01325p233","zt01325p254","zt01325p266","zt01325p276","zt01325p311","zt01325p318","zt01325p326","zt01325p334","zt01325p345","zt01325p362","zt01325p368","zt01325p384","zt01326p016","zt01326p024","zt01326p036","zt01326p044","zt01326p053","zt01326p068","zt01327p021","zt01327p040","zt01327p062","zt01327p068","zt01328p026","zt01328p038","zt01328p050","zt01328p061","zt01328p068","zt01329p027","zt01329p037","zt01329p057","zt01329p068","zt01330p026","zt01330p042","zt01330p050","zt01330p058","zt01330p068","zt01331p030","zt01331p054","zt01331p068","zt01332p036","zt01332p050","zt01332p056","zt01332p068")
jjj <- c("zt01333p023","zt01333p054","zt01333p062","zt01333p067","zt01334p025","zt01334p043","zt01334p068","zt01335p050","zt01335p053","zt01335p068","zt01336p064","zt01337p037","zt01337p059","zt01337p068","zt01338p032","zt01338p047","zt01338p055","zt01338p068","zt01339p029","zt01339p049","zt01339p068","zt01340p044","zt01340p049","zt01340p055","zt01340p068","zt01341p027","zt01341p048","zt01341p066","zt01341p068","zt01343p042","zt01343p054","zt01343p068","zt01344p022","zt01344p031","zt01344p041","zt01344p058","zt01344p062","zt01344p068","zt01345p096","zt01346p180","zt01347p092","zt01348p082","zt01349p018","zt01349p036","zt01349p045","zt01349p051","zt01349p062","zt01349p068","zt01350p019","zt01350p031","zt01350p043","zt01350p053","zt01350p068","zt01351p013","zt01351p022","zt01351p034","zt01351p043","zt01351p052","zt01351p059","zt01351p068","zt01352p070","zt01353p037","zt01353p051","zt01353p061","zt01353p068","zt01354p030","zt01354p044","zt01354p056","zt01354p061","zt01354p068","zt01355p037","zt01355p047","zt01355p059","zt01355p068","zt01356p078","zt01357p019","zt01357p029","zt01357p043","zt01357p060","zt01357p068","zt01358p037","zt01358p048","zt01358p057","zt01358p066","zt01358p068","zt01359p030","zt01359p055","zt01359p066","zt01360p040","zt01360p050","zt01360p060","zt01360p068","zt01361p020","zt01361p031","zt01361p043","zt01361p051","zt01361p059","zt01361p068","zt01362p021","zt01362p042","zt01362p053","zt01362p068","zt01363p021","zt01363p038","zt01363p048","zt01363p068","zt01364p044","zt01364p049","zt01364p057","zt01364p064","zt01364p066","zt01364p068","zt01365p017","zt01365p035","zt01365p047","zt01365p059","zt01365p068","zt01366p044","zt01366p054","zt01366p059","zt01366p068","zt01367p035","zt01367p050","zt01367p062","zt01367p068","zt01368p018","zt01368p040","zt01368p048","zt01368p056","zt01368p068","zt01369p017","zt01369p033","zt01369p041","zt01369p053","zt01369p061","zt01369p068","zt01370p022","zt01370p037","zt01370p047","zt01370p057","zt01370p068","zt01371p021","zt01371p035","zt01371p043","zt01371p056","zt01371p064","zt01371p068","zt01372p016","zt01372p025","zt01372p033","zt01372p052","zt01372p068","zt01373p035","zt01373p052","zt01373p064","zt01373p068","zt01374p054","zt01374p059","zt01374p068","zt01375p029","zt01375p049","zt01375p057","zt01375p068","zt01376p035","zt01376p051","zt01376p067","zt01377p026","zt01377p031","zt01377p045","zt01377p060","zt01377p068","zt01378p017","zt01378p035","zt01378p048","zt01378p059","zt01378p068","zt01379p102","zt01380p090","zt01381p091","zt01382p074","zt01383p021","zt01383p043","zt01383p056","zt01383p068","zt01384p039","zt01384p058","zt01384p068","zt01385p030","zt01385p046","zt01385p052","zt01385p066","zt01385p068")

hhh <- c(eee,ggg,iii,jjj)
length(hhh)
rownames(phylo_names) <- hhh
str(phylo_names)
phylo_names
class(phylo_names)
write.csv(phylo_names, file="phylo2012.csv")