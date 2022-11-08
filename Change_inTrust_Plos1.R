
# Required packages

library(RColorBrewer)
library(tidyr)
library(plyr)
library(dplyr)
library(FSA)
library(ppcor)
library(Hmisc)
library(vioplot)
library(dunn.test)
library(foreign)
library(multcomp)
library(tidyr)


surveydata <- read.csv("survey_data_recoded_dig_pol.csv", header=TRUE)

pol <- as.numeric(as.character(surveydata$politics))

surveydata$Sex_recoded <- as.numeric(as.character(revalue(surveydata$RS_Sex, c("Female"=1, "Male"=0, "Identify in another way"= NA, "NA" = NA))))

# Section 3.1

# Section 3.1.1

# Trust in scientists post-pandemic

PandTrustSci <- unlist(surveydata["PandTrustSci"])

PandTrustSci.freq <- table(PandTrustSci)
names(PandTrustSci.freq) = c("Trust them much less", "Trust them a little less", "About the same", "Trust them a little more", "Trust them much more")

Df_1.1 <- data.frame(PandTrustSci.freq)
Df_1.1 <- na.omit(Df_1.1)
Df_1.1$PandTrustSci.Perc <- round(Df_1.1[,"Freq"]/sum(Df_1.1["Freq"])*100)


pie.labels <- Df_1.1[,"PandTrustSci.Perc"]
pie.labels <- paste(pie.labels, "%", sep="")


pdf("Fig1_PandTrustSci.pdf")

pie(PandTrustSci.freq, labels=pie.labels, radius=0.7, col=brewer.pal(n=5, name="Blues"))
legend("topright", c("Trust them much less", "Trust them a little less", "About the same", "Trust them a little more", "Trust them much more"), cex=0.8, fill=brewer.pal(n=5, name="Blues"))

dev.off()


PandTrustSci.Neg <- sum(Df_1.1[1,"Freq"],Df_1.1[2,"Freq"])
PandTrustSci.Pos <- sum(Df_1.1[4,"Freq"],Df_1.1[5,"Freq"])

binom.test(c(PandTrustSci.Pos, PandTrustSci.Neg))



# Section 3.1.2

# Trust in geneticists post-pandemic

PandTrustGenet <- unlist(surveydata["PandTrustGenet"])

PandTrustGenet.freq <- table(PandTrustGenet)
names(PandTrustGenet.freq) = c("Trust them much less", "Trust them a little less", "About the same", "Trust them a little more", "Trust them much more")

Df_1.2A <- data.frame(PandTrustGenet.freq)
Df_1.2A <- na.omit(Df_1.2A)
Df_1.2A$PandTrustGenet.Perc <- round(Df_1.2A[,"Freq"]/sum(Df_1.2A["Freq"])*100)


pie.labels <- Df_1.2A[,"PandTrustGenet.Perc"]
pie.labels <- paste(pie.labels, "%", sep="")


pdf("Fig2_PandTrustGenetGeol.pdf", height=8, width=14)

par(mfrow=c(1,2),oma=c(5,0,0,0),xpd=NA)

pie(PandTrustGenet.freq, main="A", labels=pie.labels, radius=0.88, col=brewer.pal(n=5, name="Blues"))

#dev.off()


PandTrustGenet.Neg <- sum(Df_1.2A[1,"Freq"],Df_1.2A[2,"Freq"])
PandTrustGenet.Pos <- sum(Df_1.2A[4,"Freq"],Df_1.2A[5,"Freq"])

binom.test(c(PandTrustGenet.Pos, PandTrustGenet.Neg))



# Trust in geologists post-pandemic

PandTrustGeol <- unlist(surveydata["PandTrustGeol"])

PandTrustGeol.freq <- table(PandTrustGeol)
names(PandTrustGeol.freq) = c("Trust them much less", "Trust them a little less", "About the same", "Trust them a little more", "Trust them much more")

Df_1.2B <- data.frame(PandTrustGeol.freq)
Df_1.2B <- na.omit(Df_1.2B)
Df_1.2B$PandTrustGeol.Perc <- round(Df_1.2B[,"Freq"]/sum(Df_1.2B["Freq"])*100)


pie.labels <- Df_1.2B[,"PandTrustGeol.Perc"]
pie.labels <- paste(pie.labels, "%", sep="")


#pdf("Fig2B_PandTrustGeol.pdf")

pie(PandTrustGeol.freq, main="B", labels=pie.labels, radius=0.88, col=brewer.pal(n=5, name="Blues"))
legend(-3.3,-1,ncol=5, c("Trust them much less", "Trust them a little less", "About the same", "Trust them a little more", "Trust them much more"), cex=0.8, fill=brewer.pal(n=5, name="Blues"))

dev.off()


PandTrustGeol.Neg <- sum(Df_1.2B[1,"Freq"],Df_1.2B[2,"Freq"])
PandTrustGeol.Pos <- sum(Df_1.2B[4,"Freq"],Df_1.2B[5,"Freq"])

binom.test(c(PandTrustGeol.Pos, PandTrustGeol.Neg))



# Trust in scientists vs geneticists/geologists post-pandemic

kruskal.test(list(PandTrustSci, PandTrustGenet, PandTrustGeol))

Df_1.2C <- data.frame(PandTrustSci, PandTrustGenet, PandTrustGeol)

Df_1.2D <- gather(Df_1.2C, "Group", "Score", 1:3)
Group <- factor(unlist(Df_1.2D["Group"]))
Score <- unlist(Df_1.2D["Score"])

dunn.test(Df_1.2D$Score, Df_1.2D$Group, list=TRUE, table=FALSE)
dunn.test(Df_1.2D$Score, Df_1.2D$Group, method="bonferroni", list=TRUE, table=FALSE)



# Section 3.1.3

# Trust in pharmaceutical companies "e.g. Pfizer"

PandTrustPfiz <- unlist(surveydata["Pfiz"])

PandTrustPfiz.freq <- table(PandTrustPfiz)
names(PandTrustPfiz.freq) = c("Trust them much less", "Trust them a little less", "About the same", "Trust them a little more", "Trust them much more")

Df_1.3A <- data.frame(PandTrustPfiz.freq)
Df_1.3A <- na.omit(Df_1.3A)
Df_1.3A$PandTrustPfiz.Perc <- round(Df_1.3A[,"Freq"]/sum(Df_1.3A["Freq"])*100)


pie.labels <- Df_1.3A[,"PandTrustPfiz.Perc"]
pie.labels <- paste(pie.labels, "%", sep="")


pdf("Fig3_PandTrustPharma.pdf", height=8, width=14)
par(mfrow=c(1,2),oma=c(5,0,0,0),xpd=NA)

pie(PandTrustPfiz.freq, main="A", labels=pie.labels, radius=0.88, col=brewer.pal(n=5, name="Blues"))

#dev.off()


PandTrustPfiz.Neg <- sum(Df_1.3A[1,"Freq"],Df_1.3A[2,"Freq"])
PandTrustPfiz.Pos <- sum(Df_1.3A[4,"Freq"],Df_1.3A[5,"Freq"])

binom.test(c(PandTrustPfiz.Pos, PandTrustPfiz.Neg))



# Trust in pharmaceutical companies "e.g. GSK"

PandTrustGSK <- unlist(surveydata["Glaxo"])

PandTrustGSK.freq <- table(PandTrustGSK)
names(PandTrustGSK.freq) = c("Trust them much less", "Trust them a little less", "About the same", "Trust them a little more", "Trust them much more")

Df_1.3B <- data.frame(PandTrustGSK.freq)
Df_1.3B <- na.omit(Df_1.3B)
Df_1.3B$PandTrustGSK.Perc <- round(Df_1.3B[,"Freq"]/sum(Df_1.3B["Freq"])*100)


pie.labels <- Df_1.3B[,"PandTrustGSK.Perc"]
pie.labels <- paste(pie.labels, "%", sep="")


#pdf("Fig3B_PandTrustGSK.pdf")

pie(PandTrustGSK.freq, main="B", labels=pie.labels, radius=0.88, col=brewer.pal(n=5, name="Blues"))
legend(-3.3,-1,ncol=5, c("Trust them much less", "Trust them a little less", "About the same", "Trust them a little more", "Trust them much more"), cex=0.8, fill=brewer.pal(n=5, name="Blues"))

dev.off()


PandTrustGSK.Neg <- sum(Df_1.3B[1,"Freq"],Df_1.3B[2,"Freq"])
PandTrustGSK.Pos <- sum(Df_1.3B[4,"Freq"],Df_1.3B[5,"Freq"])

binom.test(c(PandTrustGSK.Pos, PandTrustGSK.Neg))



# Trust in "e.g.Pfizer" vs "e.g.GSK"

wilcox.test(PandTrustPfiz, PandTrustGSK, mu=0, alt="two.sided", paired=FALSE)
wilcox.test(PandTrustPfiz, PandTrustGSK, mu=0, alt="two.sided", paired=FALSE, correct=FALSE)


#P <- as.numeric(unlist(Df_1.3C["Pfiz.Freq"]))
#G <- as.numeric(unlist(Df_1.3C["GSK.Freq"]))

P=as.numeric(unlist(PandTrustPfiz.freq))
G=as.numeric(unlist(PandTrustGSK.freq))

PandTrustPharma <- matrix(c(P, G), ncol=2, byrow=FALSE)
colnames(PandTrustPharma) <- c("Pfizer", "GSK")
rownames(PandTrustPharma) <- c("Trust them much less", "Trust them a little less", "About the same", "Trust them a little more", "Trust them much more")

summary(PandTrustPfiz)
summary(PandTrustGSK)

PandTrustPharma_chisq <- chisq.test(PandTrustPharma)
PandTrustPharma_chisq
PandTrustPharma_chisq$observed
PandTrustPharma.exp <- round(PandTrustPharma_chisq$expected)

c1 <- chisq.test(PandTrustPharma[1,], p=PandTrustPharma.exp[1,], rescale.p=TRUE)

c2 <- chisq.test(PandTrustPharma[2,], p=PandTrustPharma.exp[2,], rescale.p=TRUE)

c3 <- chisq.test(PandTrustPharma[3,], p=PandTrustPharma.exp[3,], rescale.p=TRUE)

c4 <- chisq.test(PandTrustPharma[4,], p=PandTrustPharma.exp[4,], rescale.p=TRUE)

c5 <- chisq.test(PandTrustPharma[5,], p=PandTrustPharma.exp[5,], rescale.p=TRUE)




# Section 3.2

PreTrust <- unlist(surveydata["BeforePand_2"])
ChangeTrust <- unlist(surveydata["ViewChange2"])

Df_2.1 <- data.frame(PreTrust, ChangeTrust, PandTrustSci)
Df_2.1clean <- na.omit(Df_2.1)

PreTrust.clean <- unlist(Df_2.1clean["PreTrust"])
ChangeTrust.clean <- unlist(Df_2.1clean["ChangeTrust"])

Df_2.1clean$PostTrust <- Df_2.1clean[,"PreTrust"]+Df_2.1clean[,"ChangeTrust"]
PostTrust <- unlist(Df_2.1clean["PostTrust"])


# Section 3.2.1

# Variances randomisation test for polarisation

Vo <- var(PostTrust)

RandomVars <- c()

nrands <- 100

for (i in c(1:nrands)) {
	
  	R.ChangeTrust <- sample(ChangeTrust.clean)
  	R.PostTrust <- PreTrust.clean+R.ChangeTrust
  	R.PostTrustVar <- var(R.PostTrust)
  	RandomVars <- c(RandomVars, R.PostTrustVar)

}

Vars.greater <- RandomVars[RandomVars>=Vo]
N <- length(Vars.greater)
Pr <- N/nrands



# Section 3.2.2

# VioPlot pre-trust vs change (ViewChange2)

PreTrust.neg <- PreTrust.clean[ChangeTrust.clean==-1]
PreTrust.neut <- PreTrust.clean[ChangeTrust.clean==0]
PreTrust.pos <- PreTrust.clean[ChangeTrust.clean==+1]

MeanPreTrust <- c(mean(PreTrust.neg), mean(PreTrust.neut), mean(PreTrust.pos))
ViewChange <- c(-1, 0, 1)


pdf("Fig4_PreVsChange.pdf")

vioplot(PreTrust.neg, PreTrust.neut, PreTrust.pos, names= c("-1", "0", "+1"), col="skyblue3", xlab="Change in trust", ylab ="Pre-pandemic trust", cex.axis=1.20)

dev.off()



# Correlation and partial correlation 

Age <- unlist(surveydata["Age"])
Sex <- unlist(surveydata["Sex_recoded"])
Education <- unlist(surveydata["education_level"])
Religion <- unlist(surveydata["religion"])


Df_2.2 <- data.frame(PreTrust=PreTrust, ChangeTrust = ChangeTrust, Age=Age, Sex=Sex, Education = Education, Religion= Religion, Politics=pol)

Df_2.2clean <- na.omit(Df_2.2)

PreTrust.clean <- unlist(Df_2.2clean["PreTrust"])
ChangeTrust.clean <- unlist(Df_2.2clean["ChangeTrust"])
Age.clean <- unlist(Df_2.2clean["Age"])
Sex.clean <- unlist(Df_2.2clean["Sex"])
Edu.clean <- unlist(Df_2.2clean["Education"])
Rel.clean <- unlist(Df_2.2clean["Religion"])
Pol.clean <- unlist(Df_2.2clean["Politics"])


Table1.top <- rcorr(as.matrix(Df_2.2clean), type="spearman")

Table1.bot <- pcor(Df_2.2clean, method="spearman")



 

# Section 3.3

# Section 3.3.1

Vaccine <- unlist(surveydata["CovVac_recoded"])
ChangeTrust <- unlist(surveydata["ViewChange2"])

table(Vaccine)


# Pie-charts

VaccineNo.freq <- table(ChangeTrust[Vaccine==0])

Df_3.1A <- data.frame(VaccineNo.freq)
Df_3.1A <- na.omit(Df_3.1A)
Df_3.1A$VaccineNo.perc <- round(Df_3.1A[,"Freq"]/sum(Df_3.1A["Freq"])*100)

pie.labels <- Df_3.1A[,"VaccineNo.perc"]
pie.labels <- paste(pie.labels, "%", sep="")


pdf("Fig5_Vaccine.pdf", height=8, width=14)
par(mfrow=c(1,2),oma=c(5,0,0,0),xpd=NA)

pie(VaccineNo.freq, main="A", labels=pie.labels, radius=0.88, col=brewer.pal(n=3, name="Blues"))

#dev.off()



VaccineYes.freq <- table(ChangeTrust[Vaccine==1])

Df_3.1B <- data.frame(VaccineYes.freq)
Df_3.1B <- na.omit(Df_3.1B)
Df_3.1B$VaccineYes.perc <- round(Df_3.1B[,"Freq"]/sum(Df_3.1B["Freq"])*100)

pie.labels <- Df_3.1B[,"VaccineYes.perc"]
pie.labels <- paste(pie.labels, "%", sep="")


#pdf("Fig5B_VaccineYes.pdf")



pie(VaccineYes.freq, main="B", labels=pie.labels, radius=0.88, col=brewer.pal(n=3, name="Blues"))

legend(-2.4,-0.9,ncol=3, c("Decrease in trust", "No change in trust", "Increase in trust"), cex=0.8, fill=brewer.pal(n=5, name="Blues"))

dev.off()


# Mann-Whitney U

VaccineNo <- ChangeTrust[Vaccine==0]
VaccineYes <- ChangeTrust[Vaccine==1]

wilcox.test(VaccineNo, VaccineYes, mu=0, alt="two.sided", paired=FALSE)
wilcox.test(VaccineNo, VaccineYes, mu=0, alt="two.sided", paired=FALSE, correct=FALSE)


# Chi-squared

#col and row names swapped

VaccineFreq <- matrix(c(VaccineNo.freq, VaccineYes.freq), ncol=3, byrow=TRUE)
rownames(VaccineFreq) <- c("VaccineNo", "VaccineYes")
colnames(VaccineFreq) <- c("-1", "0", "+1")

VaccineFreq_chisq <- chisq.test(VaccineFreq)
VaccineFreq_chisq
VaccineFreq_chisq$observed
VaccineFreq_exp <- round(VaccineFreq_chisq$expected)

c1 <- chisq.test(VaccineFreq[,1], p=VaccineFreq_exp[,1], rescale.p=TRUE)
c2 <- chisq.test(VaccineFreq[,2], p=VaccineFreq_exp[,2], rescale.p=TRUE)
c3 <- chisq.test(VaccineFreq[,3], p=VaccineFreq_exp[,3], rescale.p=TRUE)


# Correlation and partial correlation

Covid <- unlist(surveydata["HadCov_recoded"])


Df_3.1B <- data.frame(Vaccine = Vaccine,  ChangeTrust = ChangeTrust, Covid=Covid, Age=Age, Sex=Sex, Education = Education, Religion= Religion, Politics=pol)
Df_3.1Bclean <- na.omit(Df_3.1B)

Vaccine.clean <- unlist(Df_3.1Bclean["Vaccine"])
ChangeTrust.clean <- unlist(Df_3.1Bclean["ChangeTrust"])
Covid.clean <- unlist(Df_3.1Bclean["Covid"])
Age.clean <- unlist(Df_3.1Bclean["Age"])
Sex.clean <- unlist(Df_3.1Bclean["Sex"])
Edu.clean <- unlist(Df_3.1Bclean["Education"])
Rel.clean <- unlist(Df_3.1Bclean["Religion"])
Pol.clean <- unlist(Df_3.1Bclean["Politics"])

Table2.top <- rcorr(as.matrix(Df_3.1Bclean), type="spearman")

Table2.bot <- pcor(Df_3.1Bclean, method="spearman")


#politics by vote
df.pol <- data.frame(pol =pol, vote = surveydata$RS_VoteChoice19)

new_order <- with(df.pol, reorder(vote, pol, median, na.rm=T))

pdf("S1 Fig.pdf", height = 8, width =40)
vioplot(pol~new_order, xlab="Vote 2019", ylab = "Right wing degree")
dev.off()
















 
 
 

 
 

 
 
 
 
 










