dat1<-read.csv("all4.csv",row.names = 1,header=T)
allnet<-dat1[1:20,1:51]
networklevel(allnet)
specieslevel(allnet,level="lower")
specieslevel(allnet,level="higher")
plotweb(allnet,text.rot = 90)
avonet<-read.csv("avonet.csv",row.names = 1,header=TRUE)
dairynet<-read.csv("dairy4.csv",row.names = 1,header=TRUE)
potatonet<-read.csv("potatonet.csv",row.names = 1,header=TRUE)
forestnet<-read.csv("forestnet.csv",row.names = 1,header=TRUE)
networklevel(avonet) #repeat for each network

#modularity
allmod<-computeModules(allnet)
plotModuleWeb(allmod)
czvalues(allmod,weighted=TRUE,level="lower")
czvalues(allmod,weighted=TRUE,level="higher")
allmod@likelihood

#null models as per Dormann & Strauss 2014
nullnet <- nullmodel(allnet, N=100, method=3)
modules.nulls <- sapply(nullnet, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (allmod@likelihood - mean(like.nulls))/sd(like.nulls))

#null models to calculate critical levels for weighted c/z values (thanks Jamie Stavert! @jstavert)
nullnet <- nullmodel(allnet, N=100, method=3) #as above
modules.nulls <- sapply(nullnet, computeModules) #as above
cz.nulls <- sapply(modules.nulls, czvalues, weighted=TRUE,level="lower")
c.nulls <- as.data.frame(unlist(cz.nulls[1,]))
colnames(c.nulls)[1] <- "cval"
c.crit <- quantile(c.nulls$cval,probs=c(0.975))
z.nulls <- as.data.frame(unlist(cz.nulls[2,]))
colnames(z.nulls)[1] <- "zval"
z.nulls <- na.omit(z.nulls)
z.crit <- quantile(z.nulls$zval,probs=c(0.975))
# repeat for upper level 

#test for effect of taxa visits on plant reproduction
visfert1<-glm(fert~Syrphidae,weights = 1/Fertvar, data=sitedata,family = quasibinomial())
visseed1<-glm(seeds~Syrphidae, weights=1/Seedvar,data=sitedata,family=quasipoisson())
visfert2<-glm(fert~OtherDiptera,weights = 1/Fertvar, data=sitedata,family = quasibinomial())
visseed2<-glm(seeds~OtherDiptera,weights = 1/Seedvar, data=sitedata,family=quasipoisson())
visfert3<-glm(fert~Hymenoptera,weights = 1/Fertvar, data=sitedata,family = quasibinomial())
visseed3<-glm(seeds~Hymenoptera,weights=1/Seedvar,data=sitedata,family=quasipoisson())
visfert4<-glm(fert~Lepidoptera,weights = 1/Fertvar, data=sitedata,family = quasibinomial())
visseed4<-glm(seeds~Lepidoptera,weights=1/Seedvar, data=sitedata,family=quasipoisson())
visfert5<-glm(fert~Coleoptera,weights=1/Fertvar, data=sitedata,family = quasibinomial())
visseed5<-glm(seeds~Coleoptera,weights=1/Seedvar, data=sitedata,family=quasipoisson())

# test for relationship between plant reproduction and network metrics
fert1<-glm(fert~cvalue,weights=1/Fertvar,data=sitedata,family = quasibinomial())
fert2<-glm(fert~d,weights=1/Fertvar,data=sitedata,family = quasibinomial())
seed1<-glm(seeds~cvalue,weights=1/Seedvar,data=sitedata,family = quasipoisson())
seed2<-glm(seeds~d,weights=1/Seedvar,data=sitedata,family = quasipoisson())


#test for pairwise site relationships between geographical distance and dissimilarity in network metric
dis1<-lm(cvalue~distance,data=distances)
dis2<-lm(d~distance,data=distances)


#coefplot package, create multiplots for model coefficients
multiplot(fert1,fert2, title = "Fertilised pods",intercept = FALSE,legend.position = "none",xlab="Coefficient",ylab="Predictor")
multiplot(seed1,seed2,title = "Number of seeds per pod",intercept = FALSE,legend.position = "none",xlab="Coefficient",ylab="Predictor")
multiplot(visfert1,visfert2,visfert3,visfert4,visfert5,title="Fertilised pods",intercept=FALSE,legend.position = "none")
multiplot(visseed1,visseed2,visseed3,visseed4,visseed5,title="Seed set",intercept=FALSE,legend.position = "none")

