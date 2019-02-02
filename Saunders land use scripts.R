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
specieslevel(avonet) #repeat for each network

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

#test for differences between land use
abun1<-glm(Visits~Order,family=quasipoisson(),data=orders)
abun2<-glm(AllVisits~land,family=quasipoisson(),data=sitedata)
rich1<-glm(richness~land,family=quasipoisson(),data=sitedata)
fertland<-glm(fert~land,weights=1/Fertvar,data=sitedata,family=quasibinomial())
seedland<-glm(seeds~land,weights=1/Seedvar,data=sitedata,family=quasipoisson())

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
visfert6<-glm(fert~AllVisits,weights=1/Fertvar, data=sitedata,family = quasibinomial())
visseed6<-glm(seeds~AllVisits,weights=1/Seedvar, data=sitedata,family=quasipoisson())
visfert7<-glm(fert~richness,weights=1/Fertvar, data=sitedata,family = quasibinomial())
visseed7<-glm(seeds~richness,weights=1/Seedvar, data=sitedata,family=quasipoisson())
model.sel(visfert1,visfert2)

# test for relationship between plant reproduction and network metrics
fert1<-glm(fert~cvalue,weights=1/Fertvar,data=sitedata,family = quasibinomial())
fert2<-glm(fert~d,weights=1/Fertvar,data=sitedata,family = quasibinomial())
fert3<-glm(fert~zvalue,weights=1/Fertvar,data=sitedata,family = quasibinomial())
seed1<-glm(seeds~cvalue,weights=1/Seedvar,data=sitedata,family = quasipoisson())
seed2<-glm(seeds~d,weights=1/Seedvar,data=sitedata,family = quasipoisson())
seed3<-glm(seeds~zvalue,weights=1/Seedvar,data=sitedata,family = quasipoisson())

#test for differences in node metrics between land use types
cland<-lm(cvalue~land,data=sitedata)
dland<-lm(d~land,data=sitedata)
visd<-glmer(d~Land+(1|Taxa),family=binomial(), data=visitor_d)

#test for effect of pollinator visitation rates/richness on network metrics
c1<-glm(cvalue~AllVisits,data=sitedata)
c2<-glm(cvalue~richness,data=sitedata)
c3<-glm(cvalue~OtherDiptera,data=sitedata)
c4<-glm(cvalue~Hymenoptera,data=sitedata)
c5<-glm(cvalue~Lepidoptera,data=sitedata)
c6<-glm(cvalue~Coleoptera,data=sitedata)
c7<-glm(cvalue~Syrphidae,data=sitedata)
c8<-glm(cvalue~1,data=sitedata)
d1<-glm(d~AllVisits,data=sitedata)
d2<-glm(d~richness,data=sitedata)
d3<-glm(d~OtherDiptera,data=sitedata)
d4<-glm(d~Hymenoptera,data=sitedata)
d5<-glm(d~Lepidoptera,data=sitedata)
d6<-glm(d~Coleoptera,data=sitedata)
d7<-glm(d~Syrphidae,data=sitedata)
d8<-glm(d~1,data=sitedata)
model.sel(c1,c2,c3,c4,c5,c6,c7,c8)
model.sel(d1,d2,d3,d4,d5,d6,d7,d8)

#test for pairwise site relationships between geographical distance and dissimilarity in network metric
dis1<-lm(cvalue~distance,data=distances)
dis2<-lm(d~distance,data=distances)

#landscape factors effect on network metrics
landa<-glm(cvalue~same250,data=sitedata)
landb<-glm(cvalue~same100,data=sitedata)
landc<-glm(cvalue~rich250,data=sitedata)
landd<-glm(cvalue~rich100,data=sitedata)
lande<-glm(cvalue~1,data=sitedata)
landk<-glm(d~same250,data=sitedata)
landl<-glm(d~same100,data=sitedata)
landm<-glm(d~rich250,data=sitedata)
landn<-glm(d~rich100,data=sitedata)
lando<-glm(d~1,data=sitedata)
model.sel(landa,landb,landc,landd,lande)
model.sel(landk,landl,landm,landn,lando)


#coefplot package, create multiplots for model coefficients
multiplot(fert1,fert2, title = "Fertilised pods",intercept = FALSE,legend.position = "none",xlab="Coefficient",ylab="Predictor")
multiplot(seed1,seed2,title = "Number of seeds per pod",intercept = FALSE,legend.position = "none",xlab="Coefficient",ylab="Predictor")
multiplot(visfert1,visfert2,visfert3,visfert4,visfert5,title="Fertilised pods",intercept=FALSE,legend.position = "none")
multiplot(visseed1,visseed2,visseed3,visseed4,visseed5,title="Seed set",intercept=FALSE,legend.position = "none")

