all.data=read.csv(file="/home/roger/roger/2017/to_do/gerlind_early_inf/earlyinf_final data_2017-06.txt", header=T, sep="\t", fill=T)
library(lme4)

sel.data=all.data[, c("correct", "age_group", "cond", "saliency", "version", "trial_no", "first_version", "item", "id", "gender")]
sel.data=droplevels(as.data.frame(na.omit(sel.data)))
nrow(all.data)
nrow(sel.data)
sel.data$age_group=as.factor(sel.data$age_group)
source("/home/roger/r_functions/diagnostic_fcns.r")
xx.fe.re=fe.re.tab(fe.model="correct~age_group*cond*saliency*version+trial_no+first_version+gender", re="(1|item)+(1|id)", data=sel.data)
xx.fe.re$summary
str(xx.fe.re$data)
xx.fe.re$data$age_group.5=xx.fe.re$data$age_group.5-mean(xx.fe.re$data$age_group.5)
xx.fe.re$data$age_group.35=xx.fe.re$data$age_group.35-mean(xx.fe.re$data$age_group.35)
xx.fe.re$data$cond.control=xx.fe.re$data$cond.control-mean(xx.fe.re$data$cond.control)
xx.fe.re$data$cond.test=xx.fe.re$data$cond.test-mean(xx.fe.re$data$cond.test)
xx.fe.re$data$cond.underinfo=xx.fe.re$data$cond.underinfo-mean(xx.fe.re$data$cond.underinfo)
xx.fe.re$data$saliency.low=xx.fe.re$data$saliency.low-mean(xx.fe.re$data$saliency.low)
xx.fe.re$data$version.scalar=xx.fe.re$data$version.scalar-mean(xx.fe.re$data$version.scalar)
xx.fe.re$data$first_version.scalar=xx.fe.re$data$first_version.scalar-mean(xx.fe.re$data$first_version.scalar)
xx.fe.re$data$gender.m=xx.fe.re$data$gender.m-mean(xx.fe.re$data$gender.m)
xx.fe.re$data$z.trial_no=as.vector(scale(xx.fe.re$data$trial_no))
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10000))
load("/home/roger_mundry/mnt/roger_mundry/a_transfer/gerlind_early_inf/new/early_inf.RData")
library(lme4)
full=glmer(correct~age_group*cond*saliency*version+z.trial_no+first_version+gender+
	(1+age_group.5+age_group.35+cond.control+cond.test+cond.underinfo+saliency.low+z.trial_no+first_version.scalar+gender.m+
		age_group.5:cond.control+age_group.35:cond.control+age_group.5:cond.test+age_group.35:cond.test+age_group.5:cond.underinfo+age_group.35:cond.underinfo+
		age_group.5:saliency.low+age_group.35:saliency.low+
		cond.control:saliency.low+cond.test:saliency.low+cond.underinfo:saliency.low+
		age_group.5:cond.control:saliency.low+age_group.5:cond.test:saliency.low+age_group.5:cond.underinfo:saliency.low+
		age_group.35:cond.control:saliency.low+age_group.35:cond.test:saliency.low+age_group.35:cond.underinfo:saliency.low||item)+
	(1+cond.control+cond.test+cond.underinfo+version.scalar+z.trial_no+
		cond.control:version.scalar+cond.test:version.scalar+cond.underinfo:version.scalar||id),
	family=binomial, data=xx.fe.re$data)
save.image("/home/roger_mundry/mnt/roger_mundry/a_transfer/gerlind_early_inf/new/early_inf.RData")

to.change=aggregate(xx.fe.re$data$correct, xx.fe.re$data[, c("age_group", "cond", "saliency", "version")], mean)
to.change=subset(to.change, x==1 | x==0)
to.change=lapply(1:nrow(to.change), function(x){
	(1:nrow(xx.fe.re$data))[xx.fe.re$data$age_group==to.change$age_group[x] & xx.fe.re$data$cond==to.change$cond[x] & xx.fe.re$data$saliency==to.change$saliency[x] & xx.fe.re$data$version==to.change$version[x]]
})

yy=aggregate(new.resp, xx.fe.re$data[, c("age_group", "cond", "saliency", "version")], mean)
yy=subset(yy, x==1 | x==0)

new.resp=xx.fe.re$data$correct
new.resp[unlist(lapply(to.change, sample, size=1))]=0
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10000))
ifull=glmer(new.resp~age_group*cond*saliency*version+z.trial_no+first_version+gender+
	(1+age_group.5+age_group.35+cond.control+cond.test+cond.underinfo+saliency.low+z.trial_no+first_version.scalar+gender.m+
		age_group.5:cond.control+age_group.35:cond.control+age_group.5:cond.test+age_group.35:cond.test+age_group.5:cond.underinfo+age_group.35:cond.underinfo+
		age_group.5:saliency.low+age_group.35:saliency.low+
		cond.control:saliency.low+cond.test:saliency.low+cond.underinfo:saliency.low+
		age_group.5:cond.control:saliency.low+age_group.5:cond.test:saliency.low+age_group.5:cond.underinfo:saliency.low+
		age_group.35:cond.control:saliency.low+age_group.35:cond.test:saliency.low+age_group.35:cond.underinfo:saliency.low||item)+
	(1+cond.control+cond.test+cond.underinfo+version.scalar+z.trial_no+
		cond.control:version.scalar+cond.test:version.scalar+cond.underinfo:version.scalar||id),
	family=binomial, data=xx.fe.re$data, control=contr)
save.image("/home/roger_mundry/mnt/roger_mundry/a_transfer/gerlind_early_inf/new/early_inf.RData")
table(new.resp)
nrow(as.data.frame(summary(ifull)$varcor))+length(fixef(ifull))

ifull=glmer(new.resp~(age_group+cond+saliency+version)^2+z.trial_no+first_version+gender+
	(1+age_group.5+age_group.35+cond.control+cond.test+cond.underinfo+saliency.low+z.trial_no+first_version.scalar+gender.m+
		age_group.5:cond.control+age_group.35:cond.control+age_group.5:cond.test+age_group.35:cond.test+age_group.5:cond.underinfo+age_group.35:cond.underinfo+
		age_group.5:saliency.low+age_group.35:saliency.low+
		cond.control:saliency.low+cond.test:saliency.low+cond.underinfo:saliency.low||item)+
	(1+cond.control+cond.test+cond.underinfo+version.scalar+z.trial_no+
		cond.control:version.scalar+cond.test:version.scalar+cond.underinfo:version.scalar||id),
	family=binomial, data=xx.fe.re$data, control=contr)
table(new.resp)
nrow(as.data.frame(summary(ifull)$varcor))+length(fixef(ifull))


contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000))
ifull=glmer(new.resp~(age_group+cond+saliency+version)^2+z.trial_no+first_version+gender+
	(1+age_group.5+age_group.35+cond.control+cond.test+cond.underinfo+saliency.low+z.trial_no+first_version.scalar+gender.m||item)+
	(1+z.trial_no||id),#cond.control+cond.test+cond.underinfo+version.scalar+
	family=binomial, data=xx.fe.re$data, control=contr)
ifull.o=glmer(correct~(age_group+cond+saliency+version)^2+z.trial_no+first_version+gender+
	(1+age_group.5+age_group.35+cond.control+cond.test+cond.underinfo+saliency.low+z.trial_no+first_version.scalar+gender.m||item)+
	(1+z.trial_no||id),#cond.control+cond.test+cond.underinfo+version.scalar+
	family=binomial, data=xx.fe.re$data, control=contr)
save.image("/home/roger_mundry/mnt/roger_mundry/a_transfer/gerlind_early_inf/new/early_inf.RData")

tapply(new.resp, list(xx.fe.re$data$id, xx.fe.re$data$cond), mean)
##all models above are just too complex and couldn't be fitted with meaningful results

#this is what we actually did:
##okay, now many iterations:
source("/home/roger_mundry/mnt/institute/Statistics/R scripts/Roger/drop1_para.r")
source("/home/roger_mundry/mnt/institute/Statistics/R scripts/Roger/helpers.r")
save.image("/home/roger_mundry/mnt/roger_mundry/a_transfer/gerlind_early_inf/new/early_inf.RData")
load("z:/a_transfer/gerlind_early_inf/new/early_inf.RData")
library(lme4)
library(parallel)
cl <- makeCluster(getOption("cl.cores", detectCores()))
cl=cl[1:(length(cl)-1)]
unlist(parLapply(cl=cl, 1:length(cl), fun=function(x){
	library(lme4)
	return(invisible(""))
}))
test.data2=xx.fe.re$data
clusterExport(cl=cl, varlist=c("contr", "test.data2", "drop1p", "to.change", "lmer.warns"))
n.iter=100

all.res=parLapply(cl=cl, X=1:n.iter, fun=function(x){
	new.resp=test.data2$correct
	new.resp[unlist(lapply(to.change, sample, size=1))]=0
	ifull=glmer(new.resp~(age_group+cond+saliency+version)^2+trial_no+first_version+gender+
		(1+age_group.5+age_group.35+cond.control+cond.test+cond.underinfo+saliency.low+trial_no+first_version.scalar+gender.m||item)+
		(1+cond.control+cond.test+cond.underinfo+version.scalar+z.trial_no||id),
		family=binomial, data=test.data2, control=contr)
	inull=glmer(new.resp~trial_no+first_version+
		(1+age_group.5+age_group.35+cond.control+cond.test+cond.underinfo+saliency.low+trial_no+first_version.scalar+gender.m||item)+
		(1+cond.control+cond.test+cond.underinfo+version.scalar+z.trial_no||id),
		family=binomial, data=test.data2, control=contr)
	tests=drop1p(model.res=ifull, para=F, data=NULL, contr=contr, n.cores=c("all-1", "all"), to.del=NULL, return.model.results=F)
	fn=as.data.frame(anova(inull, ifull, test="Chisq"))
	fe.ests=summary(ifull)$coefficients
	re.ests=as.data.frame(summary(ifull)$varcor)
	tests=tests$drop1.res
	full.warns=lmer.warns(ifull)
	null.warns=lmer.warns(inull)
	save(file=paste(c("D:/gerlind_models_full/ws_", x, ".RData"), collapse=""), list=c("fe.ests", "re.ests", "fn", "tests", "full.warns", "null.warns"))
	return(list(fe.ests=fe.ests, re.ests=re.ests, fn=fn, tests=tests, full.warns=full.warns, null.warns=null.warns))
})
save.image("z:/a_transfer/gerlind_early_inf/new/early_inf.RData")
load("z:/a_transfer/gerlind_early_inf/new/early_inf.RData")
xx=lapply(all.res, function(x){x$fn[2, ]})
xx=matrix(unlist(xx), nrow=length(xx), byrow=T)
colnames(xx)=names(all.res[[1]]$fn)
apply(xx, 2, mean)
apply(xx, 2, median)
apply(xx, 2, range)

xx=c()
for(i in 1:length(all.res)){
	x=all.res[[i]]$tests
	x=data.frame(term=rownames(x), x)
	xx=rbind(xx, x)
}
xx=mapply(FUN=tapply, xx[, -1], MoreArgs=list(INDEX=xx[, 1], FUN=mean))

xx=c()
for(i in 1:length(all.res)){
	x=all.res[[i]]$fe.ests
	x=data.frame(term=rownames(x), x)
	xx=rbind(xx, x)
}
xx=mapply(FUN=tapply, xx[, -1], MoreArgs=list(INDEX=xx[, 1], FUN=mean))
wt2(xx)




xx=mapply(FUN=tapply, xx[, -1], MoreArgs=list(INDEX=xx[, 1], FUN=median))
to.keep=c(rownames(xx)[!grepl(rownames(xx), pattern=":", fixed=T)], 
	rownames(xx)[grepl(rownames(xx), pattern=":", fixed=T) & xx[, "Pr..Chisq."]<=0.1])
wt.txt(to.keep)


age_group:version
cond:saliency

all.res2=parLapply(cl=cl, X=1:n.iter, fun=function(x){
	new.resp=test.data2$correct
	new.resp[unlist(lapply(to.change, sample, size=1))]=0
	ifull=glmer(new.resp~age_group*version+cond*saliency+trial_no+first_version+gender+
		(1+age_group.5+age_group.35+cond.control+cond.test+cond.underinfo+saliency.low+trial_no+first_version.scalar+gender.m||item)+
		(1+cond.control+cond.test+cond.underinfo+version.scalar+z.trial_no||id),
		family=binomial, data=test.data2, control=contr)
	inull=glmer(new.resp~trial_no+first_version+
		(1+age_group.5+age_group.35+cond.control+cond.test+cond.underinfo+saliency.low+trial_no+first_version.scalar+gender.m||item)+
		(1+cond.control+cond.test+cond.underinfo+version.scalar+z.trial_no||id),
		family=binomial, data=test.data2, control=contr)
	tests=drop1p(model.res=ifull, para=F, data=NULL, contr=contr, n.cores=c("all-1", "all"), to.del=NULL, return.model.results=F)
	fn=as.data.frame(anova(inull, ifull, test="Chisq"))
	fe.ests=summary(ifull)$coefficients
	re.ests=as.data.frame(summary(ifull)$varcor)
	tests=tests$drop1.res
	full.warns=lmer.warns(ifull)
	null.warns=lmer.warns(inull)
	save(file=paste(c("D:/gerlind_models_full/ws_", x, ".RData"), collapse=""), list=c("fe.ests", "re.ests", "fn", "tests", "full.warns", "null.warns"))
	return(list(fe.ests=fe.ests, re.ests=re.ests, fn=fn, tests=tests, full.warns=full.warns, null.warns=null.warns))
})
save.image("z:/a_transfer/gerlind_early_inf/new/early_inf.RData")
xx=c()
for(i in 1:length(all.res2)){
	x=all.res2[[i]]$tests
	x=data.frame(term=rownames(x), x)
	xx=rbind(xx, x)
}
xx=mapply(FUN=tapply, xx[, -1], MoreArgs=list(INDEX=xx[, 1], FUN=mean))
round(xx, 3)

xx=c()
for(i in 1:length(all.res2)){
	x=all.res2[[i]]$fe.ests
	x=data.frame(term=rownames(x), x)
	xx=rbind(xx, x)
}
xx=mapply(FUN=tapply, xx[, -1], MoreArgs=list(INDEX=xx[, 1], FUN=mean))
wt2(xx)

parLapply(cl=cl, X=1:length(cl), fun=function(x){rm(list=ls())})
stopCluster(cl)
