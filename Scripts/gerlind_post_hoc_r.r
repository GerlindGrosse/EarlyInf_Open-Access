xdata=read.table(file="/home/roger/roger/2017/to_do/gerlind_data.txt", header=T, sep="\t")#tab separated file
all.res=c()
vars=c("adhoc", "scalar")
for(a in unique(xdata$age_group)){
	sel.data=subset(xdata, age_group!=a)
	for(j in vars){
		test.data=subset(sel.data, !is.na(sel.data[, j]))
		xx=wilcox.test(test.data[, j]~test.data$age_group, paired=F)
		yy=table(sel.data$age_group)
		all.res=rbind(all.res, data.frame(var=j, age1=names(yy)[1], age2=names(yy)[2], 
			U=min(c(xx$statistic, prod(yy)-xx$statistic)), 
			n1=yy[1], n2=yy[2], p=xx$p.value))
	}
}

all.res#comprises the result
#you still need to apply the bonferoni (or whatever) to the p-values
