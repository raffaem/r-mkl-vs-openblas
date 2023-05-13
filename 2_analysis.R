library(DescTools)
library(ggplot2)
library(knitr)
library(dplyr)

rm(list=ls())

rm.out <- function(x) {
	q1 <- quantile(x, 0.25)[[1]]
	q3 <- quantile(x, 0.75)[[1]]
	iqr <- q3-q1
	out <- (x < q1-1.5*iqr) | (x > q3+1.5*iqr)
	return(x[!out])
}

rm.out2 <- function(x) {
	z <- (x-mean(x))/sd(x)
	out <- (z >= 2) | (z <= -2)
	return(x[!out])
}

dir.create("./data/img", showWarnings=F)

mkl.df <- read.csv("./data/mkl/results.csv")
ob.df <- read.csv("./data/openblas/results.csv")

test_groups <- unique(mkl.df$test_group)
findf <- data.frame(time=c(),lib=c(),test=c())
meandf <- data.frame(test_group=c(),test=c(),x=c(),y=c(),diff=c(),s=c(),p=c())

for(test_group in test_groups) {
	
	mask <- mkl.df["test_group"]==test_group
	tests <- unique(mkl.df[mask,"test"])
	
	for(test in tests) {
		
		if(test=="eigen" | test=="inverse" | test=="escoufier") {
			next
		}
		
		print(test)
		
		mkl <- mkl.df$user[mkl.df$test==test]*(10**3)
		ob <- ob.df$user[ob.df$test==test]*(10**3)
		
		res <- t.test(mkl,ob)
		mkl.mean <- res$estimate[[1]]
		ob.mean <- res$estimate[[2]]
		diff <- ob.mean-mkl.mean
		tmpdf <- data.frame(test_group=test_group,
												test=test,
												mkl=mkl.mean,
												ob=ob.mean,
												diff=diff,
												diffp=(diff/mkl.mean)*100,
												s=res$statistic,
												p=res$p.value)
		meandf <- rbind(meandf, tmpdf)
		
		mkl <- rm.out2(mkl)
		ob <- rm.out2(ob)
		
		lib <- c(rep("MKL",length(mkl)),rep("OpenBLAS",length(ob)))

		tmpdf <- data.frame(time=c(mkl,ob),lib=lib,test=test)
		findf <- rbind(findf, tmpdf)

	}
}

p <- ggplot(findf, aes(test, time)) + 
	geom_boxplot(aes(colour = lib)) +
	scale_x_discrete(guide = guide_axis(angle = 90))
p

meandf$diffp <- round(meandf$diffp, 2)
meandf$s <- round(meandf$s, 2)
meandf$p <- round(meandf$p, 2)

meandf <- meandf %>% dplyr::arrange(-diffp)
rownames(meandf) <- NULL
colnames(meandf) <- c(
	"Test Group",
	"Test",
	"Intel MLK",
	"OpenBLAS",
	"Diff",
	"Diff (%)",
	"t",
	"p-value"
)
knitr::kable(meandf)
