library(R0)
library(ggplot2)
library(tidyverse)
library(stats)
library(dplyr)
out_out=out[1:30,2]
init_case=out_out

routbreak <- function(n=100, Ret, GT_obj, initial_cases = 1) {
  # Set up time series of incident cases
  y <- rep(0, n + length(GT_pmf))
  y[seq_len(length(initial_cases))] <- initial_cases
  # Outbreak starts on 2020-02-15
  dates <- as.Date("2020-02-15") + 0:(n-1)
  # Extract serial interval PMF, ignore support at 0.
  GT_pmf <- GT_obj$GT[-1]
  
  # Loop over all time points
  for (i in 1:n) {
    date <- dates[i]
    y[i + 1:length(GT_pmf)] <- y[i] * (Ret(date) * GT_pmf) + y[i + 1:length(GT_pmf)]
  }
  
  # Data frame with the result. Assume we start on 15th of Feb
  res <- data.frame(Date=dates, y=y[1:n])
  
  #Done
  return(res)
}

est_rt_wt <- function(ts, GT_obj) {
  end <- length(ts) - (length(GT_obj$GT)-1)
  R0::est.R0.TD(ts, GT=GT_obj, begin=1, end=end, nsim=1000)
}

GT_pmf <- structure( c(0, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.2), names=0:7)



GT_obj <- R0::generation.time("empirical", val=GT_pmf)
GT_obj

Ret <- function(date) ifelse(date <= as.Date("2020-03-15"), 1.5, 1.5)
out <- routbreak(n=60, Ret=Ret, GT_obj=GT_obj)
out


############### Function to prdict next n (n2) days
GT <-  c(0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.2)
n1=30
n2=30
out_1st=out[1:n1,2]
X=out_1st

for(i in 1:n2) {                                        
  Y=X[(length(X) - 6):length(X)]
  new_case=sum(Y*Rt*rev(GT))
  X[length(X)+1]<-new_case
}

Date <- as.Date("2020-03-16") + 0:(n2-1)
predicted_case=X[(n1+1):(n1+n2)]
predicted_out <- data.frame (Date,predicted_case)
predicted_out
################


out_cum=cumsum(out$y)
out_cum

#write.csv(out_cum,"C:\\Users\\sanad\\OneDrive - Kennesaw State University\\Desktop\\RDRX\\results\\rcumout.csv")

p1 <- ggplot(out, aes(x=Date, y=y)) + geom_line() + ylab("No. cases")
p1

rt_wt <- est_rt_wt( out$y, GT=GT_obj)
Ret_true <- data.frame(Date=out$Date) %>% mutate(R_hat=Ret(Date), Method="Truth")
rt_wt_df <- cbind(Date=out$Date[as.numeric(names(rt_wt$R))], R_hat=rt_wt$R, rt_wt$conf.int, Method="W & T, correct GT")

ggplot(rt_wt_df, aes(x=Date, y=R_hat, color=Method)) +  
  geom_ribbon(aes(x=Date,  ymin=lower, ymax=upper, color=NULL), alpha=0.1) +
  geom_line(data=Ret_true, aes(x=Date, y=R_hat, color=Method), lwd=3) + 
  geom_line(lwd=1.2) +
  scale_color_brewer(type="qual",palette="Set2", name="Method:") +
  coord_cartesian(ylim=c(0, 5)) +
  ylab(expression(R[e](t))) +
  theme(legend.position="bottom")
