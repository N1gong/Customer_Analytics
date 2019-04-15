One <- readxl::read_excel("data/pentathlon-II.xls", range = "B1:I4") %>%
  fix_names() %>%
  to_fct()



Two <- readxl::read_excel("data/pentathlon-II.xls", range = "B6:I9") %>%
  fix_names() %>%
  to_fct()

Three <- readxl::read_excel("data/pentathlon-II.xls", range = "B11:I14") %>%
  fix_names() %>%
  to_fct()

Four <- readxl::read_excel("data/pentathlon-II.xls", range = "B16:I19") %>%
  fix_names() %>%
  to_fct() %>%
  slice(1)

Five <- readxl::read_excel("data/pentathlon-II.xls", range = "B21:I24") %>%
  fix_names() %>%
  to_fct()

subs1 <-One[2,]
unsubs1 <- One[3, ]
churn1 <- unlist(unname(One[1,1:7]))
cogs=0.6
retained <- cumprod(c(1, (1-churn1)))
left <- 1 - retained
prof_s <- subs1*(1-cogs)
prof_un <- unsubs1*(1-cogs)
exp_profit <- retained*prof_s + left*prof_un
time <- 1:8
d <- (1 +0.1)^(1/52) - 1
PV_exp_profit <- exp_profit / (1 + d)^time
CLV <- cumsum(PV_exp_profit)

clv2 <- function(D, subs, unsubs, churn, cogs, T, time) {
  d <- (1 +D)^(1/T) - 1
  retained <- cumprod(c(1, (1-churn)))
  left <- retained*c(0,churn)
  prof_s <- subs*(1-cogs)
  prof_un <- unsubs*(1-cogs)
  exp_profit <- retained*prof_s + left*prof_un
  PV_exp_profit <- exp_profit / (1 + d)^time
  CLV <- cumsum(PV_exp_profit)
  return(unname(CLV))
}

byweek <- data.frame(week=c(1:8))

short <- byweek %>%
  mutate(One = clv2(D=0.1, subs1, unsubs1, churn1, cogs, T=52, time=1:8),
         Two = clv2(D=0.1, subs2, unsubs2, churn2, cogs, T=52, time=1:8),
         Three = clv2(D=0.1, subs3, unsubs3, churn3, cogs, T=52, time=1:8),
         Four = clv2(D=0.1, subs4, unsubs4, churn4, cogs, T=52, time=1:8),
         Five = clv2(D=0.1, subs5, unsubs5, churn5, cogs, T=52, time=1:8))


mid <- data.frame(week=c(1:52)) %>%
  mutate(One = c(clv2(D=0.1, rep(Average$rev_sub[1],52), rep(Average$rev_unsub[1],52), rep(Average$schurn[1],51), cogs, T=52, time=1:52)),
         Two = c(clv2(D=0.1, rep(Average$rev_sub[2],52), rep(Average$rev_unsub[2],52), rep(Average$schurn[2],51), cogs, T=52, time=1:52)),
         Three = c(clv2(D=0.1, rep(Average$rev_sub[3],52), rep(Average$rev_unsub[3],52), rep(Average$schurn[3],51), cogs, T=52, time=1:52)),
         Four = c(clv2(D=0.1, rep(Average$rev_sub[4],52), rep(Average$rev_unsub[4],52), rep(Average$schurn[4],51), cogs, T=52, time=1:52)),
         Five = c(clv2(D=0.1, rep(Average$rev_sub[5],52), rep(Average$rev_unsub[5],52), rep(Average$schurn[5],51), cogs, T=52, time=1:52)))

long <- data.frame(week=c(1:104)) %>%
  mutate(One = c(clv2(D=0.1, rep(Average$rev_sub[1],104), rep(Average$rev_unsub[1],104), rep(Average$schurn[1],103), cogs, T=52, time=1:104)),
         Two = c(clv2(D=0.1, rep(Average$rev_sub[2],52*2), rep(Average$rev_unsub[2],52*2), rep(Average$schurn[2],103), cogs, T=52, time=1:104)),
         Three = c(clv2(D=0.1, rep(Average$rev_sub[3],52*2), rep(Average$rev_unsub[3],52*2), rep(Average$schurn[3],103), cogs, T=52, time=1:104)),
         Four = c(clv2(D=0.1, rep(Average$rev_sub[4],52*2), rep(Average$rev_unsub[4],52*2), rep(Average$schurn[4],103), cogs, T=52, time=1:104)),
         Five = c(clv2(D=0.1, rep(Average$rev_sub[5],52*2), rep(Average$rev_unsub[5],52*2), rep(Average$schurn[5],103), cogs, T=52, time=1:104)))





clv(D=0.1,subs=subs1, unsubs=unsubs1, churn=churn1, cogs=0.6, T=52, time=1:8)
