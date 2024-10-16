library(lpSolve)
library(dplyr)
library(ggplot2)
library(reshape2)

source("get_lp_results_basic.R")
source("get_range_of_possible_probs.R")

##################################################

outcome_basic <-
  do.call(rbind.data.frame,
          apply(
            cross_join(data.frame(minmax = c("min", "max")),
                       data.frame(case =  c("CBJ", "BCJ", "CJB", "JCB", "BJC", "JBC"))) %>%
              cross_join(data.frame(a_time = c(1130, 1200, 1230, 1330, 1400, 1430, 1510))),
            MARGIN = 1,
            get_lp_results_basic)
  )

##################################################

# read.csv("Betfair_odds.csv") %>%
#   mutate(B_elim = (1/elim_B_back + 1/elim_B_lay)/2) %>%
#   mutate(B_elim_range = (1/elim_B_back - 1/elim_B_lay)/2) %>%
#   mutate(C_Winner = (1/winner_C_back + 1/winner_C_lay)/2) %>%
#   mutate(C_Winner_range = (1/winner_C_back - 1/winner_C_lay)/2) %>%
#   mutate(time = as.POSIXct(paste("2024-10-09 ", substr(time, 0, 2), ":", substr(time, 3, 4) ,":00 BST", sep = ""))) %>%
#   ggplot(aes(x = time)) +
#   geom_point(aes(y  = B_elim)) +
#   geom_errorbar(aes(ymax = B_elim + B_elim_range, ymin = B_elim - B_elim_range)) +
#   geom_point(aes(y  = C_Winner)) +
#   geom_errorbar(aes(ymax = C_Winner + C_Winner_range, ymin = C_Winner - C_Winner_range))

jpeg("ToryLeadershipContest_BettingMarkets_LinearProgramming_1.jpg", width = 1200, height = 600)
read.csv("Betfair_odds.csv") %>% 
  mutate(B_Eliminated = (1/elim_B_back + 1/elim_B_lay)/2) %>% 
  # mutate(B_elim_range = (1/elim_B_back - 1/elim_B_lay)/2) %>% 
  mutate(C_Winner = (1/winner_C_back + 1/winner_C_lay)/2) %>% 
  # mutate(C_Winner_range = (1/winner_C_back - 1/winner_C_lay)/2) %>% 
  select(time, B_Eliminated, C_Winner) %>%
  melt(id.vars = "time") %>% 
  rename(prob = value) %>% 
  mutate(time = as.POSIXct(
    paste("2024-10-09 ", substr(time, 0, 2), ":", substr(time, 3, 4) ,":00 BST", sep = ""))) %>%
  ggplot(aes(x = time, y = prob, colour = variable)) +
  geom_point(size = 3)
dev.off()

##################################################

outcome_basic %>% filter(time == 1130) %>% dcast(case ~ minmax, value.var = "value") %>% arrange(-max)

##################################################

jpeg("ToryLeadershipContest_BettingMarkets_LinearProgramming_2.jpg", width = 1200, height = 600)
outcome_basic %>%
  dcast(case + time ~ minmax, value.var = "value") %>% 
  mutate(time = as.POSIXct(
    paste("2024-10-09 ", substr(time, 0, 2), ":", substr(time, 3, 4) ,":00 BST", sep = ""))) %>%
  mutate(prob = (max + min)/2) %>% 
  mutate(prob_range = (max - min)/2) %>% 
  ggplot(aes(x = time, y = prob, colour = case)) +
  geom_point(size = 2) +
  geom_line() +
  geom_errorbar(aes(ymax = prob + prob_range, ymin = prob - prob_range, width = 0.8))
dev.off()

##################################################

# outcome_rangeprobs <- 
#   do.call(rbind.data.frame,
#           apply(
#             cross_join(data.frame(candidates =  c("CB", "CJ", "BJ")),
#                        data.frame(a_time = c(1130, 1200, 1230, 1330, 1400, 1430, 1510))) %>% 
#               cross_join(data.frame(prob = seq(0.001, 0.999, 0.001))),
#             MARGIN = 1,
#             get_range_of_possible_probs))

jpeg("ToryLeadershipContest_BettingMarkets_LinearProgramming_3.jpg", width = 1200, height = 600)
outcome_rangeprobs %>% 
  filter(value != 0) %>% 
  group_by(time, candidates) %>% 
  summarise(minp = min(prob), maxp = max(prob), .groups = "drop") %>%
  mutate(prob = (minp+maxp)/2, eb_size = (maxp-minp)/2) %>% 
  mutate(time = as.POSIXct(
    paste("2024-10-09 ", substr(time, 0, 2), ":", substr(time, 3, 4) ,":00 BST", sep = ""))) %>%
  ggplot(aes(x = prob, y = time, colour = candidates)) +
  geom_errorbarh(aes(xmax = prob + eb_size, xmin = prob - eb_size)) +
  facet_grid(. ~ candidates) + 
  theme(legend.position="none")
dev.off()

jpeg("ToryLeadershipContest_BettingMarkets_LinearProgramming_4.jpg", width = 1200, height = 600)
outcome_rangeprobs %>% 
  filter(value != 0) %>% 
  filter(candidates != "BJ") %>% 
  group_by(time, candidates) %>% 
  summarise(minp = min(prob), maxp = max(prob), .groups = "drop") %>%
  mutate(prob = (minp+maxp)/2, eb_size = (maxp-minp)/2) %>% 
  rename(H2H = candidates) %>% 
  ggplot(aes(x = prob, y = H2H, colour = H2H)) +
  geom_errorbarh(aes(xmax = prob + eb_size, xmin = prob - eb_size)) +
  facet_wrap(. ~ time) + 
  theme(legend.position="none")
dev.off()

jpeg("ToryLeadershipContest_BettingMarkets_LinearProgramming_5.jpg", width = 1200, height = 600)
outcome_rangeprobs %>% 
  filter(value != 0) %>% 
  group_by(time, candidates) %>% 
  summarise(minp = min(prob), maxp = max(prob), .groups = "drop") %>%
  group_by(candidates) %>% 
  summarise(minp = max(minp), maxp = min(maxp), .groups = "drop") %>%
  mutate(prob = (minp+maxp)/2, eb_size = (maxp-minp)/2) %>% 
  rename(H2H = candidates) %>% 
  ggplot(aes(x = prob, y = H2H, colour = H2H)) +
  geom_errorbarh(aes(xmax = prob + eb_size, xmin = prob - eb_size)) + 
  theme(legend.position="none")
dev.off()

outcome_rangeprobs %>% 
  filter(value != 0) %>% 
  group_by(time, candidates) %>% 
  summarise(minp = min(prob), maxp = max(prob)) %>% 
  ungroup() %>% 
  group_by(candidates) %>% 
  summarise(minp = max(minp), maxp = min(maxp))

