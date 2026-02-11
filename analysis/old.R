{
  rm(list = ls())
  options(warn = 0)
  
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(viridis)
  
  df <- fread("BORIS/results.csv")
  df <- df %>% select("Time", "Behavior", "Type" )
}

{
  timeline <- tibble(
    Time = 0:max(ceiling(df$Time)), 
    Behavior = "timestamp",
    Type = "POINT"
  )
  
  df_full <- bind_rows(df, timeline) %>%
    arrange(Time, Behavior)
  
  
  df <- df_full %>%
    mutate( flow = case_when(
      Behavior %in% c("left-in", "right-in") ~ 1,
      Behavior %in% c("left-out", "right-out") ~ -1,
      TRUE ~ 0
    ),
    Time_bin = ceiling(Time)
    ) %>%
    arrange(Time) %>%
    mutate(N_inside = cumsum(flow))
  
  df_time <- df %>%
    group_by(Time_bin) %>%
    summarize(N_inside = last(N_inside),
              left_in = sum(Behavior == "left-in"),
              right_in = sum(Behavior == "right-in"),
              left_out = sum(Behavior == "left-out"),
              right_out = sum(Behavior == "right-out"),
              income = left_in+right_in,
              outcome = left_out+right_out,
              .groups = "drop")  %>%
    arrange(Time_bin)
  
  
  df_bridge <- df %>% 
    filter(Behavior == "Bridge")
  
  df_bridge <- data.frame(
    event = 1:3,
    df_bridge[df_bridge$Type == "START", "Time"],
    df_bridge[df_bridge$Type == "STOP", "Time"]
  )
  
  colnames(df_bridge) <- c("event", "start", "stop")
  
  ggplot(df_time, aes(x = Time_bin)) +
    geom_rect(data= df_bridge, aes(xmin = start, xmax = stop, 
                                   ymin = 0, ymax = 40, group = event),
              fill = viridis(6)[5], alpha = 0.4, inherit.aes = FALSE)+
    geom_line(aes(y = N_inside)) +
    geom_line(aes(y=income + outcome), col=viridis(2)[1])+
    labs(title = "Number of individuals in area", 
         x = "Time (sec)", y = "Individuals")+
    scale_x_continuous(breaks = c(0,200,400)) +
    scale_y_continuous(breaks = c(0,20,40)) +
    theme_classic() +
    theme(
      aspect.ratio = 1/1.61803398875,
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 11)
    )
  ggsave("output/results.pdf", device = cairo_pdf, family = "Arial", width = 6, height = 4)
}


df_bridge
df_traffic <- data.frame(
  Time_bin = df_time$Time_bin,
  N_inside = df_time$N_inside,
  traffic = df_time$income + df_time$outcome,
  in_traffic = df_time$income,
  out_traffic = df_time$outcome)

Time_bin <- df_time$Time_bin
non_bridge <-  (Time_bin < df_bridge[1,2]) | 
  (Time_bin > df_bridge[1,3] & Time_bin < df_bridge[2,2]) | 
  (Time_bin > df_bridge[2,3] & Time_bin < df_bridge[3,3])
bridge <- !non_bridge
bridge_end <- Time_bin == 33 | Time_bin == 111

df_traffic$non_bridge <- non_bridge
df_traffic$bridge <- bridge
df_traffic$bridge_end <- bridge_end

library(zoo)
df_traffic <- df_traffic %>%
  mutate(
    traffic_smooth = rollmean(traffic, k = 5, fill = NA, align = "center"),
    N_inside_smooth = rollmean(N_inside, k = 5, fill = NA, align = "center")
  )

ggplot(df_traffic, aes(x = Time_bin, y = traffic_smooth)) +
  geom_line()+
  geom_line(aes(y = N_inside_smooth))+
  geom_rect(data= df_bridge, aes(xmin = start, xmax = stop, 
                                 ymin = 0, ymax = 20, group = event),
            fill = viridis(6)[5], alpha = 0.4, inherit.aes = FALSE)

ggplot(subset(df_traffic, bridge), aes(y = traffic, x = bridge_end))+
  geom_jitter(width = 0.1)


library(viridis)
ggplot(df_traffic, aes(x = N_inside, y = traffic,
                       col=bridge))+
  geom_point(alpha = .5, 
             position = position_jitter(width = .25, height = .25)) +
  scale_color_viridis(discrete = T, end = .5) +
  theme(legend.position = "none") +
  theme_classic()

ggplot(df_traffic, aes(x = N_inside, y = in_traffic,
                       col=bridge))+
  geom_point(alpha = .5, 
             position = position_jitter(width = .25, height = .25)) +
  scale_color_viridis(discrete = T, end = .5) +
  theme(legend.position = "none") 

ggplot(df_traffic, aes(x = N_inside, y = out_traffic,
                       col=bridge))+
  geom_point(alpha = .5, 
             position = position_jitter(width = .25, height = .25)) +
  scale_color_viridis(discrete = T, end = .5) +
  theme(legend.position = "none") 

library(lme4)  
library(car)  

a <- rle(df_traffic$bridge)
df_traffic$event <- rep(1:6, a$lengths)

r <- glmer(traffic ~ N_inside * bridge + (1|event), family = "poisson", data = df_traffic)
Anova(r)

r <- glmer(in_traffic ~ N_inside * bridge + (1|event), family = "poisson", data = df_traffic)
Anova(r)

r <- glmer(out_traffic ~ N_inside * bridge + (1|event), family = "poisson", data = df_traffic)
Anova(r)


r <- glm(traffic ~ bridge, family = "poisson", data = df_traffic_sub)
Anova(r)

df_traffic$time_group <- floor(df_traffic$Time_bin / 5)
df_traffic_sub <- df_traffic %>%
  group_by(time_group) %>%
  summarise(N_inside = mean(N_inside),
            traffic = sum(traffic),
            in_traffic = sum(in_traffic),
            out_traffic = sum(out_traffic),
            bridge = round(mean(bridge)))


ggplot(df_traffic_sub, aes(x = N_inside, y = traffic,
                           col=as.factor(bridge)))+
  geom_point(alpha = .5) +
  scale_color_viridis(discrete = T, end = .5) +
  theme(legend.position = "none") 


a_1 <- acf(df_traffic$N_inside)
acf(df_traffic$traffic)


library(depmixS4)

mod <- depmix(list(
  N_inside ~ 1, traffic ~ 1, in_traffic ~ 1, 
  out_traffic ~ 1
), data = df_traffic, nstates = 2, family = list(
  gaussian(), gaussian(), gaussian(), gaussian()))

# Fit model
fit <- fit(mod)

# Get estimated states
df_traffic$hmm_state <- posterior(fit)$state


df_traffic$active <- df_traffic$N_inside
df_traffic[df_traffic$bridge,]$active <- df_traffic[df_traffic$bridge,]$active - 7


ggplot(df_traffic, aes(x = active, y = traffic,
                       col=as.factor(bridge)))+
  geom_point(alpha = .5,
             position = position_jitter(width = .25, height = .25)) +
  scale_color_viridis(discrete = T, end = .5) +
  theme(legend.position = "none") 

install.packages("depmixS4")
library(depmixS4)

mod <- depmix(list(
  in_traffic ~ 1, out_traffic ~ 1), 
  data = df_traffic, nstates = 3, family = list(
    gaussian(), gaussian()))

fit <- fit(mod)
df_traffic$hmm_state <- posterior(fit)$state

ggplot(df_traffic, aes(x = Time_bin, y = N_inside,
                       col = as.factor(hmm_state))) + 
  geom_point()


