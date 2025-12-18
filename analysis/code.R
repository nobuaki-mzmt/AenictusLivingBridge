# Data analysis for Aenictus bridge frmation
# preparation
{
  rm(list = ls())
  options(warn = 0)

  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(viridis)
  library(car)
}

# data preparation
{
  df <- fread("BORIS/results.csv")
  df <- df %>% select("Time", "Behavior", "Type" )
  timeline <- tibble(
    Time = 0:max(ceiling(df$Time)), 
    Behavior = "timestamp",
    Type = "POINT"
  )
  df <- bind_rows(df, timeline) %>%
    arrange(Time, Behavior)
  
  df <- df %>%
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
  
  
  df_traffic$time_group <- floor(df_traffic$Time_bin / 11)
  df_traffic_sub <- df_traffic %>%
    group_by(time_group) %>%
    summarise(N_inside = mean(N_inside),
              traffic = mean(traffic),
              in_traffic = sum(in_traffic),
              out_traffic = sum(out_traffic),
              bridge = round(mean(bridge)))
  
}


# plot overall activity
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

# relationship between traffic and the number of ants
ggplot(df_traffic_sub, aes(x = N_inside, y = traffic,
                       col=as.factor(bridge)))+
  geom_point(alpha = .5) +
  scale_color_viridis(discrete = T, end = .5) +
  theme_classic() + 
  theme(legend.position = "none", aspect.ratio = 3/4) +
  scale_x_continuous(limits = c(0,30))+
  scale_y_continuous(limits = c(0,10), breaks = c(0,5,10)) +
  stat_smooth(method = "lm")+
  xlab("Number of ants near the gap")+
  ylab("Traffic (ants/sec)")

ggsave("output/relationship.pdf")


r <- lm(traffic ~ N_inside * bridge, 
        data = df_traffic_sub)
Anova(r)



