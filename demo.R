head(mtcars) %>% 
  gt::gt() %>% 
  gtExtras::gt_theme_nytimes() %>% 
  gt::tab_header(title = "Table styled like the NY Times")

library(palmerpenguins)
library(gt)
library(reactablefmtr)


stat7 <- plyr_data %>% dplyr::group_by(over_type) %>% 
  dplyr::summarise(Runs = sum(runs_off_bat), Six = sum(isSix), Four = sum(isFour),
                   SR = round(sum(runs_off_bat)/length(runs_off_bat)*100,2))
stat8 <- table(stat6$over_type) %>% t() %>% as.data.frame()
stat8 <- stat8[,-1]
colnames(stat8) = c("over_type", "Dismissed")
table1 <- left_join(stat7, stat8) %>% dplyr::arrange(desc(over_type))
colnames(table1) <- c("Over Type","Runs","Sixes","Fours","SR","Dismissed")
table1 |> gt() |> tab_header(title = md("***Over-wise Summary***"))

###############################################

reactable(
  table1,
  pagination = FALSE,
  compact = TRUE,
  defaultColDef = colDef(
    cell = data_bars(table1,
                     fill_color = c("#1efffd", "#1e20ff"), 
                     fill_gradient = TRUE, 
                     background = "lightgrey")
  )
)









