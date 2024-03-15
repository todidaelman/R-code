custom_theme <- list(
  theme_ipsum(),
  theme(axis.line = element_line()),
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()),
  theme(axis.ticks.x = element_line(color = "black"),
        axis.ticks.y = element_line(color = "black"))
)


## COLOURS ##

general_colours <- c("#1c027a", "#6740f5", "#b4a2f5", "#04543d" , "#08aa7c", "#b3e6d7","#996212", "#f6b251", "#fac87f")


more <- c("#19647E", "#1A8EB8")
## base - NS

symbols <- c(NS      = "NS    (|)",
             `NE-SW` = "NE-SW (/)",
             EW      = "EW    (―)",
             `NW-SE` = "NW-SE (\\)")


####
c(
  0 = "#6740f5",
  45 = "#04543d",
  90 = "#f6b251",
  135 = "#19647E",
  180 = "#b4a2f5",
  225 = "#08aa7c",
  270 = "#fac87f",
  315 = "#1A8EB8",
  345 = "#C65353"
)
###
colours_ref_street_angles <- c("#6740f5", "#04543d", "#f6b251", "#19647E", "#b4a2f5", "#08aa7c", "#fac87f", "#1A8EB8", "#C65353")

###

mutate(street_orientation = factor(street_orientation, levels = c("base", "NS", "EW", "NE-SW", "NW-SE"))) %>%
  c(
    base = "#C65353"
    NS = "#674CED",
    EW = "#E8A343",
    NE-SW = "#067556",
    NW-SE = "#207E9E"
    
    
    
  )

colours_ref_street_orientations <-  c("#C65353", "#674CED", "#E8A343", "#067556","#207E9E")

custom_blue_colors <- colorRampPalette(c("darkblue", "lightblue"))(10)
