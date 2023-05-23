library(data.table)
library(ggplot2)

# indir repository
gitdir <- here::here() # please modify accordingly

source( file.path( gitdir, 'config.R' ) )  # loads the paths


# age heaping ----
# consider the participants only ----
# for shifting paper

# use the Whipple's index to assess if there is age heaping,
# i.e. If the population sizes show pronounced peaks or gaps at ages ending in 0 or 5,
# it suggests the presence of age heaping at 10 years or 5 years
# Whipple's index = |P0 - E0| + |P5 - E5| / P
# Compare the calculated index value to specific thresholds to determine the level of age heaping. Generally, higher index values indicate higher levels of age heaping.
args <- list()

reported.partnerships <- as.data.table(read.csv(file.path(file.participation)))

dpart <- reported.partnerships[COMM == 'inland']
setnames(dpart, c('AGEYRS', 'ROUND', 'SEX', 'COMM', 'PARTICIPANT'),
         c('part.age', 'part.round', 'part.sex', 'part.comm', 'part'))

dpart[, part.sex := ifelse(part.sex == 'F', 'Female', 'Male')]
ggplot(dpart, aes(x = part.age, y = part, fill = part.sex)) +
  # geom_line() +
  geom_bar(stat = 'identity', alpha = .6, position = position_dodge()) +
  facet_grid(paste0('Round ', part.round)~part.sex, scales = 'free') +
  scale_color_manual(values = c('#dd3497', '#1f78b4')) +
  scale_fill_manual(values = c( '#f768a1' , '#41b6c4')) +
  geom_text(
    aes(label = part.age),
    size = 1.5,
    position = position_dodge(1), vjust = .5,
    inherit.aes = TRUE
  ) +
  labs(x = "", y = 'Participant counts', fill = '', colour = '') +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_line(linewidth = 0.2),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, linewidth = 0.3),
        axis.line = element_line(linewidth = 0.2),
        axis.ticks = element_line(linewidth = 0.2),
        
        strip.background = element_blank(),
        axis.text = element_text(size = 5, family = 'sans'),
        legend.text = element_text(size = 5, family = 'sans'),
        strip.text = element_text(size = 7, family = 'sans'),
        legend.title = element_blank(),
        axis.title = element_text(size = 7, family = 'sans')
  )
args$prj.dir <- here::here()
tmp <- paste0(file.path('figures', "part_size.png"))
cat("\nSave capped contacts plots to file ", tmp)
ggsave(file = tmp,  width = 15, height = 20, units = 'cm', dpi = 310, limitsize = FALSE)


###
dpart <- reported.partnerships[, list(part = .N),
                               by = c('part.sex', 'part.hiv', 'part.age', 'part.comm', 'part.round')]

dpart <- dpart[part.comm == 'inland']

dpart[, part.sex := ifelse(part.sex == 'F', 'Female', 'Male')]

ggplot(dpart[part.hiv == 'P'], aes(x = part.age, y = part, fill = part.sex)) +
  # geom_line() +
  geom_bar(stat = 'identity', alpha = .6, position = position_dodge()) +
  facet_grid(part.round~part.sex + paste0('HIV status: ', part.hiv), scales = 'free') +
  scale_color_manual(values = c('#dd3497', '#1f78b4')) +
  scale_fill_manual(values = c( '#f768a1' , '#41b6c4')) +
  geom_text(
    aes(label = part.age),
    size = 1.5,
    position = position_dodge(1), vjust = .5,
    inherit.aes = TRUE
  ) +
  labs(x = "", y = 'Participant counts', fill = '', colour = '') +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_line(linewidth = 0.2),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, linewidth = 0.3),
        axis.line = element_line(linewidth = 0.2),
        axis.ticks = element_line(linewidth = 0.2),
        
        strip.background = element_blank(),
        axis.text = element_text(size = 5, family = 'sans'),
        legend.text = element_text(size = 5, family = 'sans'),
        strip.text = element_text(size = 7, family = 'sans'),
        legend.title = element_blank(),
        axis.title = element_text(size = 7, family = 'sans')
  )
args$prj.dir <- here::here()
tmp <- paste0(file.path(args$prj.dir, 'results', 'shift', "part_size_hiv_p.png"))
cat("\nSave capped contacts plots to file ", tmp)
ggsave(file = tmp,  width = 15, height = 12, units = 'cm', dpi = 310, limitsize = FALSE)


ggplot(dpart[part.hiv == 'N'], aes(x = part.age, y = part, fill = part.sex)) +
  # geom_line() +
  geom_bar(stat = 'identity', alpha = .6, position = position_dodge()) +
  facet_grid(part.round~part.sex + paste0('HIV status: ', part.hiv), scales = 'free') +
  scale_color_manual(values = c('#dd3497', '#1f78b4')) +
  scale_fill_manual(values = c( '#f768a1' , '#41b6c4')) +
  geom_text(
    aes(label = part.age),
    size = 1.5,
    position = position_dodge(1), vjust = .5,
    inherit.aes = TRUE
  ) +
  labs(x = "", y = 'Participant counts', fill = '', colour = '') +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_line(linewidth = 0.2),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, linewidth = 0.3),
        axis.line = element_line(linewidth = 0.2),
        axis.ticks = element_line(linewidth = 0.2),
        
        strip.background = element_blank(),
        axis.text = element_text(size = 5, family = 'sans'),
        legend.text = element_text(size = 5, family = 'sans'),
        strip.text = element_text(size = 7, family = 'sans'),
        legend.title = element_blank(),
        axis.title = element_text(size = 7, family = 'sans')
  )
args$prj.dir <- here::here()
tmp <- paste0(file.path(args$prj.dir, 'results', 'shift', "part_size_hiv_n.png"))
cat("\nSave capped contacts plots to file ", tmp)
ggsave(file = tmp,  width = 15, height = 12, units = 'cm', dpi = 310, limitsize = FALSE)
