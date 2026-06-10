################################################################################
#                           TABLES AND FIGURES                                 #
################################################################################
# Created by: Sarah Rauf 
# Date: 2026-04-02

# Description: code for generating figures (main text and supplementary) 
#              for RaufS MSc thesis 

################################# Figure 1 #####################################

tmin_0rmax_plot <- 
  traits_filtered_ci_df %>% 
  filter(treatment %in% c("A", "C", "E", "F", "H", "I", "K", "L")) %>% 
  ggplot(., aes(x = np, y = tmin_0rmax_mean, color = treatment)) +
  geom_point() + 
  geom_errorbar(aes(ymin = tmin_0rmax_lower, ymax = tmin_0rmax_upper, color = treatment), 
                width = 0.2, position=position_dodge(width=1)) +
  scale_colour_manual(values = c("#000000", # High N,P; enriched C
                                 "#a9a9a9", # High N,P; non-enriched C
                                 "#000075", # P-limited; enriched C
                                 "#3cb44b", # P-limited; non-enriched C
                                 "#800000", # N-limited; enriched C
                                 "#f58231", # N-limited; non-enriched C
                                 "#ffe119", # Low N,P; enriched C
                                 "#dcbeff"  # Low N,P; non-enriched C
  )) + 
  labs(color = "Method", y = bquote(T["min, 100%"]), x = NULL) + theme_classic() +
  facet_grid(~ c, switch = "x") +
  theme(strip.text = element_text(size=10), strip.placement = "outside",
        strip.background = element_blank(), legend.position="none",
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))

tmax_0rmax_plot <- 
  traits_filtered_ci_df %>% 
  filter(treatment %in% c("A", "C", "E", "F", "H", "I", "K", "L")) %>% 
  ggplot(., aes(x = np, y = tmax_0rmax_mean, color = treatment)) +
  geom_point() + 
  geom_errorbar(aes(ymin = tmax_0rmax_lower, ymax = tmax_0rmax_upper, color = treatment), 
                width = 0.2, position=position_dodge(width=1)) +
  scale_colour_manual(values = c("#000000", # High N,P; enriched C
                                 "#a9a9a9", # High N,P; non-enriched C
                                 "#000075", # P-limited; enriched C
                                 "#3cb44b", # P-limited; non-enriched C
                                 "#800000", # N-limited; enriched C
                                 "#f58231", # N-limited; non-enriched C
                                 "#ffe119", # Low N,P; enriched C
                                 "#dcbeff"  # Low N,P; non-enriched C
  )) + 
  labs(color = "Method", y = bquote(T["max, 100%"]), x = NULL) + theme_classic() +
  facet_grid(~ c, switch = "x") +
  theme(strip.text = element_text(size=10), strip.placement = "outside",
        strip.background = element_blank(), legend.position="none",
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))

breadth_0rmax_plot <- 
  traits_filtered_ci_df %>% 
  filter(treatment %in% c("A", "C", "E", "F", "H", "I", "K", "L")) %>% 
  ggplot(., aes(x = np, y = breadth_0rmax_mean, color = treatment)) +
  geom_point() + 
  geom_errorbar(aes(ymin = breadth_0rmax_lower, ymax = breadth_0rmax_upper, color = treatment), 
                width = 0.2, position=position_dodge(width=1)) +
  scale_colour_manual(values = c("#000000", # High N,P; enriched C
                                 "#a9a9a9", # High N,P; non-enriched C
                                 "#000075", # P-limited; enriched C
                                 "#3cb44b", # P-limited; non-enriched C
                                 "#800000", # N-limited; enriched C
                                 "#f58231", # N-limited; non-enriched C
                                 "#ffe119", # Low N,P; enriched C
                                 "#dcbeff"  # Low N,P; non-enriched C
  )) + 
  labs(color = "Method", y = bquote("Breadth (100%)"), x = NULL) + theme_classic() +
  facet_grid(~ c, switch = "x") +
  theme(strip.text = element_text(size=10), strip.placement = "outside",
        strip.background = element_blank(), legend.position="none",
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))

tmin_0.2rmax_plot <- 
  traits_filtered_ci_df %>% 
  filter(treatment %in% c("A", "C", "E", "F", "H", "I", "K", "L")) %>% 
  ggplot(., aes(x = np, y = tmin_0.2rmax_mean, color = treatment)) +
  geom_point() + 
  geom_errorbar(aes(ymin = tmin_0.2rmax_lower, ymax = tmin_0.2rmax_upper, color = treatment), 
                width = 0.2, position=position_dodge(width=1)) +
  scale_colour_manual(values = c("#000000", # High N,P; enriched C
                                 "#a9a9a9", # High N,P; non-enriched C
                                 "#000075", # P-limited; enriched C
                                 "#3cb44b", # P-limited; non-enriched C
                                 "#800000", # N-limited; enriched C
                                 "#f58231", # N-limited; non-enriched C
                                 "#ffe119", # Low N,P; enriched C
                                 "#dcbeff"  # Low N,P; non-enriched C
  )) + 
  labs(color = "Method", y = bquote(T["min, 80%"]), x = NULL) + theme_classic() +
  facet_grid(~ c, switch = "x") +
  theme(strip.text = element_text(size=10), strip.placement = "outside",
        strip.background = element_blank(), legend.position="none",
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))

tmax_0.2rmax_plot <- 
  traits_filtered_ci_df %>% 
  filter(treatment %in% c("A", "C", "E", "F", "H", "I", "K", "L")) %>% 
  ggplot(., aes(x = np, y = tmax_0.2rmax_mean, color = treatment)) +
  geom_point() + 
  geom_errorbar(aes(ymin = tmax_0.2rmax_lower, ymax = tmax_0.2rmax_upper, color = treatment), 
                width = 0.2, position=position_dodge(width=1)) +
  scale_colour_manual(values = c("#000000", # High N,P; enriched C
                                 "#a9a9a9", # High N,P; non-enriched C
                                 "#000075", # P-limited; enriched C
                                 "#3cb44b", # P-limited; non-enriched C
                                 "#800000", # N-limited; enriched C
                                 "#f58231", # N-limited; non-enriched C
                                 "#ffe119", # Low N,P; enriched C
                                 "#dcbeff"  # Low N,P; non-enriched C
  )) + 
  labs(color = "Method", y = bquote(T["max, 80%"]), x = NULL) + theme_classic() +
  facet_grid(~ c, switch = "x") +
  theme(strip.text = element_text(size=10), strip.placement = "outside",
        strip.background = element_blank(), legend.position="none",
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))

breadth_0.2rmax_plot <- 
  traits_filtered_ci_df %>% 
  filter(treatment %in% c("A", "C", "E", "F", "H", "I", "K", "L")) %>% 
  ggplot(., aes(x = np, y = breadth_0.2rmax_mean, color = treatment)) +
  geom_point() + 
  geom_errorbar(aes(ymin = breadth_0.2rmax_lower, ymax = breadth_0.2rmax_upper, color = treatment), 
                width = 0.2, position=position_dodge(width=1)) +
  scale_colour_manual(values = c("#000000", # High N,P; enriched C
                                 "#a9a9a9", # High N,P; non-enriched C
                                 "#000075", # P-limited; enriched C
                                 "#3cb44b", # P-limited; non-enriched C
                                 "#800000", # N-limited; enriched C
                                 "#f58231", # N-limited; non-enriched C
                                 "#ffe119", # Low N,P; enriched C
                                 "#dcbeff"  # Low N,P; non-enriched C
  )) + 
  labs(color = "Method", y = bquote("Niche Breadth (80%)"), x = NULL) + theme_classic() +
  facet_grid(~ c, switch = "x") +
  theme(strip.text = element_text(size=10), strip.placement = "outside",
        strip.background = element_blank(), legend.position="none",
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))

topt_plot <- 
  traits_filtered_ci_df %>%
  filter(treatment %in% c("A", "C", "E", "F", "H", "I", "K", "L")) %>% 
  ggplot(., aes(x = np, y = topt_mean, color = treatment)) +
  geom_point() + 
  geom_errorbar(aes(ymin = topt_lower, ymax = topt_upper, color = treatment), 
                width = 0.2, position=position_dodge(width=1)) +
  scale_colour_manual(values = c("#000000", # High N,P; enriched C
                                 "#a9a9a9", # High N,P; non-enriched C
                                 "#000075", # P-limited; enriched C
                                 "#3cb44b", # P-limited; non-enriched C
                                 "#800000", # N-limited; enriched C
                                 "#f58231", # N-limited; non-enriched C
                                 "#ffe119", # Low N,P; enriched C
                                 "#dcbeff"  # Low N,P; non-enriched C
  )) + 
  ylab(bquote(T[opt])) + labs(color = "Treatment", x = NULL) + theme_classic() +
  facet_grid(~ c, switch = "x") +
  theme(strip.text = element_text(size=10), strip.placement = "outside",
        strip.background = element_blank(), legend.position="none",
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))

rmax_plot <- 
  traits_filtered_ci_df %>%
  filter(treatment %in% c("A", "C", "E", "F", "H", "I", "K", "L")) %>% 
  ggplot(., aes(x = np, y = rmax_mean, color = treatment)) +
  geom_point() + 
  geom_errorbar(aes(ymin = rmax_lower, ymax = rmax_upper, color = treatment), 
                width = 0.2, position=position_dodge(width=1)) +
  scale_colour_manual(values = c("#000000", # High N,P; enriched C
                                 "#a9a9a9", # High N,P; non-enriched C
                                 "#000075", # P-limited; enriched C
                                 "#3cb44b", # P-limited; non-enriched C
                                 "#800000", # N-limited; enriched C
                                 "#f58231", # N-limited; non-enriched C
                                 "#ffe119", # Low N,P; enriched C
                                 "#dcbeff"  # Low N,P; non-enriched C
  )) + 
  ylab(expression(mu[max])) + labs(color = "Treatment", x = NULL) + theme_classic() +
  facet_grid(~ c, switch = "x") +
  theme(strip.text = element_text(size=10), strip.placement = "outside",
        strip.background = element_blank(), legend.position="none",
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))

ea_plot <- 
  traits_filtered_ci_df %>% 
  filter(treatment %in% c("A", "C", "E", "F", "H", "I", "K", "L")) %>% 
  ggplot(., aes(x = np, y = ea_mean, color = treatment)) +
  geom_point() + 
  geom_errorbar(aes(ymin = ea_lower, ymax = ea_upper, color = treatment), 
                width = 0.2, position=position_dodge(width=1)) +
  scale_colour_manual(values = c("#000000", # High N,P; enriched C
                                 "#a9a9a9", # High N,P; non-enriched C
                                 "#000075", # P-limited; enriched C
                                 "#3cb44b", # P-limited; non-enriched C
                                 "#800000", # N-limited; enriched C
                                 "#f58231", # N-limited; non-enriched C
                                 "#ffe119", # Low N,P; enriched C
                                 "#dcbeff"  # Low N,P; non-enriched C
  )) + 
  ylab(bquote(E[a])) + labs(color = "Treatment", x = NULL) + theme_classic() + 
  facet_grid(~ c, switch = "x") +
  theme(strip.text = element_text(size=10), strip.placement = "outside",
        strip.background = element_blank(), legend.position="none",
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))

auc_plot <- 
  traits_filtered_ci_df %>% 
  filter(treatment %in% c("A", "C", "E", "F", "H", "I", "K", "L")) %>% 
  ggplot(., aes(x = np, y = auc_mean, color = treatment)) +
  geom_point() + 
  geom_errorbar(aes(ymin = auc_lower, ymax = auc_upper, color = treatment), 
                width = 0.2, position=position_dodge(width=1)) +
  scale_colour_manual(values = c("#000000", # High N,P; enriched C
                                 "#a9a9a9", # High N,P; non-enriched C
                                 "#000075", # P-limited; enriched C
                                 "#3cb44b", # P-limited; non-enriched C
                                 "#800000", # N-limited; enriched C
                                 "#f58231", # N-limited; non-enriched C
                                 "#ffe119", # Low N,P; enriched C
                                 "#dcbeff"  # Low N,P; non-enriched C
  )) + 
  ylab("AUC") + 
  labs(color = "Treatment", x = NULL) + 
  theme_classic() + 
  facet_grid(~ c, switch = "x") +
  theme(strip.text = element_text(size=10), strip.placement = "outside",
        strip.background = element_blank(), legend.position="none",
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))






fig1_plot <- 
  ggarrange(tmin_0.2rmax_plot, tmax_0.2rmax_plot, breadth_0.2rmax_plot, 
            topt_plot, rmax_plot, auc_plot,
            legend = "none", common.legend = TRUE,
            labels = c("a)", "b)", "c)", "d)", "e)", "f)"),
            ncol = 3, nrow = 2)

ggsave(fig1_plot, dpi = 600, height = 150, width = 250, units = "mm",
       filename = here("processed_data/fig1_plot.png")
)





################################# Figure 2 #####################################

balancednp_growth_plot <- 
  nb_direct_filtered_preds %>% 
  dplyr::filter(treatment %in% c("A", "C")) %>% 
  mutate(cnp = fct_relevel(cnp, 
                           "High N,P; enriched C", "High N,P; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean, group = cnp, colour = cnp, fill = cnp)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype=0) +
  geom_line(size = 1.2) +
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Growth Rate (d'^-1~')')) + 
  scale_colour_manual(values = c("#000000", "#a9a9a9"), 
                      labels = function(x) str_wrap(x, width = 15)) + 
  scale_fill_manual(values = c("#000000", "#a9a9a9"), 
                    labels = function(x) str_wrap(x, width = 15)) + 
  theme_classic() + 
  ylim(-.2, 3.5) +
  theme(legend.title=element_blank(), legend.position = "bottom")

nlimited_growth_plot <- 
  nb_direct_filtered_preds %>% 
  dplyr::filter(treatment %in% c("A", "H", "I")) %>% 
  mutate(cnp = fct_relevel(cnp, 
                           "High N,P; enriched C", "N-limited; enriched C", 
                           "N-limited; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean, group = cnp, colour = cnp, fill = cnp)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype=0) +
  geom_line(size = 1.2) +
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Growth Rate (d'^-1~')')) + 
  scale_colour_manual(values = c("#000000", "#800000", "#f58231"), 
                      labels = function(x) str_wrap(x, width = 15)) + 
  scale_fill_manual(values = c("#000000", "#800000", "#f58231"), 
                    labels = function(x) str_wrap(x, width = 15)) + 
  theme_classic() + 
  ylim(-.2, 3.5) +
  theme(legend.title=element_blank(), legend.position = "bottom")

plimited_growth_plot <- 
  nb_direct_filtered_preds %>% 
  dplyr::filter(treatment %in% c("A", "E", "F")) %>% 
  mutate(cnp = fct_relevel(cnp, 
                           "High N,P; enriched C", "P-limited; enriched C", 
                           "P-limited; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean, group = cnp, colour = cnp, fill = cnp)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype=0) +
  geom_line(size = 1.2) +
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Growth Rate (d'^-1~')')) + 
  scale_colour_manual(values = c("#000000", "#000075", "#3cb44b"), 
                      labels = function(x) str_wrap(x, width = 15)) + 
  scale_fill_manual(values = c("#000000", "#000075", "#3cb44b"), 
                    labels = function(x) str_wrap(x, width = 15)) + 
  theme_classic() + 
  ylim(-.2, 3.5) +
  theme(legend.title=element_blank(), legend.position = "bottom")

lownp_growth_plot <- 
  nb_direct_filtered_preds %>% 
  dplyr::filter(treatment %in% c("A", "K", "L")) %>% 
  mutate(cnp = fct_relevel(cnp, 
                           "High N,P; enriched C", "Low N,P; enriched C", "Low N,P; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean, group = cnp, colour = cnp, fill = cnp)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype=0) +
  geom_line(size = 1.2) +
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Growth Rate (d'^-1~')')) + 
  scale_colour_manual(values = c("#000000", "#ffe119", "#dcbeff"), 
                      labels = function(x) str_wrap(x, width = 15)) + 
  scale_fill_manual(values = c("#000000", "#ffe119", "#dcbeff"), 
                    labels = function(x) str_wrap(x, width = 15)) + 
  theme_classic() + 
  ylim(-.2, 3.5) +
  theme(legend.title=element_blank(), legend.position = "bottom")




fig2_plot <- 
  ggarrange(balancednp_growth_plot, nlimited_growth_plot, 
            plimited_growth_plot, lownp_growth_plot,
            common.legend = FALSE,
            labels = c("a)", "b)", "c)", "d)"),
            ncol = 2, nrow = 2)


ggsave(fig2_plot, dpi = 600, height = 200, width = 200, units = "mm",
       filename = here("processed_data/fig2_filtered_plot.png"))  





################################# Figure 3 #####################################
balancednp_cellsize_plot <- 
  cellsize_summarized_df %>% 
  dplyr::filter(treatment %in% c("A", "C")) %>% 
  dplyr::filter(date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, 
                           "High N,P; enriched C", "High N,P; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_cellvol, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_cellvol-se_cellvol, ymax= mean_cellvol+se_cellvol), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Single Cell Volume ('*mu*'m'^3~')')) + 
  scale_colour_manual(values = c("#000000", "#a9a9a9"), 
                      labels = function(x) str_wrap(x, width = 15)) + 
  scale_fill_manual(values = c("#000000", "#a9a9a9"), 
                    labels = function(x) str_wrap(x, width = 15)) + 
  theme_classic() + 
  theme(legend.title=element_blank(), legend.position = "bottom")  

nlimited_cellsize_plot <- 
  cellsize_summarized_df %>% 
  dplyr::filter(treatment %in% c("A", "H", "I")) %>% 
  dplyr::filter(date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, 
                           "High N,P; enriched C", "N-limited; enriched C", 
                           "N-limited; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_cellvol, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_cellvol-se_cellvol, ymax= mean_cellvol+se_cellvol), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Single Cell Volume ('*mu*'m'^3~')')) + 
  scale_colour_manual(values = c("#000000", "#800000", "#f58231"), 
                      labels = function(x) str_wrap(x, width = 15)) + 
  scale_fill_manual(values = c("#000000", "#800000", "#f58231"), 
                    labels = function(x) str_wrap(x, width = 15)) + 
  theme_classic() + 
  theme(legend.title=element_blank(), legend.position = "bottom")

plimited_cellsize_plot <- 
  cellsize_summarized_df %>% 
  dplyr::filter(treatment %in% c("A", "E", "F")) %>%
  dplyr::filter(date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, 
                           "High N,P; enriched C", "P-limited; enriched C", 
                           "P-limited; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_cellvol, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_cellvol-se_cellvol, ymax= mean_cellvol+se_cellvol), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Single Cell Volume ('*mu*'m'^3~')')) + 
  scale_colour_manual(values = c("#000000", "#000075", "#3cb44b"), 
                      labels = function(x) str_wrap(x, width = 15)) + 
  scale_fill_manual(values = c("#000000", "#000075", "#3cb44b"), 
                    labels = function(x) str_wrap(x, width = 15)) + 
  theme_classic() + 
  theme(legend.title=element_blank(), legend.position = "bottom")

lownp_cellsize_plot <- 
  cellsize_summarized_df %>% 
  dplyr::filter(treatment %in% c("A", "K", "L")) %>%
  dplyr::filter(date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, 
                           "High N,P; enriched C", "Low N,P; enriched C", "Low N,P; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_cellvol, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_cellvol-se_cellvol, ymax= mean_cellvol+se_cellvol), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Single Cell Volume ('*mu*'m'^3~')')) + 
  scale_colour_manual(values = c("#000000", "#ffe119", "#dcbeff"), 
                      labels = function(x) str_wrap(x, width = 15)) + 
  scale_fill_manual(values = c("#000000", "#ffe119", "#dcbeff"), 
                    labels = function(x) str_wrap(x, width = 15)) + 
  theme_classic() + 
  theme(legend.title=element_blank(), legend.position = "bottom")




fig3_plot <- 
  ggarrange(balancednp_cellsize_plot, nlimited_cellsize_plot, 
            plimited_cellsize_plot, lownp_cellsize_plot,
            common.legend = FALSE,
            labels = c("a)", "b)", "c)", "d)"),
            ncol = 2, nrow = 2)

ggsave(fig3_plot, dpi = 600, height = 200, width = 200, units = "mm",
       filename = here("processed_data/fig3_plot.png"))




################################# Figure 4 #####################################

balancednp_lipid_plot <- 
  lipid_data_summarized %>% 
  dplyr::filter(treatment %in% c("A", "C")) %>% 
  dplyr::filter(date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, 
                           "High N,P; enriched C", "High N,P; Non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_lipid, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_lipid-se_lipid, ymax= mean_lipid+se_lipid), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab('Lipid content') + 
  scale_colour_manual(values = c("#000000", "#a9a9a9"), 
                      labels = function(x) str_wrap(x, width = 15)) + 
  scale_fill_manual(values = c("#000000", "#a9a9a9"), 
                    labels = function(x) str_wrap(x, width = 15)) + 
  theme_classic() + 
  theme(legend.title=element_blank(), legend.position = "bottom")

nlimited_lipid_plot <- 
  lipid_data_summarized %>% 
  dplyr::filter(treatment %in% c("A", "H", "I")) %>% 
  dplyr::filter(date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, 
                           "High N,P; enriched C", "N-limited; enriched C", 
                           "N-limited; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_lipid, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_lipid-se_lipid, ymax= mean_lipid+se_lipid), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab('Lipid content') + 
  scale_colour_manual(values = c("#000000", "#800000", "#f58231"), 
                      labels = function(x) str_wrap(x, width = 15)) + 
  scale_fill_manual(values = c("#000000", "#800000", "#f58231"), 
                    labels = function(x) str_wrap(x, width = 15)) + 
  theme_classic() + 
  theme(legend.title=element_blank(), legend.position = "bottom")

plimited_lipid_plot <- 
  lipid_data_summarized %>% 
  dplyr::filter(treatment %in% c("A", "E", "F")) %>%
  dplyr::filter(date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, 
                           "High N,P; enriched C", "P-limited; enriched C", 
                           "P-limited; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_lipid, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_lipid-se_lipid, ymax= mean_lipid+se_lipid), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab('Lipid content') +
  scale_colour_manual(values = c("#000000", "#000075", "#3cb44b"), 
                      labels = function(x) str_wrap(x, width = 15)) + 
  scale_fill_manual(values = c("#000000", "#000075", "#3cb44b"), 
                    labels = function(x) str_wrap(x, width = 15)) + 
  theme_classic() + 
  theme(legend.title=element_blank(), legend.position = "bottom")

lownp_lipid_plot <- 
  lipid_data_summarized %>% 
  dplyr::filter(treatment %in% c("A", "K", "L")) %>%
  dplyr::filter(date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, 
                           "High N,P; enriched C", "Low N,P; enriched C", "Low N,P; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_lipid, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_lipid-se_lipid, ymax= mean_lipid+se_lipid), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab('Lipid content') +
  scale_colour_manual(values = c("#000000", "#ffe119", "#dcbeff"), 
                      labels = function(x) str_wrap(x, width = 15)) + 
  scale_fill_manual(values = c("#000000", "#ffe119", "#dcbeff"), 
                    labels = function(x) str_wrap(x, width = 15)) + 
  theme_classic() + 
  theme(legend.title=element_blank(), legend.position = "bottom")



fig4_plot <- 
  ggarrange(balancednp_lipid_plot, nlimited_lipid_plot, 
            plimited_lipid_plot, lownp_lipid_plot,
            common.legend = FALSE,
            labels = c("a)", "b)", "c)", "d)"),
            ncol = 2, nrow = 2)

ggsave(fig4_plot, dpi = 600, height = 200, width = 200, units = "mm",
       filename = here("processed_data/fig4_plot.png")) 






############################ Supplementary Figures #############################


################################ CELL VOLUME ###################################

### N-limited comp

# "#800000", # N-limited; enriched C
# "#f58231", # N-limited; non-enriched C

high_cnlimited_volume_plot <- 
  cellvol_sum_df %>% 
  dplyr::filter(treatment %in% c("A", "I") & date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, "High N,P; enriched C", "N-limited; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_cellvol, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_cellvol-se_cellvol, ymax= mean_cellvol+se_cellvol), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Single Cell Volume ('*mu*'m'^3~')')) + 
  scale_colour_manual(values = c("#000000", "#f58231")) + 
  scale_fill_manual(values = c("#000000", "#f58231")) +  
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  annotate("text", label = "*", x = 6.72, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 18.44, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 24, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 29.82, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 36.49, y = 1200, size = 10, colour = "#000000") +
  annotate("text", label = "*", x = 38.74, y = 1200, size = 10, colour = "#000000") +
  annotate("text", label = "*", x = 41.1, y = 1200, size = 10, colour = "#000000")

n_cnlimited_volume_plot <- 
  cellvol_sum_df %>% 
  dplyr::filter(treatment %in% c("H", "I") & date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, "N-limited; enriched C", "N-limited; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_cellvol, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_cellvol-se_cellvol, ymax= mean_cellvol+se_cellvol), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Single Cell Volume ('*mu*'m'^3~')')) + 
  scale_colour_manual(values = c("#800000", "#f58231")) + 
  scale_fill_manual(values = c("#800000", "#f58231")) + 
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) +
  
  annotate("text", label = "*", x = 6.72, y = 500, size = 10, colour = "#800000") + 
  annotate("text", label = "*", x = 18.44, y = 500, size = 10, colour = "#800000") +
  # annotate("text", label = "*", x = 24, y = 500, size = 10, colour = "#800000") + 
  annotate("text", label = "*", x = 29.82, y = 500, size = 10, colour = "#f58231") + 
  annotate("text", label = "*", x = 36.49, y = 500, size = 10, colour = "#800000") +
  annotate("text", label = "*", x = 38.74, y = 500, size = 10, colour = "#800000") + 
  annotate("text", label = "*", x = 41.1, y = 500, size = 10, colour = "#800000")

high_nlimited_volume_plot <- 
  cellvol_sum_df %>%
  dplyr::filter(treatment %in% c("A", "H") & date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, "High N,P; enriched C", "N-limited; enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_cellvol, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_cellvol-se_cellvol, ymax= mean_cellvol+se_cellvol), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Single Cell Volume ('*mu*'m'^3~')')) + 
  scale_colour_manual(values = c("#000000", "#800000")) + 
  scale_fill_manual(values = c("#000000", "#800000")) + 
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) +
  
  annotate("text", label = "*", x = 6.72, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 18.44, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 29.82, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 36.49, y = 1200, size = 10, colour = "#000000") +
  annotate("text", label = "*", x = 38.74, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 41.1, y = 1200, size = 10, colour = "#000000")

### P-limited comp

# "#000075", # P-limited; enriched C
# "#4363d8", # P-limited; non-enriched C

high_plimited_volume_plot <- 
  cellvol_sum_df %>% 
  dplyr::filter(treatment %in% c("A", "E") & date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, "High N,P; enriched C", "P-limited; enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_cellvol, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_cellvol-se_cellvol, ymax= mean_cellvol+se_cellvol), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Single Cell Volume ('*mu*'m'^3~')')) + 
  scale_colour_manual(values = c("#000000", "#000075")) + 
  scale_fill_manual(values = c("#000000", "#000075")) + 
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  annotate("text", label = "*", x = 6.72, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 18.44, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 24, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 29.82, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 36.49, y = 1200, size = 10, colour = "#000000") +
  annotate("text", label = "*", x = 38.74, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 41.1, y = 1200, size = 10, colour = "#000000")

high_cplimited_volume_plot <- 
  cellvol_sum_df %>% 
  dplyr::filter(treatment %in% c("A", "F") & date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, "High N,P; enriched C", "P-limited; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_cellvol, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_cellvol-se_cellvol, ymax= mean_cellvol+se_cellvol), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Single Cell Volume ('*mu*'m'^3~')')) + 
  ylab(bquote('Growth Rate (d'^-1~')')) + 
  scale_colour_manual(values = c("#000000", "#3cb44b")) + 
  scale_fill_manual(values = c("#000000", "#3cb44b")) + 
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  annotate("text", label = "*", x = 6.72, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 18.44, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 24, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 29.82, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 36.49, y = 1200, size = 10, colour = "#000000") +
  annotate("text", label = "*", x = 38.74, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 41.1, y = 1200, size = 10, colour = "#000000")

p_cplimited_volume_plot <- 
  cellvol_sum_df %>% 
  dplyr::filter(treatment %in% c("E", "F") & date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, "P-limited; enriched C", "P-limited; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_cellvol, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_cellvol-se_cellvol, ymax= mean_cellvol+se_cellvol), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Single Cell Volume ('*mu*'m'^3~')')) + 
  ylab(bquote('Growth Rate (d'^-1~')')) + 
  scale_colour_manual(values = c("#000075", "#3cb44b")) + 
  scale_fill_manual(values = c("#000075", "#3cb44b")) + 
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  annotate("text", label = "*", x = 6.72, y = 500, size = 10, colour = "#000075") + 
  annotate("text", label = "*", x = 18.44, y = 500, size = 10, colour = "#000075") +
  annotate("text", label = "*", x = 24, y = 500, size = 10, colour = "#000075") + 
  annotate("text", label = "*", x = 29.82, y = 500, size = 10, colour = "#000075") + 
  annotate("text", label = "*", x = 36.49, y = 500, size = 10, colour = "#000075") +
  annotate("text", label = "*", x = 38.74, y = 500, size = 10, colour = "#000075")

### high NP comp

# "#000000", # High N,P; enriched C (A)
# "#a9a9a9", # High N,P; non-enriched C (B)

high_climited_volume_plot <- 
  cellvol_sum_df %>% 
  dplyr::filter(treatment %in% c("A", "C") & date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, "High N,P; enriched C", "High N,P; Non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_cellvol, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_cellvol-se_cellvol, ymax= mean_cellvol+se_cellvol), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Single Cell Volume ('*mu*'m'^3~')')) + 
  scale_colour_manual(values = c("#000000", "#a9a9a9")) + 
  scale_fill_manual(values = c("#000000", "#a9a9a9")) + 
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) +
  
  annotate("text", label = "*", x = 6.72, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 24, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 29.82, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 36.49, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 38.74, y = 1200, size = 10, colour = "#000000") 


### low NP comp

# "#ffe119", # Low N,P; enriched C (J)
# "#dcbeff"  # Low N,P; non-enriched C (L)

highNP_lowNPnoneC_volume_plot <- 
  cellvol_sum_df %>% 
  dplyr::filter(treatment %in% c("A", "L") & date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, 
                           "High N,P; enriched C", "Low N,P; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_cellvol, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_cellvol-se_cellvol, ymax= mean_cellvol+se_cellvol), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Single Cell Volume ('*mu*'m'^3~')')) + 
  scale_colour_manual(values = c("#000000", "#dcbeff")) + 
  scale_fill_manual(values = c("#000000", "#dcbeff")) + 
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  annotate("text", label = "*", x = 6.72, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 18.44, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 24, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 29.82, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 36.49, y = 1200, size = 10, colour = "#000000") +
  annotate("text", label = "*", x = 38.74, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 41.1, y = 1200, size = 10, colour = "#000000")


highNP_lowNPeC_volume_plot <- 
  cellvol_sum_df %>% 
  dplyr::filter(treatment %in% c("A", "K") & date == "2026-02-06") %>%
  mutate(cnp = fct_relevel(cnp, "High N,P; enriched C", "Low N,P; enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_cellvol, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_cellvol-se_cellvol, ymax= mean_cellvol+se_cellvol), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Single Cell Volume ('*mu*'m'^3~')')) + 
  scale_colour_manual(values = c("#000000", "#ffe119")) + 
  scale_fill_manual(values = c("#000000", "#ffe119")) + 
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  annotate("text", label = "*", x = 6.72, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 18.44, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 24, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 29.82, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 36.49, y = 1200, size = 10, colour = "#000000") +
  annotate("text", label = "*", x = 38.74, y = 1200, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 41.1, y = 1200, size = 10, colour = "#000000")


lowNP_volume_plot <- 
  cellvol_sum_df %>% 
  dplyr::filter(treatment %in% c("K", "L") & date == "2026-02-06") %>%
  mutate(cnp = fct_relevel(cnp, "Low N,P; enriched C", "Low N,P; nonenriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_cellvol, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_cellvol-se_cellvol, ymax= mean_cellvol+se_cellvol), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Single Cell Volume ('*mu*'m'^3~')')) + 
  scale_colour_manual(values = c("#ffe119", "#dcbeff")) + 
  scale_fill_manual(values = c("#ffe119", "#dcbeff")) + 
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  annotate("text", label = "*", x = 6.72, y = 300, size = 10, colour = "#ffe119") + 
  annotate("text", label = "*", x = 18.44, y = 300, size = 10, colour = "#ffe119") + 
  annotate("text", label = "*", x = 24, y = 300, size = 10, colour = "#ffe119") + 
  annotate("text", label = "*", x = 29.82, y = 300, size = 10, colour = "#ffe119") + 
  annotate("text", label = "*", x = 36.49, y = 300, size = 10, colour = "#ffe119") +
  annotate("text", label = "*", x = 38.74, y = 300, size = 10, colour = "#ffe119") + 
  annotate("text", label = "*", x = 41.1, y = 300, size = 10, colour = "#ffe119")




highNP_volume_suppl_plot <- 
  ggarrange(high_climited_volume_plot, # high
            common.legend = FALSE,
            labels = c("a)"),
            ncol = 1, nrow = 1)

ggsave(highNP_volume_suppl_plot, dpi = 600, height = 125, width = 125, units = "mm",
       filename = here("processed_data/highNP_volume_suppl_plot.png")
)

limitedN_volume_suppl_plot <- 
  ggarrange(high_nlimited_volume_plot, high_cnlimited_volume_plot, n_cnlimited_volume_plot, # N-limited 
            common.legend = FALSE,
            labels = c("a)", "b)", "c)"),
            ncol = 3, nrow = 1)

ggsave(limitedN_volume_suppl_plot, dpi = 600, height = 125, width = 300, units = "mm",
       filename = here("processed_data/limitedN_volume_suppl_plot.png")
)

limitedP_volume_suppl_plot <-   
  ggarrange(high_plimited_volume_plot, high_cplimited_volume_plot, p_cplimited_volume_plot, # P-limited
            common.legend = FALSE,
            labels = c("a)", "b)", "c)"),
            ncol = 3, nrow = 1)

ggsave(limitedP_volume_suppl_plot, dpi = 600, height = 125, width = 300, units = "mm",
       filename = here("processed_data/limitedP_volume_suppl_plot.png")
)

lowNP_volume_suppl_plot <-   
  ggarrange(highNP_lowNPnoneC_volume_plot, highNP_lowNPeC_volume_plot, lowNP_volume_plot, # low 
            common.legend = FALSE,
            labels = c("a)", "b)", "c)"),
            ncol = 3, nrow = 1)

ggsave(lowNP_volume_suppl_plot, dpi = 600, height = 125, width = 300, units = "mm",
       filename = here("processed_data/lowNP_volume_suppl_plot.png")
)




############################### LIPID CONTENT ##################################

### N-limited comp

high_cnlimited_lipid_plot <- 
  lipid_data_summarized %>% 
  dplyr::filter(treatment %in% c("A", "H") & date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, "High N,P; enriched C", "N-limited; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_lipid, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_lipid-se_lipid, ymax= mean_lipid+se_lipid), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Lipid content (RFU)')) + 
  scale_colour_manual(values = c("#000000", "#f58231")) + 
  scale_fill_manual(values = c("#000000", "#f58231")) +  
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  annotate("text", label = "*", x = 6.72, y = 1000, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 18.44, y = 1000, size = 10, colour = "#f58231") + 
  annotate("text", label = "*", x = 29.82, y = 1000, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 36.49, y = 1000, size = 10, colour = "#000000") +
  annotate("text", label = "*", x = 41.1, y = 1000, size = 10, colour = "#000000")

n_cnlimited_lipid_plot <- 
  lipid_data_summarized %>% 
  dplyr::filter(treatment %in% c("H", "I") & date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, "N-limited; enriched C", "N-limited; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_lipid, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_lipid-se_lipid, ymax= mean_lipid+se_lipid), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Lipid content (RFU)')) + 
  scale_colour_manual(values = c("#800000", "#f58231")) + 
  scale_fill_manual(values = c("#800000", "#f58231")) + 
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) +
  
  annotate("text", label = "*", x = 6.72, y = 700, size = 10, colour = "#800000") + 
  annotate("text", label = "*", x = 18.44, y = 700, size = 10, colour = "#f58231") +
  annotate("text", label = "*", x = 38.74, y = 700, size = 10, colour = "#800000") + 
  annotate("text", label = "*", x = 41.1, y = 700, size = 10, colour = "#800000")

high_nlimited_lipid_plot <- 
  lipid_data_summarized %>% 
  dplyr::filter(treatment %in% c("A", "H") & date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, "High N,P; enriched C", "N-limited; enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_lipid, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_lipid-se_lipid, ymax= mean_lipid+se_lipid), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Lipid content (RFU)')) + 
  scale_colour_manual(values = c("#000000", "#800000")) + 
  scale_fill_manual(values = c("#000000", "#800000")) + 
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) +
  
  annotate("text", label = "*", x = 6.72, y = 1000, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 18.44, y = 1000, size = 10, colour = "#800000") +
  annotate("text", label = "*", x = 29.82, y = 1000, size = 10, colour = "#000000") +
  annotate("text", label = "*", x = 36.49, y = 1000, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 41.1, y = 1000, size = 10, colour = "#000000") 

### P-limited comp

high_plimited_lipid_plot <- 
  lipid_data_summarized %>% 
  dplyr::filter(treatment %in% c("A", "E") & date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, "High N,P; enriched C", "P-limited; enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_lipid, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_lipid-se_lipid, ymax= mean_lipid+se_lipid), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Lipid content (RFU)')) + 
  scale_colour_manual(values = c("#000000", "#000075")) + 
  scale_fill_manual(values = c("#000000", "#000075")) + 
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  annotate("text", label = "*", x = 6.72, y = 1250, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 18.44, y = 1250, size = 10, colour = "#000075") + 
  annotate("text", label = "*", x = 41.1, y = 1250, size = 10, colour = "#000000")

high_cplimited_lipid_plot <- 
  lipid_data_summarized %>% 
  dplyr::filter(treatment %in% c("A", "F") & date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, "High N,P; enriched C", "P-limited; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_lipid, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_lipid-se_lipid, ymax= mean_lipid+se_lipid), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Lipid content (RFU)')) + 
  ylab(bquote('Growth Rate (d'^-1~')')) + 
  scale_colour_manual(values = c("#000000", "#3cb44b")) + 
  scale_fill_manual(values = c("#000000", "#3cb44b")) + 
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  annotate("text", label = "*", x = 6.72, y = 1000, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 18.44, y = 1000, size = 10, colour = "#3cb44b") + 
  annotate("text", label = "*", x = 41.1, y = 1000, size = 10, colour = "#000000")

p_cplimited_lipid_plot <- 
  lipid_data_summarized %>% 
  dplyr::filter(treatment %in% c("E", "F") & date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, "P-limited; enriched C", "P-limited; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_lipid, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_lipid-se_lipid, ymax= mean_lipid+se_lipid), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Lipid content (RFU)')) + 
  ylab(bquote('Growth Rate (d'^-1~')')) + 
  scale_colour_manual(values = c("#000075", "#3cb44b")) + 
  scale_fill_manual(values = c("#000075", "#3cb44b")) + 
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  )


### high NP comp

lipid_data_summarized <- 
  lipid_sum_df %>% 
  dplyr::filter(treatment %in% c("A", "C") & date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, "High N,P; enriched C", "High N,P; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_lipid, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_lipid-se_lipid, ymax= mean_lipid+se_lipid), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Lipid content (RFU)')) + 
  scale_colour_manual(values = c("#000000", "#a9a9a9")) + 
  scale_fill_manual(values = c("#000000", "#a9a9a9")) + 
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) +
  
  annotate("text", label = "*", x = 6.72, y = 1100, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 18.44, y = 1100, size = 10, colour = "#a9a9a9") + 
  annotate("text", label = "*", x = 24, y = 1100, size = 10, colour = "#a9a9a9") 

### low NP comp

highNP_lowNPnoneC_lipid_plot <- 
  lipid_data_summarized %>% 
  dplyr::filter(treatment %in% c("A", "L") & date == "2026-02-06") %>% 
  mutate(cnp = fct_relevel(cnp, "High N,P; enriched C", "Low N,P; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_lipid, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_lipid-se_lipid, ymax= mean_lipid+se_lipid), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Lipid content (RFU)')) + 
  scale_colour_manual(values = c("#000000", "#dcbeff")) + 
  scale_fill_manual(values = c("#000000", "#dcbeff")) + 
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  annotate("text", label = "*", x = 6.72, y = 1300, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 18.44, y = 1300, size = 10, colour = "#dcbeff") + 
  annotate("text", label = "*", x = 36.49, y = 1300, size = 10, colour = "#000000") 

highNP_lowNPeC_lipid_plot <- 
  lipid_data_summarized %>% 
  dplyr::filter(treatment %in% c("A", "K") & date == "2026-02-06") %>%
  mutate(cnp = fct_relevel(cnp, "High N,P; enriched C", "Low N,P; enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_lipid, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_lipid-se_lipid, ymax= mean_lipid+se_lipid), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Lipid content (RFU)')) + 
  scale_colour_manual(values = c("#000000", "#ffe119")) + 
  scale_fill_manual(values = c("#000000", "#ffe119")) + 
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  annotate("text", label = "*", x = 6.72, y = 950, size = 10, colour = "#000000") + 
  annotate("text", label = "*", x = 18.44, y = 950, size = 10, colour = "#ffe119") + 
  annotate("text", label = "*", x = 36.49, y = 950, size = 10, colour = "#000000") +
  annotate("text", label = "*", x = 41.1, y = 950, size = 10, colour = "#000000")

lowNP_lipid_plot <- 
  lipid_data_summarized %>% 
  dplyr::filter(treatment %in% c("K", "L") & date == "2026-02-06") %>%
  mutate(cnp = fct_relevel(cnp, "Low N,P; enriched C", "Low N,P; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean_lipid, group = cnp, colour = cnp, fill = cnp)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin= mean_lipid-se_lipid, ymax= mean_lipid+se_lipid), width=1.2) + 
  labs(x = "Temperature (°C)", colour = "Limitation Type", fill = "Limitation Type") +
  ylab(bquote('Lipid content (RFU)')) + 
  scale_colour_manual(values = c("#ffe119", "#dcbeff")) + 
  scale_fill_manual(values = c("#ffe119", "#dcbeff")) + 
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  annotate("text", label = "*", x = 18.44, y = 1350, size = 10, colour = "#dcbeff") + 
  annotate("text", label = "*", x = 36.49, y = 1350, size = 10, colour = "#dcbeff")


highNP_lipid_suppl_plot <- 
  ggarrange(high_climited_lipid_plot, # high 
            common.legend = FALSE,
            labels = c("a)"),
            ncol = 1, nrow = 1)

ggsave(highNP_lipid_suppl_plot, dpi = 600, height = 125, width = 125, units = "mm",
       filename = here("processed_data/highNP_lipid_suppl_plot.png")
)

limitedN_lipid_suppl_plot <- 
  ggarrange(high_nlimited_lipid_plot, high_cnlimited_lipid_plot, n_cnlimited_lipid_plot, # N-limited 
            common.legend = FALSE,
            labels = c("a)", "b)", "c)"),
            ncol = 3, nrow = 1)

ggsave(limitedN_lipid_suppl_plot, dpi = 600, height = 125, width = 300, units = "mm",
       filename = here("processed_data/limitedN_lipid_suppl_plot.png")
)

limitedP_lipid_suppl_plot <- 
  ggarrange(high_plimited_lipid_plot, high_cplimited_lipid_plot, p_cplimited_lipid_plot, # P-limited
            common.legend = FALSE,
            labels = c("a)", "b)", "c)"),
            ncol = 3, nrow = 1)

ggsave(limitedP_lipid_suppl_plot, dpi = 600, height = 125, width = 300, units = "mm",
       filename = here("processed_data/limitedP_lipid_suppl_plot.png")
)

lowNP_lipid_suppl_plot <- 
  ggarrange(highNP_lowNPeC_lipid_plot,highNP_lowNPnoneC_lipid_plot, lowNP_lipid_plot, # low 
            common.legend = FALSE,
            labels = c("a)", "b)", "c)"),
            ncol = 3, nrow = 1)

ggsave(lowNP_lipid_suppl_plot, dpi = 600, height = 125, width = 300, units = "mm",
       filename = here("processed_data/lowNP_lipid_suppl_plot.png")
)





################################### GROWTH #####################################

### N-limited comp 
# "#800000", # N-limited; enriched C
# "#f58231", # N-limited; non-enriched C

high_cnlimited_growth_plot <- 
  nb_direct_filtered_preds %>% 
  dplyr::filter(treatment %in% c("A", "I")) %>% 
  mutate(cnp = fct_relevel(cnp, "High N,P; enriched C", "N-limited; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean, group = cnp,
                colour = cnp, fill = cnp)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype=0) +
  geom_line(size = 1.2) +
  labs(
    x = "Temperature (°C)",
    colour = "Limitation Type",
    fill = "Limitation Type") +
  ylab(bquote('Growth Rate (d'^-1~')')) +  
  scale_colour_manual(values = c("#000000", "#f58231")) + 
  scale_fill_manual(values = c("#000000", "#f58231")) +
  ylim(-.2, 4.5) +
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  # comparing C,N-limit and high C,N,P growth
  geom_segment(aes(x = 11.06, y = 4.5, xend = 40.33, yend = 4.5), size = 2, colour = "#000000") 

n_cnlimited_growth_plot <- 
  nb_direct_filtered_preds %>% 
  dplyr::filter(treatment %in% c("H", "I")) %>% 
  mutate(cnp = fct_relevel(cnp, 
                           "N-limited; enriched C", "N-limited; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean, group = cnp,
                colour = cnp, fill = cnp)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype=0) +
  geom_line(size = 1.2) +
  labs(
    # title = "Effects of carbon under nitrogen limitation",
    x = "Temperature (°C)",
    colour = "Limitation Type",
    fill = "Limitation Type") +
  ylab(bquote('Growth Rate (d'^-1~')')) + 
  scale_colour_manual(values = c("#800000", "#f58231")) + 
  scale_fill_manual(values = c("#800000", "#f58231")) + 
  ylim(-.2, 4.5) +
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  # comparing N-limit and C,N-limit growth 
  geom_segment(aes(x = 14.03, y = 4.5, xend = 39.48, yend = 4.5), size = 2, colour = "#800000") 

high_nlimited_growth_plot <- 
  nb_direct_filtered_preds %>% 
  dplyr::filter(treatment %in% c("A", "H")) %>% 
  mutate(cnp = fct_relevel(cnp, "High N,P; enriched C", "N-limited; enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean, group = cnp,
                colour = cnp, fill = cnp)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype=0) +
  geom_line(size = 1.2) +
  labs(
    x = "Temperature (°C)",
    colour = "Limitation Type",
    fill = "Limitation Type") +
  ylab(bquote('Growth Rate (d'^-1~')')) + 
  scale_colour_manual(values = c("#000000", "#800000")) + 
  scale_fill_manual(values = c("#000000", "#800000")) + 
  ylim(-.2, 4.5) +
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  # comparing N-limit and high C,N,P growth
  geom_segment(aes(x = 25.06, y = 4.5, xend = 40.76, yend = 4.5), size = 2, colour = "#000000") # 25.06 – 40.76

### P-limited comp
# "#000075", # P-limited; enriched C
# "#4363d8", # P-limited; non-enriched C

high_plimited_growth_plot <- 
  nb_direct_filtered_preds %>% 
  dplyr::filter(treatment %in% c("A", "E")) %>% 
  mutate(cnp = fct_relevel(cnp, "High N,P; enriched C", "P-limited; enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean, group = cnp,
                colour = cnp, fill = cnp)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype=0) +
  geom_line(size = 1.2) +
  labs(
    # title = "Effects of carbon under phosphorus limitation",
    x = "Temperature (°C)",
    colour = "Limitation Type",
    fill = "Limitation Type") +
  ylab(bquote('Growth Rate (d'^-1~')')) + 
  scale_colour_manual(values = c("#000000", "#000075")) + 
  scale_fill_manual(values = c("#000000", "#000075")) + 
  ylim(-.2, 4.5) +
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  # comparing P-limit and high C,N,P growth
  geom_segment(aes(x = 13.18, y = 4.5, xend = 40.33, yend = 4.5), size = 2, colour = "#000000") # 13.18 – 40.33

high_cplimited_growth_plot <- 
  nb_direct_filtered_preds %>% 
  dplyr::filter(treatment %in% c("A", "F")) %>% 
  mutate(cnp = fct_relevel(cnp, "High N,P; enriched C", "P-limited; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean, group = cnp,
                colour = cnp, fill = cnp)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype=0) +
  geom_line(size = 1.2) +
  labs(
    # title = "Effects of carbon under phosphorus limitation",
    x = "Temperature (°C)",
    colour = "Limitation Type",
    fill = "Limitation Type") +
  ylab(bquote('Growth Rate (d'^-1~')')) + 
  scale_colour_manual(values = c("#000000", "#3cb44b")) + 
  scale_fill_manual(values = c("#000000", "#3cb44b")) + 
  ylim(-.2, 4.5) +
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  # comparing C,P-limit and high C,N,P growth
  geom_segment(aes(x = 14.88, y = 4.5, xend = 40.76, yend = 4.5), size = 2, colour = "#000000") # 14.88 – 40.76

p_cplimited_growth_plot <- 
  nb_direct_filtered_preds %>% 
  dplyr::filter(treatment %in% c("E", "F")) %>% 
  mutate(cnp = fct_relevel(cnp, "P-limited; enriched C", "P-limited; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean, group = cnp,
                colour = cnp, fill = cnp)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype=0) +
  geom_line(size = 1.2) +
  labs(
    # title = "Effects of carbon under phosphorus limitation",
    x = "Temperature (°C)",
    colour = "Limitation Type",
    fill = "Limitation Type") +
  ylab(bquote('Growth Rate (d'^-1~')')) + 
  scale_colour_manual(values = c("#000075", "#3cb44b")) + 
  scale_fill_manual(values = c("#000075", "#3cb44b")) + 
  ylim(-.2, 4.5) +
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  # comparing P-limit and C,P-limit growth 
  geom_segment(aes(x = 24.21, y = 4.5, xend = 40.33, yend = 4.5), size = 2, colour = "#000075") # 24.21 – 40.33


### high NP comp 
# "#000000", # High N,P; enriched C (A)
# "#a9a9a9", # High N,P; non-enriched C (B)

high_climited_growth_plot <- 
  nb_direct_filtered_preds %>% 
  dplyr::filter(treatment %in% c("A", "C")) %>% 
  mutate(cnp = fct_relevel(cnp, "High N,P; enriched C", "High N,P; Non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean, group = cnp, colour = cnp, fill = cnp)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype=0) +
  geom_line(size = 1.2) +
  labs(
    x = "Temperature (°C)",
    colour = "Limitation Type",
    fill = "Limitation Type") +
  ylab(bquote('Growth Rate (d'^-1~')')) + 
  scale_colour_manual(values = c("#000000", "#a9a9a9")) + 
  scale_fill_manual(values = c("#000000", "#a9a9a9")) +
  ylim(-.2, 4.5) +
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  # comparing high C,N,P and C-limited 
  geom_segment(aes(x = 4.27, y = 4.5, xend = 19.55, yend = 4.5), size = 2, colour = "#a9a9a9") + # 4.27 – 19.55
  geom_segment(aes(x = 22.52, y = 4.5, xend = 40.76, yend = 4.5), size = 2, colour = "#000000") # 22.52 – 40.76


### low NP comp 
# "#ffe119", # Low N,P; enriched C (J)
# "#dcbeff"  # Low N,P; non-enriched C (L)

highNP_lowNPnoneC_growth_plot <- 
  nb_direct_filtered_preds %>% 
  dplyr::filter(treatment %in% c("A", "L")) %>% 
  mutate(cnp = fct_relevel(cnp, "High N,P; enriched C", "Low N,P; non-enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean, group = cnp,
                colour = cnp, fill = cnp)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype=0) +
  geom_line(size = 1.2) +
  labs(
    x = "Temperature (°C)",
    colour = "Limitation Type",
    fill = "Limitation Type") +
  ylab(bquote('Growth Rate (d'^-1~')')) + 
  scale_colour_manual(values = c("#000000", "#dcbeff")) + 
  scale_fill_manual(values = c("#000000", "#dcbeff")) + 
  ylim(-.2, 4.5) +
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  # comparing high and low C,N,P growth
  geom_segment(aes(x = 8.94, y = 4.5, xend = 40.76, yend = 4.5), size = 2, colour = "#000000") # 8.94 – 40.76

highNP_lowNPeC_growth_plot <- 
  nb_direct_filtered_preds %>% 
  dplyr::filter(treatment %in% c("A", "K")) %>% 
  mutate(cnp = fct_relevel(cnp, "High N,P; enriched C", "Low N,P; enriched C")) %>% 
  ggplot(., aes(x = temp, y = mean, group = cnp, colour = cnp, fill = cnp)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype=0) +
  geom_line(size = 1.2) +
  labs(
    x = "Temperature (°C)",
    colour = "Limitation Type",
    fill = "Limitation Type") +
  ylab(bquote('Growth Rate (d'^-1~')')) + 
  scale_colour_manual(values = c("#000000", "#ffe119")) + 
  scale_fill_manual(values = c("#000000", "#ffe119")) + 
  ylim(-.2, 4.5) +
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  # comparing high and low N,P & enriched C growth
  geom_segment(aes(x = 13.18, y = 4.5, xend = 41.18, yend = 4.5), size = 2, colour = "#000000") # 13.18 – 41.18

lowNP_growth_plot <- 
  nb_direct_filtered_preds %>% 
  dplyr::filter(treatment %in% c("K", "L")) %>% 
  mutate(cnp = fct_relevel(cnp, "Low N,P; enriched C", "Low N,P; nonenriched C")) %>% 
  ggplot(., aes(x = temp, y = mean, group = cnp, colour = cnp, fill = cnp)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype=0) +
  geom_line(size = 1.2) +
  labs(
    x = "Temperature (°C)",
    colour = "Limitation Type",
    fill = "Limitation Type") +
  ylab(bquote('Growth Rate (d'^-1~')')) + 
  scale_colour_manual(values = c("#ffe119", "#dcbeff")) + 
  scale_fill_manual(values = c("#ffe119", "#dcbeff")) + 
  ylim(-.2, 4.5) +
  theme_classic() + 
  theme(
    legend.title=element_blank(),
    legend.position = c(.01, .935),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  ) + 
  
  # comparing high and low N,P & enriched C growth
  geom_segment(aes(x = 18.70, y = 4.5, xend = 34.39, yend = 4.5), size = 2, colour = "#ffe119") # 18.70 – 34.39





highNP_growth_suppl_plot <- 
  ggarrange(high_climited_growth_plot, # high 
            common.legend = FALSE,
            labels = c("a)"),
            ncol = 1, nrow = 1)

ggsave(highNP_growth_suppl_plot, dpi = 600, height = 125, width = 125, units = "mm",
       filename = here("processed_data/highNP_growth_suppl_plot.png")
)

limitedN_growth_suppl_plot <- 
  ggarrange(high_nlimited_growth_plot, high_cnlimited_growth_plot, n_cnlimited_growth_plot, # N-limited 
            common.legend = FALSE,
            labels = c("a)", "b)", "c)"),
            ncol = 3, nrow = 1)

ggsave(limitedN_growth_suppl_plot, dpi = 600, height = 125, width = 300, units = "mm",
       filename = here("processed_data/limitedN_growth_suppl_plot.png")
)

limitedP_growth_suppl_plot <- 
  ggarrange(high_plimited_growth_plot, high_cplimited_growth_plot, p_cplimited_growth_plot, # P-limited
            common.legend = FALSE,
            labels = c("a)", "b)", "c)"),
            ncol = 3, nrow = 1)

ggsave(limitedP_growth_suppl_plot, dpi = 600, height = 125, width = 300, units = "mm",
       filename = here("processed_data/limitedP_growth_suppl_plot.png")
)

lowNP_growth_suppl_plot <- 
  ggarrange(highNP_lowNPeC_growth_plot,highNP_lowNPnoneC_growth_plot, lowNP_growth_plot, # low 
            common.legend = FALSE,
            labels = c("a)", "b)", "c)"),
            ncol = 3, nrow = 1)

ggsave(lowNP_growth_suppl_plot, dpi = 600, height = 125, width = 300, units = "mm",
       filename = here("processed_data/lowNP_growth_suppl_plot.png")
)





