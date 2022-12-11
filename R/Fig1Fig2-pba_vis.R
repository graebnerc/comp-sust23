here::i_am("R/Fig1Fig2-pba_vis.R")
library(data.table)
library(dplyr)
library(tidyr)
library(here)
library(ggplot2)
library(countrycode)
library(ggrepel)
library(icaeDesign)
update_pop_data <- FALSE

row_regions <- data.frame(
  ccode = c('WA', 'WL', 'WE', 'WF', 'WM'),
  region = c('RoW Asia and Pacific', 'RoW America', 'RoW Europe',
             'RoW Africa', 'RoW Middle East')
)

pba_accounts_abs <- fread(here("data/pba_disagg.csv")) %>%
  dplyr::mutate(iso3c=ifelse(
    test = country %in% row_regions$ccode,
    yes =countrycode(
      country, "ccode", "region", custom_dict = row_regions, warn = FALSE),
    no = countrycode(
      country, "iso2c", "iso3c", warn = FALSE)
    )) %>%
  dplyr::select(-any_of(c("V1", "type", "country")))

single_countries <- unique(pba_accounts_abs$iso3c)[
  nchar(unique(pba_accounts_abs$iso3c))==3]
years_considered <- unique(pba_accounts_abs$year)

if (update_pop_data){
  pop_data <- WDI::WDI(
    country = countrycode(single_countries, "iso3c", "iso2c"),
    start = min(years_considered),
    end = max(years_considered),
    indicator = c("population"="SP.POP.TOTL")) %>%
    select(-country, -iso2c)
  fwrite(pop_data, file = here("data/pop_data.csv"))
} else {
  pop_data <- fread(here("data/pop_data.csv"))
}

# GLOBAL PBA-------------------------------------------------------------------
pba_accounts_pc <- pba_accounts_abs %>%
  left_join(x = ., y = pop_data, by = c("iso3c", "year")) %>%
  dplyr::mutate(
    group = countrycode(iso3c, "iso3c", "eu28", warn = FALSE),
    group = ifelse(is.na(group), iso3c, group)
  ) %>%
  group_by(group, year) %>%
  summarise(
    across(.cols = all_of(c( "GWP", "population")), # "Employment", "ValueAdded"
           .fns = ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::filter(population>0) %>%
  dplyr::mutate(
    GWP_pc = GWP/population,
    group_name = countrycode(group, "iso3c", "country.name", warn = FALSE),
    group_name = ifelse(group=="EU", "EU28", group_name))

# Dynamic line plot -----------------------------------------------------------

pba_accounts_pc_n <-  pba_accounts_pc %>%
  dplyr::filter(year>1999) %>%
  group_by(group) %>%
  dplyr::mutate(GWP_pc_n = GWP_pc / first(GWP_pc)) %>%
  ungroup()

dyn_pba_n <- ggplot(
  data = pba_accounts_pc_n,
  aes(x=year, y=GWP_pc_n, color=group_name)
) +
  geom_hline(yintercept = 1.0, color=get_euf_colors()) +
  geom_point() + geom_line() +
  labs(
    title = "Global Warming Potential per capita over time (PBA)",
    y = "GWP100 per capita",
    caption = "Data: EXIOBASE3, World Bank; own calculation.") +
  ggrepel::geom_label_repel(
    data = dplyr::filter(
      pba_accounts_pc_n,
      year %in% c(max(years_considered))),
    mapping = aes(label=group),
    show.legend = FALSE, size=2, label.r = 0.1) +
  scale_color_euf(palette = "mixed") +
  theme_icae() +
  theme(
    axis.title.x = element_blank(),
    legend.position = c(0.35, 0.85)) +
  guides(color = guide_legend(ncol = 3))
dyn_pba_n

# Bar plot per capita ---------------------------------------------------------

bar_pba <- pba_accounts_pc %>%
  dplyr::filter(year>=2015) %>%
  group_by(group, group_name) %>%
  summarise(GWP_pc = mean(GWP_pc), .groups = "drop") %>%
  dplyr::mutate(
    eu_marker = ifelse(group_name=="EU28", "EU28", "Other")
    ) %>%
  ggplot(
  data = .,
  aes(
    x=reorder(group_name, -GWP_pc),
    y=GWP_pc, fill=eu_marker)
) +
  geom_bar(stat = "identity", color="white") +
  scale_y_continuous(
    labels = scales::label_number(scale = 0.001, suffix = "k"),
    expand = expansion(add = c(0, 1500))) +
  labs(
    title = "Global Warming Potential per capita (PBA, 2015-2019)",
    y = "GWP100 per capita (2015-2019 avg.)",
    caption = "Data: EXIOBASE3, World Bank; own calculation.") +
  scale_color_euf(
    palette = "mixed", aesthetics = c("color", "fill"), reverse = FALSE
    ) +
  theme_icae() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none")
bar_pba

# Bar plot abs ----------------------------------------------------------------

pba_abs_plot <- pba_accounts_abs %>%
  dplyr::filter(year>=2015) %>%
  dplyr::mutate(
    group = countrycode(iso3c, "iso3c", "eu28", warn = FALSE),
    group = ifelse(is.na(group), iso3c, group)
  ) %>%
  group_by(group, year) %>%
  summarise(
    across(.cols = all_of(c( "GWP")),
           .fns = ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    group_name = countrycode(group, "iso3c", "country.name", warn = FALSE),
    group_name = ifelse(group=="EU", "EU28", group_name),
    group_name = ifelse(substr(group, 1, 3)=="RoW", group, group_name)) %>%
  group_by(group_name) %>%
  summarise(
    GWP_mean = mean(GWP),
    GWP_sum = sum(GWP),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    eu_marker = ifelse(group_name=="EU28", "EU28", "Other")
    ) %>%
  ggplot(
    data = .,
    aes(
      x=reorder(group_name, -GWP_mean),
      y=GWP_mean, fill=eu_marker)
  ) +
  geom_bar(stat = "identity", color="white") +
  scale_y_continuous(
    labels = scales::label_number(scale = 0.000000000001, suffix = "T"),
    expand = expansion(add = c(0, 1500))) +
  labs(
    title = "Global Warming Potential (PBA, 2015-2019)",
    y = "GWP100 (2015-2019 average)",
    caption = "Data: EXIOBASE3, World Bank; own calculation.") +
  scale_color_euf(
    palette = "mixed", aesthetics = c("color", "fill"), reverse = FALSE
    ) +
  theme_icae() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 1),
        legend.position = "none") +
  coord_flip()
pba_abs_plot

# Complete plot ---------------------------------------------------------------

pba_plots_l <- ggpubr::ggarrange(
  bar_pba, pba_abs_plot, ncol = 1, nrow = 2, labels = c("B)", "C)"))

pba_plots <- ggpubr::ggarrange(
  dyn_pba_n,pba_plots_l, ncol = 2, nrow = 1, labels = c("A)", NA))

ggsave(plot = pba_plots,
       filename = here("output/Figure-2-PBA-World.pdf"),
       width = 12, height = 6)

ggsave(plot = pba_plots,
       filename = here("output/Figure-2-PBA-World.png"),
       width = 12, height = 6)


# EU PBA ----------------------------------------------------------------------
pba_eu_accounts_pc <- pba_accounts_abs %>%
  left_join(x = ., y = pop_data, by = c("iso3c", "year")) %>%
  dplyr::mutate(
    group = countrycode(iso3c, "iso3c", "eu28", warn = FALSE)
  ) %>%
  dplyr::filter(!is.na(group)) %>%
  dplyr::mutate(
    GWP_pc = GWP/population,
    full_name = countrycode(iso3c, "iso3c", "country.name"))

pba_eu_accounts_pc_n <-  pba_eu_accounts_pc %>%
  dplyr::filter(year>1999) %>%
  group_by(iso3c, full_name) %>%
  dplyr::mutate(GWP_pc_n = GWP_pc / first(GWP_pc)) %>%
  ungroup()

# Dynamic line plot -----------------------------------------------------------

dyn_pba_eu_n <- ggplot(
  data = pba_eu_accounts_pc_n,
  aes(x=year, y=GWP_pc_n, color=full_name)
) +
  geom_hline(yintercept = 1.0, color=get_euf_colors()) +
  geom_point() + geom_line() +
  labs(
    title = "Global Warming Potential per capita over time (PBA)",
    y = "GWP100 per capita",
    caption = "Data: EXIOBASE3, World Bank; own calculation.") +
  scale_y_continuous(
    limits = c(0.2, 1.6),
    breaks = seq(0.4, 1.6, 0.2),
    labels = scales::label_percent()
  ) +
  scale_x_continuous(limits = c(1999, 2023), breaks = seq(2000, 2020, 5)) +
  ggrepel::geom_label_repel(
    data = dplyr::filter(
      pba_eu_accounts_pc_n,
      year %in% c(max(years_considered))),
    mapping = aes(label=iso3c),
    show.legend = FALSE, size=2, label.r = 0.1, max.overlaps = 20, xlim = c(1990, 2025)) +
  scale_color_euf(palette = "mixed") +
  theme_icae() +
  theme(
    axis.title.x = element_blank(),
    legend.position = c(0.38, 0.2)) +
  guides(color = guide_legend(ncol = 4))
dyn_pba_eu_n


# Bar plot per capita ---------------------------------------------------------

bar_eu_pba <- pba_eu_accounts_pc %>%
  dplyr::filter(year>=2015) %>%
  group_by(iso3c, full_name) %>%
  summarise(GWP_pc = mean(GWP_pc), .groups = "drop") %>%
  ggplot(
    data = .,
    aes(
      x=reorder(full_name, -GWP_pc),
      y=GWP_pc)
  ) +
  geom_bar(stat = "identity", color="white", fill="#00395B") +
  scale_y_continuous(
    labels = scales::label_number(scale = 0.001, suffix = "k"),
    expand = expansion(add = c(0, 1500))) +
  labs(
    title = "Global Warming Potential per capita (PBA, 2015-2019)",
    y = "GWP100 per capita (2015-2019 avg.)",
    caption = "Data: EXIOBASE3, World Bank; own calculation.") +
  theme_icae() +
  theme(axis.title.y = element_blank(),
        #axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none") +
  coord_flip()
bar_eu_pba

# Bar plot abs ----------------------------------------------------------------

pba_eu_abs_plot <- pba_eu_accounts_pc %>%
  dplyr::filter(year>=2015) %>%
  group_by(iso3c, full_name) %>%
  summarise(
    GWP_mean = mean(GWP),
    GWP_sum = sum(GWP),
    .groups = "drop"
  ) %>%
  ggplot(
    data = .,
    aes(
      x=reorder(full_name, -GWP_mean),
      y=GWP_mean)
  ) +
  geom_bar(stat = "identity", color="white", fill="#00395B") +
  scale_y_continuous(
    labels = scales::label_number(scale = 0.000000000001, suffix = "T"),
    expand = expansion(add = c(0, 1500))) +
  labs(
    title = "Global Warming Potential (PBA, 2015-2019)",
    y = "GWP100 (2015-2019 average)",
    caption = "Data: EXIOBASE3, World Bank; own calculation.") +
  scale_color_euf(
    palette = "mixed", aesthetics = c("color", "fill"), reverse = FALSE
  ) +
  theme_icae() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 1),
        legend.position = "none") +
  coord_flip()
pba_eu_abs_plot

# Complete plot ---------------------------------------------------------------
pba_eu_plots_l <- ggpubr::ggarrange(
  bar_eu_pba, pba_eu_abs_plot, ncol = 1, nrow = 2, labels = c("B)", "C)"))

pba_eu_plots <- ggpubr::ggarrange(
  dyn_pba_eu_n, pba_eu_plots_l, ncol = 2, nrow = 1, labels = c("A)", NA))

ggsave(plot = pba_eu_plots,
       filename = here("output/Figure-1-PBA-EU.pdf"),
       width = 12, height = 6)

ggsave(plot = pba_eu_plots,
       filename = here("output/Figure-1-PBA-EU.png"),
       width = 12, height = 6)
