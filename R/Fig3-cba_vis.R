here::i_am("R/Fig3-cba_vis.R")
library(data.table)
library(dplyr)
library(tidyr)
library(here)
library(ggplot2)
library(countrycode)
library(ggrepel)
library(icaeDesign)
library(ggpubr)
update_pop_data <- FALSE

row_regions <- data.frame(
  ccode = c('WA', 'WL', 'WE', 'WF', 'WM'),
  region = c('RoW Asia and Pacific', 'RoW America', 'RoW Europe',
             'RoW Africa', 'RoW Middle East')
)

cba_accounts_abs <- fread(here("data/cba_disagg.csv")) %>%
  dplyr::mutate(iso3c=ifelse(
    test = country %in% row_regions$ccode,
    yes =countrycode(
      country, "ccode", "region", custom_dict = row_regions, warn = FALSE),
    no = countrycode(
      country, "iso2c", "iso3c", warn = FALSE)
  )) %>%
  dplyr::select(-any_of(c("V1", "type", "country")))  %>%
  dplyr::mutate(
    eu = countrycode(iso3c, "iso3c", "eu28", warn = FALSE),
    eu = ifelse(is.na(eu), "NonEU", eu)
  )

cba_accounts_eu <- cba_accounts_abs %>%
  dplyr::filter(eu=="EU") %>%
  dplyr::mutate(GWP_extern = GWP_export - GWP_import)
cba_accounts_eu

cba_accounts_eu_agg <- cba_accounts_eu %>%
  dplyr::filter(year>1999) %>%
  group_by(iso3c) %>%
  summarise(across(
    .cols = starts_with("GWP"),
    .fns = ~ sum(.x, na.rm = TRUE)),
    .groups = "drop") %>%
  dplyr::mutate(positive = ifelse(GWP_extern>0, "Net exporter", "Net importer"))

cba_accounts_eu_long <- cba_accounts_eu %>%
  tidyr::pivot_longer(
    cols = starts_with("GWP"),
    names_to = "Measure",
    values_to = "Value")

eu_bar <- ggplot(
  data = cba_accounts_eu_agg,
  aes(x=reorder(iso3c, -GWP_extern), y=GWP_extern) # , linetype=Measure, shape=Measure
) +
  geom_bar(
    stat = "identity", color="white",
    fill=get_euf_colors(col_name = "blue")
    ) +
  labs(
    title = "Cumulated GWP exports of EU countries (2000-2020)",
    y = "Cumulated GWP exports (2000-2020)"
    ) +
  geom_hline(yintercept = 1.0, color=get_euf_colors()) +
  theme_icae() +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1))


# Global dimension --------------------

global_bar <- cba_accounts_abs %>%
  dplyr::mutate(actor = ifelse(eu=="EU", "EU", iso3c)) %>%
  dplyr::mutate(GWP_extern = GWP_export - GWP_import) %>%
  group_by(actor, eu) %>%
  summarise(GWP_extern_cumul = sum(GWP_extern), .groups = "drop") %>%
  dplyr::mutate(
    positive = ifelse(GWP_extern_cumul>0, "Net exporter", "Net importer")
    ) %>%
  ggplot(
    data = .,
    mapping = aes(
      x = reorder(actor, -GWP_extern_cumul),
      y = GWP_extern_cumul,
      fill = eu)
    ) +
  geom_hline(yintercept = 0.0, color=get_euf_colors()) +
  scale_color_euf(
    palette = "mixed", aesthetics = c("color", "fill"), reverse = FALSE
  ) +
  labs(
    title = "Cumulated GWP exports (2000-2020)",
    y = "Cumulated GWP exports (2000-2020)",
    caption = "Data: EXIOBASE3; own calculation."
    ) +
  geom_bar(stat = "identity") +
  theme_icae() +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1))

# Full plot --------------------

full_plot <- ggpubr::ggarrange(
  eu_bar, global_bar,
  ncol = 2,
  labels = c("A)", "B)"),
  font.label=list(size=11)
  )

ggsave(
  plot = full_plot,
  filename = here("output/Figure-3-CBA.pdf"),
  width = 8, height = 3)

ggsave(
  plot = full_plot,
  filename = here("output/Figure-3-CBA.png"),
  width = 8, height = 3)

