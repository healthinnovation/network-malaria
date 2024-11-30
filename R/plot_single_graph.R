source("R/graph_palette.R")
g_palette <- graph_palette()
plot_single_graph <- function(graphs, layout, group, weight, centrality, weight_label, centrality_label) {
  plt  <- graphs |>
    ggraph::ggraph(layout = "kk") +
    ggraph::geom_edge_arc(
      ggplot2::aes(alpha = {{ weight }}, color = as.factor({{ group }})),
      strength = 0.1
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(size = {{ centrality }}, color = as.factor({{ group }}))
    ) +
    ggplot2::scale_color_manual(
      limits = as.factor(dplyr::pull(layout, {{ group }})),
      values = g_palette(nrow(layout)),
      name = "Watershed",
      guide = ggplot2::guide_legend(
        position = "right", direction = "vertical",
        theme = theme(ggplot2::element_text(size = 12)),
      )
    ) +
    ggraph::scale_edge_color_manual(
      limits = as.factor(dplyr::pull(layout, {{ group }})),
      values = g_palette(nrow(layout)),
      name = "Watershed",
      guide = ggplot2::guide_legend(
        position = "right", direction = "vertical",
        theme = theme(ggplot2::element_text(size = 12))
      )
    ) +
    ggplot2::scale_size_continuous(
      name = centrality_label,
      guide = ggplot2::guide_legend(
        position = "top", direction = "horizontal",
        theme = ggplot2::theme(
          ggplot2::element_text(size = 15)
        )
      )
    ) +
    ggraph::scale_edge_alpha_continuous(
      name = weight_label,
      guide = ggplot2::guide_legend(
        position = "top", direction = "horizontal",
        theme = ggplot2::theme(
          ggplot2::element_text(size = 15)
        )
      )
    ) +
    ggraph::theme_graph(base_family = "sans") +
    ggplot2::theme(
      legend.box = "vertical"
    )

  plt
}

