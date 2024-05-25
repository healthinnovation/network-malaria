source("R/graph_palette.R")
plot_multiple_graph <- function(graphs, layout, facet, weight, centrality, weight_label, centrality_label) {
  plt <- graphs |>
    ggraph::ggraph(layout = "stress") +
    ggraph::facet_nodes(ggplot2::vars({{ facet }}), ncol = 4, scales = "free") +
    ggraph::geom_edge_arc(
      ggplot2::aes(alpha = {{ weight }}, color = as.factor({{ facet }})),
      strength = 0.1
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(size = {{ centrality }}, colour = as.factor({{ facet }}))
    ) +
    ggplot2::scale_color_manual(
      limits = as.factor(dplyr::pull(layout, {{ facet }})),
      values = g_palette(nrow(layout)),
      guide = "none"
    ) +
    ggraph::scale_edge_color_manual(
      limits = as.factor(dplyr::pull(layout, {{ facet }})),
      values = g_palette(nrow(layout)),
      guide = "none"
    ) +
    ggplot2::scale_size_continuous(name = centrality_label, guide = "legend") +
    ggraph::scale_edge_alpha_continuous(name = weight_label) +
    ggraph::theme_graph(
      base_family = "sans", strip_text_size = 14, strip_text_face = "plain"
    ) +
    ggplot2::theme(
      legend.position = "top", legend.box = "vertical",
      text = ggplot2::element_text(size = 15)
    )

  plt
}
