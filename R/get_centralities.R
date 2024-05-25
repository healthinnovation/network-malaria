get_centralities <- function(graph) {
  graph_centralities <- graph |>
    tidygraph::activate(nodes) |>
    dplyr::mutate(
      dist_stre = tidygraph::centrality_degree(
        weights = 1/distance
      ),
      dist_pop_gravity_stre = tidygraph::centrality_degree(
        weights = dist_pop_gravity
      ),
      dist_forest_gravity_stre = tidygraph::centrality_degree(
        weights = dist_forest_gravity
      ),
      time_stre = tidygraph::centrality_degree(
        weights = 1/time
      ),
      time_pop_gravity_stre = tidygraph::centrality_degree(
        weights = time_pop_gravity
      ),
      time_forest_gravity_stre = tidygraph::centrality_degree(
        weights = time_forest_gravity
      ),
      dist_close = tidygraph::centrality_closeness(
        weights = distance
      ),
      dist_pop_gravity_close = tidygraph::centrality_closeness(
        weights = 1/dist_pop_gravity
      ),
      dist_forest_gravity_close = tidygraph::centrality_closeness(
        weights = 1/dist_forest_gravity
      ),
      time_close = tidygraph::centrality_closeness(
        weights = time
      ),
      time_pop_gravity_close = tidygraph::centrality_closeness(
        weights = 1/time_pop_gravity
      ),
      time_forest_gravity_close = tidygraph::centrality_closeness(
        weights = 1/time_forest_gravity
      ),
      dist_between = tidygraph::centrality_betweenness(
        weights = distance
      ),
      dist_pop_gravity_between = tidygraph::centrality_betweenness(
        weights = 1/dist_pop_gravity
      ),
      dist_forest_gravity_between = tidygraph::centrality_betweenness(
        weights = 1/dist_forest_gravity
      ),
      time_between = tidygraph::centrality_betweenness(
        weights = time
      ),
      time_pop_gravity_between = tidygraph::centrality_betweenness(
        weights = 1/time_pop_gravity
      ),
      time_forest_gravity_between = tidygraph::centrality_betweenness(
        weights = 1/time_forest_gravity
      ),
      dist_eigen = tidygraph::centrality_eigen(
        weights = 1/distance
      ),
      dist_pop_gravity_eigen = tidygraph::centrality_eigen(
        weights = dist_pop_gravity
      ),
      dist_forest_gravity_eigen = tidygraph::centrality_eigen(
        weights = dist_forest_gravity
      ),
      time_eigen = tidygraph::centrality_eigen(
        weights = 1/time
      ),
      time_pop_gravity_eigen = tidygraph::centrality_eigen(
        weights = time_pop_gravity
      ),
      time_forest_gravity_eigen = tidygraph::centrality_eigen(
        weights = time_forest_gravity
      )
    ) |>
    dplyr::mutate(
      dplyr::across(dist_stre:time_forest_gravity_eigen, scales::rescale)
    )
  graph_centralities
}
