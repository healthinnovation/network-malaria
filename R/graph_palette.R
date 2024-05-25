graph_palette <- function() {
  set.seed(1)
  base <- sample(
    c(
      "#F94144", "#F3722C", "#F8961E", "#F9844A", "#F9C74F", "#90BE6D", "#43AA8B",
      "#4D908E", "#577590", "#277DA1", "#F7A072", "#EDDEA4", "#8EA604", "#00CFC1",
      "#485696", "#37323E", "#985277", "#5C374C", "#7678ED", "#CE4257", "#FFBD00"
    )
  )
  palette <- colorRampPalette(base)
  palette
}

