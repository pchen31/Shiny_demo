dt_style_1 <- function(data, pagelength) {
  datatable(data,
            filter = "top",
            extensions = c("ColReorder", "FixedColumns", "KeyTable"),
            options = list(
              colReorder = TRUE,
              scrollX = TRUE,
              fixedColumns = TRUE,
              keys = TRUE,
              dom = "Bfrtip",
              pageLength = pagelength,
              columnDefs = list(
                list(
                  className = "dt-center", 
                  width = 120, 
                  targets = "_all"
                )
              )
            ),
            selection = "none",
            rownames = FALSE,
            class = 'cell-border stripe'
  )
}