#' Random sample one case per combination of factors
#'
#' To prevent dependence between samples when repeated measures are used.

#' @param .data a dataframe
#' @param seed random seed by default set to 100
#' @param ... grouping columns (e.g. patient ID, confirmed biopsy truth status, etc.)
#' @return an ungrouped dataframe containing only the randomly selected rows
#'
#' @examples
#' nrows <- 300
#' test_data <- data.frame(
#' 	patient_ids = sort(sample(1:100, nrows, replace=TRUE)),
#' 	test = runif(n=nrows),
#' 	casefile_id = seq_along(1:nrows)
#' )
#'
#' test_data %>%
#' 	mutate(
#' 		status = if_else(test < .80, "STABLE", "AR")
#' 	) %>%
#' 	select(-test) -> test_data
#'
#' select_one_sample_per(test_data, patient_ids, status)
#' @export

select_one_sample_per <- function(.data, seed=100, ...) {
	set.seed(seed)

	.data$randomizer <- runif(n=nrow(.data))
	dplyr::group_by(.data,
				   	...) -> .data
    dplyr::arrange(.data,
				   randomizer,
				   .by_group = TRUE) -> .data
	dplyr::filter(.data,
				  row_number() == 1) -> .data
	dplyr::ungroup(.data) -> .data

	return(.data)
}

