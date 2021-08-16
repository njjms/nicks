#' Identify Signatera Constant VAF targets
#'
#' @param .data data.frame containing alleleRatio and some grouping variables such as amplicon_id
#' @param constant_vaf_bound defaults to .3.
#' @return df with a binary labelling column "constant_vaf_target": 1 if yes, 0 if no
#' @examples
#' sig_constant_vaf(df, constant_vaf_bound = .3)

sig_constant_vaf <- function(.data, constant_vaf_bound = .3) {
	dplyr::group_by(.data=.data, amplicon_id) -> output_df
	dplyr::mutate(.data = .data,
				  average_vaf = mean(alleleRatio),
				  max_percentage_change = max(abs((alleleRatio-average_vaf)/average_vaf)),
				  constant_vaf_target = ifelse(max_percentage_change < constant_vaf_bound, 1, 0)) -> output_df
	dplyr::ungroup(output_df) -> output_df
	dplyr::distinct(output_df) -> output_df
	return(output_df)
	
}
