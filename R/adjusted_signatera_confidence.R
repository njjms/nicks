#' Adjust Signatera positive calls
#'
#' New updated threshold for C>A/G>T and C>T/G>A from 0.97→ 0.98
#'
#' @param df containing columns for "ref", "mut", "confidence"
#' @param amplicon_id defaults to TRUE. Adds "ref", "mut" columns based on optional amplicon_id column
#' @return df with "adjustedMutationPresent" column added
#' @examples
#' adjust_sig_positives(df)
#' @export

sig_adjust_confidences <- function(input_df, amplicon_id=TRUE) {
	if (amplicon_id) {
			dplyr::mutate(
				.data = input_df,
				ref = stringr::str_sub(amplicon_id, -3, -3),
				mut = stringr::str_sub(amplicon_id, -1, -1)
			) -> input_df
	}
	dplyr::mutate(
		.data = input_df,
		should_adjust = ifelse((ref %in% c("C", "G") &
									   (mut %in% c("A", "T"))), 1, 0),
		adjust_mutationPresent = ifelse((should_adjust == 1) &
										(confidence >= .98), 1, 0),
		noadjust_mutationPresent = ifelse((should_adjust == 0) &
										  (confidence >= .97) &
										  (mutationPresent != 0), 1, 0),
		adjustedMutationPresent = adjust_mutationPresent + noadjust_mutationPresent
	) -> output_df
	dplyr::select(.data = output_df,
	              -adjust_mutationPresent,
                -should_adjust,
                -noadjust_mutationPresent) -> output_df
	return(output_df)
}

