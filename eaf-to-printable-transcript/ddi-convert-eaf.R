library(tidyverse)
library(phonfieldwork)

nth.timestamp <- 5

convert.eaf.ddi <- function(eaf.filename, output.name) {
  # extract filename
  eaf.parts <- unlist(str_split(output.name, "[\\/\\.\\\\]"))
  eaf.name <- as.character(eaf.parts[length(eaf.parts)-1])
  
  # convert to a dataframe
  eaf.contents <- eaf_to_df(eaf.filename)
  
  # create a simplified table from the eaf
  eaf.simple <- eaf.contents %>%
    # make the tier titles consistent
    mutate(
      tier_name = case_when(
        grepl("[Ii]nter", tier_name) ~ "INT",
        grepl("([Pp]art)|([Ss]pe)", tier_name) ~ "PTCP",
        TRUE ~ "other"
      ),
      # round down to preceding second (to avoid printing msec) and
      # convert ss.ms time to hhmmss format
      onset = as.character(hms::as_hms(as.integer(time_start))),
      offset = as.character(hms::as_hms(as.integer(time_end))),
      timestamp = paste0("\n----[", onset, "--", offset, "]----\n")
    ) %>%
    # only include interviewer and participant tier data
    filter(tier_name %in% c("INT", "PTCP")) %>%
    # remove all but nth timestamps
    mutate(
      # elaborate but efficient way to mark every nth row with a 1
      nth.row = rep(
        c(rep(0, nth.timestamp - 1), 1),
        as.integer((nrow(.)/nth.timestamp) + 0.9))[1:nrow(.)],
      # remove timestamps that occur on a "0" line
      timestamp = ifelse(nth.row == 0, "", timestamp),
      # concatenate the utterance data for each line, e.g., "INT: Mm hm"
      concat.utt = paste0(tier_name, ": ", content)
    )
  
  # create a vector of lines
  out.lines <- rep("", nrow(eaf.simple)*2)
  for (i in 1:nrow(eaf.simple)){
    out.lines[(i*2)-1] <- eaf.simple$timestamp[i]
    out.lines[i*2] <- eaf.simple$concat.utt[i]
  }
  
  return(
    list(
      ptcp = eaf.name,
      trsc = out.lines)
    )
}

