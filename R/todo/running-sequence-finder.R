library(tidyverse)

# The source data:
tmp = tibble::tribble(
  ~patient, ~is.pos,
  1 , c(FALSE,FALSE,FALSE,FALSE,FALSE),
  2 , c(TRUE,TRUE,FALSE,FALSE,FALSE),
  3 , c(FALSE,FALSE,FALSE,TRUE,TRUE),
  4 , c(TRUE,FALSE,FALSE,TRUE,TRUE),
) %>% tidyr::unnest(is.pos)

# The expected output:
target = tibble::tribble(
  ~patient, ~result, ~ censor,
  1, 0, FALSE, # N.b. not sure you rally want this. If not leave off the last stage
  2, 2, FALSE,
  3, 2, TRUE,
  4, 1, FALSE,
  4, 2, TRUE
)


test = tmp %>% group_by(patient) %>%
  # identify the start of a sequence of TRUE's when the value before is FALSE
  # and keep the row_number as an start index
  mutate(
    begin.seq = ifelse(
      is.pos == TRUE & lag(is.pos,default = FALSE) == FALSE,
      row_number(),-1
    )
  ) %>%
  # identify the end of a sequence of TRUE's when the value after is FALSE
  # and keep the row_number as an end index, -1 is used as a marker of
  # rows that do not match
  mutate(
    end.seq = ifelse(
      is.pos == TRUE & lead(is.pos,default = FALSE) == FALSE,
      row_number(),-1
    )
  ) %>%
  # The cumulative max is a way of creating a running index in sequence
  # this keeps the sequencing intact of the start index and end index
  mutate(
    begin.seq.cum = cummax(begin.seq),
    end.seq.cum = cummax(end.seq)
  ) %>%
  # Keep only combinations that you want.
  # if either begin or end is -1 then we haven't found a start or end of ,
  # if being.seq > end.seq then the current row is within a run of TRUES,
  # that has not yet completed, and the end.seq is from the last match.
  filter(begin.seq.cum > 0 & end.seq.cum > 0 & begin.seq.cum <= end.seq.cum) %>%
  # by this stage we have a marker for every row that is within a sequence
  # but we are only interested in those that
  select(patient, begin.seq.cum, end.seq.cum) %>%
  distinct() %>%
  mutate(
    result = end.seq.cum - begin.seq.cum + 1,
    censor = end.seq.cum == 5
  ) %>%
  # I'm not sure you actually need this last bit
  # its only to fill in the zero results for patient 1:
  ungroup() %>%
  complete(
    patient = 1:4, fill = list(result = 0, censor = FALSE)
  )

all(test$result == target$result)
all(test$censor == target$censor)
