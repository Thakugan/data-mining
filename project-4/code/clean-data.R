clean_dat <- function(dat) {
  # Save original length of dat
  original_len <- length(dat$PseudoID)

  # Selecting features and changing name
  dat <- select(dat,
                pay = Pay,
                age = Age, education = Education,
                los = LOS, supervisory_status = SupervisoryStatus)
  
  # Remove all NA's and unknowns
  dat <- filter(dat,
                !(pay == 0),
                !(age == "UNSP"),
                !(education %in% c("", "*", "**")),
                !(los == "UNSP"),
                !(supervisory_status %in% c("", "*", "**")))
  dat <- drop_na(dat)
  
  # Make sure each attribute is the correct type
  dat <- mutate(dat,
                pay = as.numeric(pay),
                age = as.numeric(substr(age, start = 1, stop = 2)),
                education = as.numeric(education),
                los = as.numeric(sub("-.*|\\+|<", "", los)),
                supervisory_status = as.numeric(ifelse(supervisory_status == 8, 0, 1)))
  
  dat <- select(dat,
                pay, age, education, los, supervisory_status)
  
  # Show change
  dat <- drop_na(dat)
  new_len <- length(dat$pay)
  print(paste(new_len, "of", original_len,"records maintained:",
              format(round(new_len/original_len*100, 2)), "%"))
  
  return(dat)
}