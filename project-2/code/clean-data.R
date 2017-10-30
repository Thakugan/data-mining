clean_dat <- function(dat) {
  # Save original length of dat
  original_len <- length(dat$PseudoID)
  
  # Replacing codes with values
  # Agency names
  agency_trans <- read_lines("https://archive.org/download/opm-federal-employment-data/data/1973-09-to-2014-06/SCTFILE.TXT")
  agency_id <- sapply(agency_trans, FUN = function(x) substring(x, 3,6))
  agency_name <- trimws(sapply(agency_trans, FUN = function(x) substring(x, 36,75)))
  agency_trans_table <- data.frame(agency_id = agency_id, agency_name = agency_name)
  m <- match(dat$Agency, agency_trans_table$agency_id)
  dat$Agency <-  agency_trans_table$agency_name[m]
  
  # States
  state_trans <- read_lines("https://ia600608.us.archive.org/16/items/opm-federal-employment-data/data/2014-09-to-2016-09/non-dod/translations/State%20Translations.txt")
  state_id <- sapply(state_trans, FUN = function(x) substring(x, 1,2))
  state_name <- trimws(sapply(state_trans, FUN = function(x) substring(x, 35,72)))
  dat <- mutate(dat,
                Station = as.factor(substring(Station, 1,2)))
  state_trans_table <- tail(data.frame(state_id = state_id, state_name = state_name),-1)
  m <- match(dat$Station, state_trans_table$state_id)
  dat$Station <-  state_trans_table$state_name[m]
  
  # Work Schedule
  schedule_trans <- read_lines("https://ia600608.us.archive.org/16/items/opm-federal-employment-data/data/2014-09-to-2016-09/dod/translations/Work%20Schedule%20Translation.txt")
  schedule_id <- sapply(schedule_trans, FUN = function(x) substring(x, 1,1))
  schedule_name <- trimws(sapply(schedule_trans, FUN = function(x) substring(x, 28,103)))
  schedule_trans_table <- tail(data.frame(schedule_id = schedule_id, schedule_name = schedule_name),-3)
  m <- match(dat$Schedule, schedule_trans_table$schedule_id)
  dat$Schedule <-  schedule_trans_table$schedule_name[m]
  
  # Creating pay
  dat <- mutate(dat,
                pay = ifelse(as.numeric(Pay) < 200000, as.character(((as.numeric(Pay) %/% 25000) + 1 ) * 25000), as.character(200000)))
  
  # Get rid of small agencies
  agencies <- names(which(table(dat$Agency)>10000))
  dat <- filter(dat,
                Agency %in% agencies)
  
  # Selecting features and changing name
  dat <- select(dat,
                pay, date = Date, agency = Agency, state = Station,
                age = Age, education = Education, pay_plan = PayPlan,
                grade = Grade, los = LOS, occupation = Occupation,
                category = Category, supervisory_status = SupervisoryStatus,
                appointment = Appointment, schedule = Schedule, nsftp = NSFTP)
  
  # Remove all NA's and unknowns
  dat <- filter(dat,
                !(state %in% c("##", "**")),
                !(age == "UNSP"),
                !(education %in% c("", "*", "**")),
                !(pay_plan %in% c("", "*", "**")),
                !(grade %in% c("", "**")),
                !(los == "UNSP"),
                !(occupation %in% c("", "****")),
                !(category %in% c("", "*", "**")),
                !(supervisory_status %in% c("", "*", "**")),
                !(appointment %in% c("", "**")),
                !(schedule %in% c("", "*", "**")))
  dat <- drop_na(dat)
  
  # TODO Change attributes to continuous values
  # Make sure each attribute is the correct type
  dat <- mutate(dat,
                pay = as.factor(pay),
                agency = as.character(agency),
                state = as.character(state),
                age = as.numeric(substr(age, start = 1, stop = 2)),
                education = as.numeric(education),
                los = as.numeric(sub("-.*|\\+|<", "", los)),
                category = as.character(category),
                supervisory_status = as.numeric(supervisory_status))
  
  dat <- select(dat,
                pay, agency, state, age, education, los, category, supervisory_status)
  
  # Show change
  new_len <- length(dat$pay)
  print(paste(new_len, "of", original_len,"records maintained:",
              format(round(new_len/original_len*100, 2)), "%"))
  
  return(dat)
}