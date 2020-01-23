## code to prepare `DATASET` dataset goes here

df <- data.frame(x = 1:3, y = 2:4, z = 3:5)

usethis::use_data(df)

# list excercises and muscles
exercise_list <- c("Squat (Barbell)",
                   "Stiff Leg Deadlift (Barbell)",
                   "Seated Calf Raise (Machine)",
                   "Lying Leg Curl (Machine)",
                   "Leg Press",
                   "Standing Calf Raise (Smith Machine)",
                   "Chin Up",
                   "Bent Over Row - Underhand (Barbell)",
                   "Preacher Curl (Machine)",
                   "Face Pull (Cable)",
                   "Bicep Curl (Dumbbell)",
                   "Lat Pulldown - Wide Grip (Cable)",
                   "Seated Row (Cable)",
                   "Bench Press (Barbell)",
                   "Overhead Press (Barbell)",
                   "Triceps Dip (Assisted)",
                   "Lateral Raise (Dumbbell)",
                   "Bench Press (Cable)",
                   "Lateral Raise (Cable)")

muscle_list <- c("Core",
                 "Arms",
                 "Back",
                 "Chest",
                 "Legs",
                 "Shoulders")

# function which outputs some muscle groups to 1, 0.6, 0.3
classify_exercise <- function(major = c(),
                              minor = c(),
                              subminor = c(),
                              muscle_list) {
  temp_df <- data.frame(Core = 0,
                        Arms = 0,
                        Back = 0,
                        Chest = 0,
                        Legs = 0,
                        Shoulders = 0)

  for (muscle in muscle_list) {
    if (muscle %in% major) {
      temp_df[[muscle]] <- 1
    }
    if (muscle %in% minor) {
      temp_df[[muscle]] <- 0.6
    }
    if (muscle %in% subminor) {
      temp_df[[muscle]] <- 0.3
    }
  }

  return(temp_df)
}
