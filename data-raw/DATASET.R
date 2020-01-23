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

# function which sets some muscles groups to 1, 0.6, 0.3
classify_exercise <- function(major,
                              minor,
                              subminor) {
  # TODO: output c(0,1,0.3,0,0,0) style vector corresponding to muscles groups
}
