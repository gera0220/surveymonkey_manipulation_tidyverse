library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

data <- read_excel("data/survey_monkeys.xlsx", sheet = "Edited_Data") %>% 
  janitor::clean_names() %>% 
  select(-c('start_date', 'end_date', 'email_address', 'first_name', 'last_name',
            'custom_data_1')) %>% 
  pivot_longer(cols = starts_with('question'), names_to = 'question_subquestion',
               values_to = 'answer')

questions <- read_excel('data/survey_monkeys.xlsx', sheet = 'Question_R') %>% 
  janitor::clean_names()

questions$question_subquestion <- questions$question_subquestion %>%
  str_to_lower() %>% 
  str_replace_all("\\.\\.\\.", "_") %>% ## Replace something like hey.man...bye into hey.man_bye
  str_replace_all("\\.", "_") # Replace something like hey.man_bye into hey_man_bye

data_merged <- left_join(data, questions, by = "question_subquestion")

respondents <- data_merged %>% 
  drop_na(answer)

respondents <- respondents %>% 
  group_by(question) %>% 
  count(name = "respondents")
  
data_merged_two <- left_join(data_merged, respondents, by = "question") %>% 
  drop_na(answer)

same_answer <- data_merged_two %>%
  group_by(question_subquestion, answer) %>% 
  count(name = "same_answer")

data_merged_three <- left_join(data_merged_two, same_answer, 
                               by = c('question_subquestion', 'answer')) %>% 
  rename(
    division = identify_which_division_you_work_in_response,
    division_other = identify_which_division_you_work_in_other_please_specify,
    position = which_of_the_following_best_describes_your_position_level_response,
    generation = which_generation_are_you_apart_of_response,
    gender = please_select_the_gender_in_which_you_identify_response,
    tenure = which_duration_range_best_aligns_with_your_tenure_at_your_company_response,
    employment_type = which_of_the_following_best_describes_your_employment_type_response
  )

write_csv(data_merged_three, file = "data/final_output.csv")
                               