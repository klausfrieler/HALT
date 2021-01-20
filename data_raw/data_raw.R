library(tidyverse)

usethis::use_build_ignore(c("material", "data_raw"))

stimuli_map <- c(
"Po1" = "Po1_12x_pink_noise_-46dBFS.mp3",
"Po2A" =  "Po2A_3x_pink_noise_-1dBFS+4x_pink_noise_-46dBFS-count_7.mp3",
"Po2B" =	"Po2B_3x_pink_noise_-1dBFS+5x_pink_noise_-46dBFS-count_8.mp3",
"Po2C" =	"Po2C_3x_pink_noise_-1dBFS+6x_pink_noise_-46dBFS-count_9.mp3",
"Po3A" = "Po3A_4x_pink_noise_-40dBFS+3x_pink_noise_-46dBFS+3x_pink_noise_-52dBFS-count_10.mp3",
"Po3B" =	"Po3B_4x_pink_noise_-40dBFS+4x_pink_noise_-46dBFS+3x_pink_noise_-52dBFS-count_11.mp3",
"Po3C" =	"Po3C_4x_pink_noise_-40dBFS+4x_pink_noise_-46dBFS+4x_pink_noise_-52dBFS-count_12.mp3",
"Po4" =	"Po4_left-right-picture.png",
"Po5A" =	"Po5A_pink_noise_-1dBFS_4xleft_6xright_count_10.mp3",
"Po5B" =	"Po5B_pink_noise_-1dBFS_6xleft_4xright_count_10.mp3",
"Po5C" =	"Po5C_pink_noise_-1dBFS_8xleft_2xright_count_10.mp3",
"Po6A" =	"Po6A_Bilsen_left_to_right.mp3",
"Po6B" =	"Po6B_Bilsen_right_to_left.mp3",
"Po7A" =	"Po7A_Franssen_effect_-15dBFS_sine_1kHz_left_perception.mp3",
"Po7B" =	"Po7B_Franssen_effect_-15dBFS_sine_1kHz_right_perception.mp3",
"Po8A" =	"Po8A_3x_140Hz_count_3.mp3",
"Po8B" =	"Po8B_4x_140Hz_count_4.mp3",
"Po8C" =	"Po8C_6x_140Hz_count_6.mp3",
"Po9A" =	"Po9A_3x_100Hz_count_3.mp3",
"Po9B" =	"Po9B_5x_100Hz_count_5.mp3",
"Po9C" =	"Po9C_6x_100Hz_count_6.mp3",
"Po10A" =	"Po10A_3x_60Hz_count_3.mp3",
"Po10B" =	"Po10B_4x_60Hz_count_4.mp3",
"Po10C" =	"Po10C_5x_60Hz_count_5.mp3",
"Po11A" =	"Po11A_4x_20Hz_count_4.mp3",
"Po11B" =	"Po11B_5x_20Hz_count_5.mp3",
"Po11C" =	"Po11C_6x_20Hz_count_6.mp3",
"Po12A" =	"Po12A_4x_pink_noise_-40dBFS+3x_pink_noise_-46dBFS+3x_pink_noise_-52dBFS-count_10.mp3",
"Po12B" =	"Po12B_4x_pink_noise_-40dBFS+4x_pink_noise_-46dBFS+3x_pink_noise_-52dBFS-count_11.mp3",
"Po12C" =	"Po12C_4x_pink_noise_-40dBFS+4x_pink_noise_-46dBFS+4x_pink_noise_-52dBFS-count_12.mp3",
"Po13A" = "Wo1LPQ.wav",
"Po13B" = "Wo2LQP.wav",
"Po13C" = "Wo3PLQ.wav",
"Po13D" = "Wo4PQL.wav",
"Po13E" = "Wo5QLP.wav",
"Po13F" = "Wo6QPL.wav"
)
#'Internal item bank
#'
item_bank <- read.csv("data_raw/item_bank.csv", sep  =";", stringsAsFactors = F) %>%
  as_tibble() %>%
  left_join(stimuli_map  %>%
              as_tibble() %>%
              mutate(stimulus_id = tolower(names(stimuli_map)),
                     value = sprintf("%s.mp3", names(stimuli_map))) %>%
              dplyr::select(stimulus_id, audio_file = value), by = "stimulus_id")
item_bank[item_bank$stimulus_id == "po4",]$audio_file <- "Po4.png"
item_bank[item_bank$page_id == "po13",]$audio_file <- str_replace(item_bank[item_bank$page_id == "po13",]$audio_file, "mp3", "wav")

usethis::use_data(item_bank, overwrite = TRUE)

#'Internal configuration data
#'
test_config <- readRDS("data_raw/test_config.rds") %>% as_tibble()
test_config$logic_expr <- str_replace(test_config$method, "Test ", "") %>%
  str_replace("Test", "") %>%
  str_replace(" HP", "") %>%
  str_replace("OR", "||") %>%
  str_replace("AND", "&&") %>%
  str_replace("at least 1", "A || B || C") %>%
  str_replace("at least 2", "(A && B) || (A && C ) || (B && C)") %>%
  str_replace("all", "A && B && C")

usethis::use_data(test_config, overwrite = TRUE)


HALT_dict_raw <- readxl::read_xlsx("data_raw/HLT_dict.xlsx", trim_ws = T)
HALT_dict <- HALT_dict_raw %>% psychTestR::i18n_dict$new()
usethis::use_data(HALT_dict, overwrite = TRUE)
