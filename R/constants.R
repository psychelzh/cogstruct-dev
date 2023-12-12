game_id_rapm <- bit64::as.integer64(265520726213317) # 瑞文高级推理
# 注意警觉, 注意指向: 1.0.0 records device for all right arrow resp as "mouse"
game_id_dev_err <- bit64::as.integer64(c(380173315257221, 380174783693701))
# 多彩文字PRO: correct game duration data
game_id_strp <- bit64::as.integer64(224378628399301)
# 人工词典: re-grade accuracy based on raters
game_id_cr <- bit64::as.integer64(380174879445893)
games_keyboard <- readr::read_lines("config/games_keyboard")
