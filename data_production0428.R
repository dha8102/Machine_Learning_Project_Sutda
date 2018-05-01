
install.packages("readxl")
install.packages("writexl")
library(readxl)
library(writexl)
setwd("C:/R/Machine_Learning/Team_Project")
# 1. 섯다 20개 화투 패 생성 ----
CARD <- matrix(ncol = 2, nrow = 20) 
CARD[ ,1] <- rep(1:10, times = 2)
CARD[ ,2] <- rep(0:1, each = 10)
# 2. 족보 정리되어 있는 파일 읽어옴 ----
jokbo <- readxl::read_excel(path = "jokborank.xlsx", sheet = 1, col_names = TRUE)
jokbo <- as.data.frame(jokbo)

##############################

# 두 카드를 넣었을 때 족보 순위 도출 함수 ----
ranking <- function (x, y){ 
  xy <- c(x, y)
  cnt <- 0
  for(i in 1:nrow(jokbo)){
    for(j in 1:4){
      if(xy[j] == jokbo[i, j+1]){  # 카드 벡터(xy)의 넘버가 jokbo의 해당 열과 일치하면 cnt + 1
        cnt <- cnt + 1
      }
    }
    if(cnt == 4){ # 4장 다 정확히 맞으면
      return(jokbo[i, "rank"]) # 해당 행의 절대순위(rank) 리턴
      break
    }
    cnt <- 0
  }
  return(28 - (x[1] + y[1])%%10)  # 끗일 경우 계산식
}


#####################################

# game : 세 족보 순위를 넣었을 때 플레이어 순위를 알려주는 함수 ----

game <- function(p1, p2, p3){ 
  vec <- c(p1,p2,p3)
  if(sum(vec %in% c(26.3, 27.3, 28.3)) > 0){ # 특수패가 있을 때
    if(sum(vec %in% 26.3) > 0){ # 49일 때
      if(sum(vec <= 12) == 0){ # 알리 이상이 없어서 49가 실행될 때
        return(ranking <- c(0,0,0))
        break
      }else{ # 알리 이상이 있어서 49가 3끗으로 처리됨
        ranking <- round(rank(round(vec))) 
        return(ranking)
      }
    }else if(sum(vec %in% 28.3) > 0){ # 땡잡이일 때
      if(sum(vec <= 12 & vec >= 4) > 0){ # 땡이 있어서 땡잡이가 실행될 때
        for(i in 1:3){ #땡잡이에 0번 순위를 매긴다.
          if(vec[i] == 28.3){
            vec[i] <- 0
            ranking <- round(rank(round(vec)))
            return(ranking)
            break
          }
        }#for문 괄호
      }else{ # 땡이 없어서 땡잡이가 망통으로 처리
        ranking <- round(rank(round(vec)))
        return(ranking)
      }
    }else{ # 암행어사일  때
      if(sum(vec %in% 2) > 0){ # 13,18광땡이 있어서 암행어사가 실행될 때
        for(i in 1:3){#암행어사에 0번 순위를 매긴다.
          if(vec[i] == 27.3){
            vec[i] <- 0
            ranking <- round(rank(round(vec)))
            return(ranking)
          }
        }#for문 괄호
        return(ranking)
      }else{ # 광땡이 없어서 암행어사가 1끗으로 처리
        ranking <- round(rank(round(vec)))
        return(ranking)
      }
    }
  }else{ #특수패가 없을 때
    ranking <- round(rank(round(vec)))
    return(ranking)
  }
}



################################################

# ----play : 패 돌리고 랭크 생성
play <- function(x){ 
  for(i in 1:x){
    idx <- sample(1:20) # 카드 섞기
    CARD <- CARD[idx,]
    p1 <- c(CARD[1,], CARD[2,]) #p1 카드 2 장받기
    p2 <- c(CARD[3,], CARD[4,]) #p2 카드 2 장받기
    p3 <- c(CARD[5,], CARD[6,]) #p3 카드 2 장받기
    p1.rank <- ranking(p1[1:2], p1[3:4]) # 절대랭크 생성
    p2.rank <- ranking(p2[1:2], p2[3:4])
    p3.rank <- ranking(p3[1:2], p3[3:4])
    result <- game(p1.rank, p2.rank, p3.rank) # 상대랭크 생성
    p1.ranking <- result[1]
    p2.ranking <- result[2]
    p3.ranking <- result[3]
    absrank <- c(p1.rank, p2.rank, p3.rank)
    name <- c()
    winlose <- c()
    for(j in 1:3){
      if(absrank[j] <= 18){
        for(k in 1:nrow(jokbo)){
          if(absrank[j] == jokbo[k, "rank"]){
            name <- c(name, jokbo[k, "jokbo"])    
            break
          }
        }
      }
      else if(absrank[j] == 25.3){
        name <- c(name, "구사")
      }
      else if(absrank[j] == 28.3){
        name <- c(name, "땡잡이")
      }
      else if(absrank[j] == 27.3){
        name <- c(name, "암행어사")
      }else if(absrank[j] == 19){
        name <- c(name, "갑오")
      }else if(absrank[j] == 28){
        name <- c(name, "망통") 
      }
      else{
        name <- c(name, paste0((28 - absrank[j]),"끗"))
      }
    }
    if(result[1] == 1){
      winlose <- c(winlose, "win")
    }else{
      winlose <- c(winlose, "lose")
    }
    if(i == 1){
      game.result <- data.frame(p1[1], p1[2], p1[3], p1[4], p2[1], p2[2], p2[3], p2[4], 
                                p3[1], p3[2], p3[3], p3[4], p1.ranking, p2.ranking, p3.ranking, 
                                name[1], name[2], name[3], winlose)
    }else{
      game.result <- rbind(game.result, 
                           data.frame(p1[1], p1[2], p1[3], p1[4], p2[1], p2[2], p2[3], p2[4], 
                                      p3[1], p3[2], p3[3], p3[4], 
                                      p1.ranking, p2.ranking, p3.ranking,
                                      name[1], name[2], name[3], winlose))
    }
    rm(name)
  }
  return(game.result)
}


#### game play~
a <- play(100)

colnames(a) <- c("p1_num1_m", "p1_num1_s", "p1_num2_m", "p1_num2_s",
                           "p2_num1_m", "p2_num1_s", "p2_num2_m", "p2_num2_s",
                           "p3_num1_m", "p3_num1_s", "p3_num2_m", "p3_num2_s",
                           "p1rank", "p2rank", "p3rank", "p1jokbo", "p2jokbo", "p3jokbo", "win/lose")
writexl::write_xlsx(a, path = "masterdata.xlsx")

# 1,2행은 p1 첫 카드, 3,4행은 p2 두 번째 카드, 5,6행은 p2 첫 카드, 7,8행은 p2 두 번째 카드 등으로 진행되며, 13번째 행은 p1의 족보 등수 14는 p2의 족보 등수, 15는 p3의 족보 등수, 16은 p1의 게임 등수, 17은 p2 게임 등수, 18은 p3의 게임 등수를 추출합니다.
# 등수는 49파토일 경우 000을 배열하고, 공동 일등일 경우 223, 공동 이등일 경우 122일 배열했습니다.