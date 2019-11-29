#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#---------------------------WAR: Probabilidades de Conquista de Território-------------------------# 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# 0. Initial Settings -----------------------------------------------------
source("my_PACKAGES.R")

# 1. Função de Probabilidades ---------------------------------------------
WAR <- function(N_ataque = 5, N_defesa = 3, nsims = 5000){
        
        #### Initial Settings ####
        DADOS <- 1:6
        probs <- 1/6
        Ataque <- Defesa <- rep(NA, nsims)
        i=1
        for(i in 1:nsims){
                
                est_ata <- N_ataque
                est_def <- N_defesa
                
                while (est_ata > 1 && est_def != 0) {
                
                        if(est_ata > 3){
                                tropas_ata <- 3
                        }else{
                                tropas_ata <- est_ata-1
                        }
                        
                        if(est_def >= 3){
                                tropas_def <- 3
                        }else{
                                tropas_def <- est_def
                        }
                        
                        dados_ata <- sort(sample(x = DADOS, size = tropas_ata, replace = TRUE, prob = rep(probs,6)), decreasing = TRUE)
                        N_ata     <- length(dados_ata)
                        
                        dados_def <- sort(sample(x = DADOS, size = tropas_def, replace = TRUE, prob = rep(probs,6)), decreasing = TRUE)
                        dados_def <- dados_def[1:N_ata]
                        
                        saldo_ata <- -sum(dados_ata <= dados_def, na.rm = T)
                        saldo_def <- -sum(dados_ata >  dados_def, na.rm = T)
                        
                        est_ata <- est_ata + saldo_ata
                        est_def <- est_def + saldo_def
                
                }
                
                if(est_ata <= 1 && est_def > 0){
                        Defesa[i] <- 1
                        Ataque[i] <- 0
                }else{
                        Defesa[i] <- 0
                        Ataque[i] <- 1
                }
        }
        
        #### Final Output ####  
        # beepr::beep()
        out <- sum(Ataque)/nsims
        return(out)       
}


# 2. Teste da Função ------------------------------------------------------
(teste1 <- WAR(N_ataque = 20, N_defesa = 20, nsims = 5000))


# 3. Simulações de Monte Carlo --------------------------------------------
MATRIZ <- matrix(data = NA, nrow = 20, ncol = 20)
rownames(MATRIZ) <- colnames(MATRIZ) <- 1:20

tic("Teste")
for(i in 1:nrow(MATRIZ)){
        for(j in 1:ncol(MATRIZ)){
                print(i)
                print(j)
                MATRIZ[i,j] <- WAR(N_ataque = i+1, N_defesa = j, nsims = 5000)
        }
}
beepr::beep()
toc()

write.table(MATRIZ, 'clipboard', sep='\t')


MATRIZ2 <- matrix(data = NA, nrow = 5, ncol = 5)
rownames(MATRIZ2) <- colnames(MATRIZ2) <- seq(30,50,by = 5)

tic("Teste")
for(i in 1:nrow(MATRIZ2)){
        for(j in 1:ncol(MATRIZ2)){
                print(i)
                print(j)
                MATRIZ2[i,j] <- WAR(N_ataque = as.numeric(rownames(MATRIZ2)[i])+1, N_defesa = as.numeric(rownames(MATRIZ2)[j]), nsims = 5000)
        }
}
beepr::beep()
toc()

write.table(MATRIZ2, 'clipboard', sep='\t')


# 4. Final ----------------------------------------------------------------
rm(i,j,teste1,MATRIZ, MATRIZ2)



























