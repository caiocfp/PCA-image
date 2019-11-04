####pacotes####

library(imager) ###carrega imagens###
library(here)  #ajuda a setar o path#
library(broom)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)


###vamos carregar a imagem##

img <- load.image("C:/Users/Admin/Desktop/gauge_comuinicados/perfil.jpg")

str(img)

##podemos usar o dim(img) sabermos as dimensões entre pixels 

dim(img)

# 486 por 486 de pixels #

##transformar em data frame##

img_df_long <- as.data.frame(img)


head(img_df_long)
tail(img_df_long)

##Para fazer um PCA, nós podemos criar uma matriz com as dimensões que respeitem 
## as proporcoes da foto importada###

#para isso é necessário usar a função tidyr::pivot_wider 

img_df <- tidyr::pivot_wider(img_df_long,
                               names_from = y,
                               values_from = value)

###aqui as dimensões são mantidas###
dim(img_df)

img_df[1:5, 1:5]

###pca####

img_pca <- img_df %>%
  dplyr::select(-x,-cc) %>%
  prcomp(scale = T,
         center = T)

summary(img_pca)


### organizar o pca COM O tidy ###

pca_tidy <- tidy(img_pca,
                 matrix = "pcs")

pca_tidy %>%
  ggplot(aes(x = PC,
             y = percent)) +
  geom_line() +
  labs(x = "Principal Component",
       y = "Variance Explained")


###revertendo o PCA###

names(img_pca)


#sdev corresponde aos desvios padrões dos componentes princiais
#rotation corresponde à rotação da matriz de componentes principais
#x é uma matriz dos valores rotacionados multiplicados pela matriz de rotacao


##Para revertermos os o pca temos que: x multiplicado pela rotation transposta e tirar a normalização e o centro


reverse_pca <- function(n_comp = 20, pca_object = img_pca){
  recon <- pca_object$x[, 1:n_comp] %*% t(pca_object$rotation[, 1:n_comp])
  
  if(all(pca_object$scale != FALSE)){
  
    recon <- scale(recon, center = FALSE, scale = 1/pca_object$scale)
  }
  if(all(pca_object$center != FALSE)){

    recon <- scale(recon, scale = FALSE, center = -1 * pca_object$center)
  }

  recon_df <- data.frame(cbind(1:nrow(recon), recon))
  colnames(recon_df) <- c("x", 1:(ncol(recon_df)-1))
  

  recon_df_long <- recon_df %>%
    tidyr::pivot_longer(cols = -x, 
                        names_to = "y", 
                        values_to = "value") %>%
    mutate(y = as.numeric(y)) %>%
    arrange(y) %>%
    as.data.frame()
  
  recon_df_long
}                



# a sequencia de PCAs #

n_pcs <- c(2:5, 10, 20, 50, 100)

names(n_pcs) <- paste("First",
                      n_pcs,
                      "Components",
                      sep = "_")


#mapear o reverse_pca#

recovered_imgs <- map_dfr(n_pcs, 
                          reverse_pca, 
                          .id = "pcs") %>%
  mutate(pcs = stringr::str_replace_all(pcs, "_", " "), 
         pcs = factor(pcs, levels = unique(pcs), ordered = TRUE))




p <- ggplot(data = recovered_imgs, 
            mapping = aes(x = x, y = y, fill = value))
p_out <- p + geom_raster() + 
  scale_y_reverse() + 
  scale_fill_gradient(low = "black", high = "white") +
  facet_wrap(~ pcs, ncol = 2) + 
  guides(fill = FALSE) + 
  labs(title = "Recuperação de imagem de fotos") + 
  theme(strip.text = element_text(face = "bold", size = rel(1.2)),
        plot.title = element_text(size = rel(1.5)))

p_out
