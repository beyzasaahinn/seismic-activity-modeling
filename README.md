Bu proje, R programlama dilini kullanarak `Significant_Earthquakes` veri seti üzerinde lojistik regresyon, Ridge, Lasso ve Elastic Net modellerini uygulayarak depremlerin "şiddetli" olup olmadığını sınıflandırmayı amaçlamaktadır.

# Bu projeyi çalıştırabilmek için R'da aşağıdaki paketlerin kurulu olması gerekmektedir. 
Eğer kurulu değillerse, R konsolunuzda aşağıdaki komutları çalıştırarak yükleyebilirsiniz:
install.packages(c("tidyverse", "rsample", "caret", "pROC", "glmnet"))

## Kütüphaneleri çağırmak için 
library(tidyverse)
library(rsample)
library(caret)
library(pROC)
library(glmnet)

Projenin çıktıları, farklı lojistik regresyon türlerinin deprem verisi üzerindeki tahmin yeteneklerini göstermektedir.
