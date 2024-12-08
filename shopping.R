# Etapa 1: Instalar e carregar pacotes necessários
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if (!require(corrplot)) install.packages("corrplot", dependencies = TRUE)

library(ggplot2)
library(dplyr)
library(corrplot)

# Etapa 2: Importar o dataset
dados <- read.csv("C:\\Users\\Rodrigo\\Project\\Datascience\\Input\\shopping_trends.csv", 
                  header = TRUE, sep = ",")

# Visualizar os dados
head(dados)          # Primeiras linhas do dataset
str(dados)           # Estrutura do dataset
summary(dados)       # Estatísticas descritivas
dim(dados)           # Dimensão do dataset
colnames(dados)      # Nomes das colunas

# Etapa 3: Limpeza dos dados
dados <- na.omit(dados)  # Remover linhas com valores ausentes (NA)

# Renomear colunas para simplificar
colnames(dados) <- c("CustomerID", "Age", "Gender", "ItemPurchased", "Category",
                     "PurchaseAmount", "Location", "Size", "Color", "Season",
                     "ReviewRating", "SubscriptionStatus", "PaymentMethod",
                     "ShippingType", "DiscountApplied", "PromoCodeUsed",
                     "PreviousPurchases", "PreferredPaymentMethod", "PurchaseFrequency")

# Etapa 4: Análise univariada
# Estatísticas descritivas para variáveis numéricas
numerical_cols <- dados[, sapply(dados, is.numeric)]
summary(numerical_cols)

# Histograma para Idade
ggplot(dados, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribuição de Idades", x = "Idade", y = "Frequência")

# Gráfico de barras para Gênero
ggplot(dados, aes(x = Gender)) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Distribuição por Gênero", x = "Gênero", y = "Frequência")

# Boxplot para variáveis numéricas
boxplot(numerical_cols, main = "Boxplot de Variáveis Numéricas", col = "lightblue")

# Frequência para variáveis categóricas
table(dados$Category)         # Categoria de produto
table(dados$PaymentMethod)    # Métodos de pagamento

# Etapa 5: Análise bivariada
# Relação entre Idade e Gênero
ggplot(dados, aes(x = Gender, y = Age, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Idade por Gênero", x = "Gênero", y = "Idade")

# Tabela cruzada: Método de pagamento por categoria de produto
payment_category <- table(dados$PaymentMethod, dados$Category)
barplot(payment_category, beside = TRUE, legend = TRUE, col = rainbow(ncol(payment_category)),
        main = "Método de Pagamento por Categoria de Produto")

# Correlação entre variáveis numéricas
correlation_matrix <- cor(numerical_cols, use = "complete.obs")
corrplot(correlation_matrix, method = "circle", type = "upper", tl.cex = 0.8)

# Etapa 6: Detecção de outliers
boxplot.stats(dados$Age)$out          # Outliers em Idade
boxplot.stats(dados$PurchaseAmount)$out   # Outliers no valor da compra

# Etapa 7: Agregações e resumos
# Total de compras por categoria de produto
sales_by_category <- dados %>%
  group_by(Category) %>%
  summarise(TotalSales = sum(PurchaseAmount, na.rm = TRUE)) %>%
  arrange(desc(TotalSales))

print(sales_by_category)

# Gráfico de barras: Total de vendas por categoria
ggplot(sales_by_category, aes(x = reorder(Category, -TotalSales), y = TotalSales)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  labs(title = "Total de Vendas por Categoria", x = "Categoria", y = "Total de Vendas")

# Etapa 8: Exportar resultados
write.csv(sales_by_category, "C:\\Users\\Rodrigo\\Project\\Datascience\\Output\\sales_by_category.csv",
          row.names = FALSE)

# Conclusão
cat("Análise estatística concluída. Resultados salvos no diretório especificado.\n")

