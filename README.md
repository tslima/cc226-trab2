Tarefa #2 – CC226 Introdução à Análise de Padrões – Prof. Carlos Henrique Q. Forster
Para 5/10/2016 – relatório PDF
Regressão e Análise de Componentes
A tarefa consiste em 5 análises que devem ser construídas com linguagem de programação, funções
gráficas e biblioteca de algebra linear e eventualmente de estatística.
I – Análise utilizando a regressão linear múltipla.
Base: bodyfat http://lib.stat.cmu.edu/datasets/bodyfat
Procurar uma estimação para a porcentagem de gordura utilizando as circunferências do corpo.
Através de tentativas, selecionar um conjunto mínimo de variáveis para fazer a estimação.
Avaliar utilizando o erro RMS com validação cruzada com 10 dobras.
II – Análise utilizando regressão RBF (base de funções radiais)
Base: ukraine_ged40_simpl.csv extraída de http://ucdp.uu.se/ Uppsala Conflict Data Program
(ver pasta compartilhada Extras)
Analisar o conflito de Donbass, plotando um gráfico da intensidade dos conflitos. Colunas são:
latitude, longitude e número de fatalidades. Utilizar RBF ou KDE (kernel density estimation).
III – Análise preditiva de séries temporais. Utilizar regressão polinomial com regularização.
Base: http://lib.stat.cmu.edu/datasets/DJ30-1985-2003.zip Dow Jones
Prever valores de fechamento no horizonte de 1 ou mais dias e avaliar a qualidade da predição
apenas com dados passados.
IV – Análise de componentes principais e análise discriminante linear de Fisher
Base: https://archive.ics.uci.edu/ml/datasets/Pen-Based+Recognition+of+Handwritten+Digits
Projetar os 16 atributos no plano (redução de dimensionalidade para 2 eixos) e mostrar a
distribuição das classes. Visualizar o Scree plot. Plotar as figuras dos "eigendigits" encontrados.
