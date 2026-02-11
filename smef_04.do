* Семинар 4.

clear

set more off // отключить полный вывод результатов работы команд
cd /Users/polinapogorelova/Desktop/СМЭФ_Эконометрика/Эконометрика_24_25 // директория, в которой хранятся файлы
log using seminar4.log, text replace // начало записи результатов do-файла в seminar3.log

* Эндогенность
clear
use CARD.DTA
describe // описание набора данных
summarize // описательные статистики переменных

* 1. Метод наименьших квадратов
reg lwage educ exper expersq black south smsa smsa66 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669
est store ols
estat ovtest // RESET-test  (тест Рамсея). Ho: нет пропущенных переменных
predict yhat, xb // сохраняем прогноз по модели
tabstat yhat, statistics(mean) by(south) // среднее значение прогноза по двум категориям бинарной переменной south (tabstat отображает сводную статистику для ряда числовых переменных в одной таблице)

* 2. Метод инструментальных переменных и 2-МНК
* Проанализируем, какие из имеющихся переменных могут быть инструментами для educ

* nearc4 - инструмент для educ
ivregress 2sls lwage (educ = nearc4) exper expersq black south smsa smsa66 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669, first
est store iv

estat firststage // тестирование релевантности (качества) инструментов. H0: инструменты слабые
hausman iv ols // тест Хаусмана на проверку эндогенности регрессоров. H0: OLS-оценки. H1: IV-оценки

* nearc4 и nearc2 - инструменты для educ
ivregress 2sls lwage (educ = nearc4 nearc2) exper expersq black south smsa smsa66 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669, first
est store iv2
estat firststage // тестирование релевантности (качества) инструментов. H0: инструменты слабые
estat overid // тестирование валидности (экзогенности) инструментов (l>=k) (тест Саргана). H0: все инструменты экзогенные (не коррелируют с ошибкой). H1: хотя бы один из инструментов эндогенный
hausman iv2 ols // тест Хаусмана на проверку эндогенности регрессоров. H0: OLS-оценки. H1: IV-оценки

* fatheduc, motheduc - инструменты для educ
ivregress 2sls lwage (educ = fatheduc motheduc) educ exper expersq black south smsa smsa66 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669, first
est store iv3

* Тестирование инструментов
estat firststage // тестирование релевантности (качества) инструментов. H0: инструменты слабые
estat overid // тестирование валидности (экзогенности) инструментов (l>=k) (тест Саргана). H0: все инструменты экзогенные (не коррелируют с ошибкой). H1: хотя бы один из инструментов эндогенный
hausman iv3 ols // тест Хаусмана на проверку эндогенности регрессоров. H0: OLS-оценки. H1: IV-оценки

* nearc4, fatheduc, motheduc - инструменты для educ
ivregress 2sls lwage (educ = nearc4 fatheduc motheduc) educ exper expersq black south smsa smsa66 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669, first
estat iv4
estat firststage // тест на релевантность инструментов
estat overid // тест Саргана
hausman iv4 ols // тест Хаусмана


* Модели бинарного выбора

* Logit model
use http://www.stata.com/data/jwooldridge/eacsap/mroz, clear
describe
summarize
tab inlf city

reg inlf educ exper expersq age kidslt6 kidsge6 mtr huswage // оценим линейную модель вероятности
predict prob_ols

logit inlf educ exper  age kidslt6 kidsge6 // оценивание logit-модели с помощью ММП (логистические шансы). Коэффициент при переменной показывает, во сколько раз изменится логарифм отношения P("удачи")/P("неудачи")
logit, or // odds ratio (отношение шансов)
predict xb0, xb // прогнозирование латентной переменной
predict pl0, pr // прогнозирование вероятности "успеха"
sum pl0 // описательная статистика предсказанной вероятности

test kidslt6 = kidsge6 = 0 // тестирование гипотезы о незначимости переменных, характеризующих состав детей в семье

margins, dydx(*) // усреднённые предельные эффекты (average marginal effects)
margins, dydx(*) atmeans // предельные эффекты для "среднего" наблюдения. ПЭ для переменной xj показывает, на сколько изменится вероятность "успеха" при увеличении xj на 1 единицу
margins, at(educ = (10(2)20)) // средняя вероятность "успеха" на каждом уровне educ от 10 до 20 лет с шагом 2 года
margins, at(educ = (10(2)20)) vsquish // средняя вероятность "успеха" на каждом уровне educ от 10 до 20 лет с шагом 2 года
margins, at(educ = (10(2)20)) atmeans // вероятность "успеха" на каждом уровне educ от 10 до 20 лет с шагом 2 года при средних значениях остальных переменных


* Сравним коэффициенты при образровании в logit и probit моделях
logit inlf educ exper  age kidslt6 kidsge6 
est store logit_mod
local b_logit = _b[educ]

probit inlf educ exper  age kidslt6 kidsge6 
est store probit_mod
local b_probit = _b[educ]

* Считаем отношение
local ratio = `b_logit' / `b_probit'
display "Отношение logit-коэфф / probit-коэфф = " `ratio'

log close // заканчиваем запись в файл
