* Семинар 6.

clear

set more off // отключить полный вывод результатов работы команд
cd /Users/polinapogorelova/Desktop/СМЭФ_Эконометрика/Эконометрика_25_26 // директория, в которой хранятся файлы
log using sem_6.log, text replace // начало записи результатов do-файла в sem_6.log

* Модели бинарного выбора

* MROZ — это широко известный в эконометрике и статистике набор данных, основанный на исследовании T. A. Mroz 1987 года. 
* Он содержит информацию о 753 замужних женщинах, взятуую из волны 1976 года Панельного исследования динамики доходов (Panel Study of Income Dynamics, PSID) в США.
* Основная цель этого датасета — анализ предложения труда и факторов, влияющих на участие замужних женщин в рабочей силе.
* Данные представлены в нескольких распространенных версиях, которые различаются по набору переменных. Ниже приведено описание основных из них.
* Показатели занятости: находится ли женщина в составе рабочей силы (inlf), количество отработанных часов (hours).
* Демографические характеристики: возраст (age), количество детей младше 6 лет (kidslt6) и детей от 6 до 18 лет (kidsge6).
* Образование и доход: уровень образования жены и мужа в годах (educ, huseduc), заработная плата (wage), доход семьи (faminc).
* Дополнительные переменные: данные о родителях жены (образование матери motheduc и отца fatheduc), опыт работы (exper) и другие.

use mroz.dta
describe
summarize
tab inlf city

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

* Сравним коэффициенты при образовании в logit и probit моделях
logit inlf educ exper age kidslt6 kidsge6 
est store logit_mod
local b_logit = _b[educ]

probit inlf educ exper  age kidslt6 kidsge6 
est store probit_mod
local b_probit = _b[educ]

* Считаем отношение
local ratio = `b_logit' / `b_probit'
display "Отношение logit-коэфф / probit-коэфф = " `ratio'


* Ordered logit regression

* Этот синтетический набор данных содержит переменную apply, имеющую три уровня: «маловероятно» (unlikely), 
* «отчасти вероятно» (somewhat likely) и «очень вероятно» (very likely), которые закодированы как 1, 2 и 3 соответственно. 
* Эта переменная будет использоваться в качестве зависимой (выходной) переменной. У нас также есть три переменные, которые 
* будут использоваться в качестве предикторов: pared (бинарная переменная: 1 — хотя бы один из родителей имеет диплом о 
* высшем образовании (магистра или доктора), 0 — нет); public (бинарная переменная: 1 — обучение на бакалавриате проходило 
* в государственном вузе, 0 — в частном) и gpa (средний балл успеваемости студента). 

use ologit.dta, clear
tab apply
tab apply, nolabel
tab apply public
sum gpa

ologit apply i.pared i.public gpa // оценим порядковый логит. Коэффициенты показывают во сколько раз увеличится логарифм отношения шансов при изменении объясняющей переменной на 1 единицу
brant, detail
ologit apply i.pared i.public gpa, or // получим оценки изменения отношения шансов (odds ratio) перейти с одного уровня на более высокий
listcoef, help

margins, at(pared = (0/1)) predict(outcome(0)) atmeans // прогнозируемая вероятность попасть в низшую категорию в зависимости от значения категориальной переменной "pared"
margins, at(pared = (0/1)) predict(outcome(1)) atmeans // прогнозируемая вероятность попасть в среднюю категорию в зависимости от значения категориальной переменной "pared"
margins, at(pared = (0/1)) predict(outcome(2)) atmeans // прогнозируемая вероятность попасть в высшую категорию в зависимости от значения категориальной переменной "pared"

forvalues i = 0/2 {
  margins, at(gpa = 3.5 pared = 1 public = 1) predict(outcome(`i'))
} // прогноз вероятности попасть в каждую категорию для индивидуума, имеющего GPA равный 3.5, обучавшегося в частной школе и у которого хотя бы один из родителей обучался в аспирантуре

net search omodel
omodel logit apply pared public gpa // альтернативная команда для оценивания упорядоченных моделей логит и пробит, содержащая также реультаты теста на пропорциональность шансов (test for the equivalence of the estimates for cut levels)
predict prob_unlikely prob_somewhatlikely prob_verylikely, pr // прогнозирование вероятностей попасть в каждую категорию



* Модель множественного выбора (Multinomial logistic regression)
* Файл keane.dta взят из архива данных к учебнику Джеффри Вулдриджа «Эконометрический анализ перекрёстных и панельных данных» (EACSAP). 
* Набор данных, скорее всего, связан с известным исследованием Майкла П. Кина (Michael P. Keane) и используется для иллюстрации моделей 
* панельных данных или дискретного выбора. Обычно такие данные содержат информацию о:
* заработной плате, статусе занятости, образовании, возрасте, опыте работы, других социально-экономических характеристиках индивидов за 
* несколько периодов времени.

use keane.dta, clear
keep if year == 87
sum
drop if missing(status)
label define status 1 "school" 2 "home" 3 "work"
label values status status
table status, con(mean educ sd educ)
tab status black, chi2 // Хи2-критерий для проверки независимости двух факторов. H0: переменные независимы

mlogit status educ exper expersq i.black, base(1)
predict p_1 p_2 p_3, pr // прогноз вероятности принадлежности индивидуума к каждой категории
mlogit, rrr // relative-risk ratios (показывает, чему равно отношение вероятностей принадлежности к разным группам при увеличении объясняющей переменной на 1 единицу)
test [home]educ = [work]educ

margins black, pr(out(1))
margins, dydx(black) atmeans predict(out(1)) // предельный эффект, посчитанный для "среднего" наблюдения

margins black, atmeans pr(out(1)) // прогноз вероятности выбрать школу (y=1) в зависимости от значения факторной переменной black при условии, что все остальные переменные выбраны на их среднем уровне
marginsplot, name(schhol)
graph export scool, as(png) width(600) height(450) replace
margins black, atmeans pr(out(2))
marginsplot, name(home)
margins black, atmeans pr(out(3))
marginsplot, name(work)
graph combine school home work, ycommon

margins black, pr(out(3)) // средняя вероятность того, что y=3 на каждом уровне факторной переменной black

margins, at(educ = (8(2)18)) predict(outcome(1)) vsquish // средняя вероятность того, что y=1 на каждом уровне количественной переменной educ (от 8 до 18 с шагом равным 2 годам)
margins, at(educ = (8(2)18)) predict(outcome(2)) vsquish
margins, at(educ = (8(2)18)) predict(outcome(3)) vsquish

fitstat // проверка качества модели



* Пуассоновская регрессия
* Файл crime.dta — это классический набор данных из учебника Джеффри Вулдриджа «Эконометрический анализ перекрёстных и панельных данных»
* (EACSAP). Он содержит информацию об уровне преступности и социально-экономических характеристиках округов США (обычно за 1987 год). 
* Данные используются для иллюстрации регрессионного анализа, в частности, при изучении факторов, влияющих на преступность.
* Данные основаны на статистике ФБР (Uniform Crime Reports) и переписи населения США (Census Bureau). Они объединяют показатели 
* преступности, работы полиции, демографии и экономики по округам.
use crime.dta, clear
hist narr86, discrete scheme(sr1mono) title("Гистограмма распределения числа преступлений")

reg narr86 pcnv avgsen tottime ptime86 qemp86 inc86 durat black hispan born60, vce(robust) // оценим линейную модель с помощью МНК
predict narr86_f_ols

poisson narr86 pcnv avgsen tottime ptime86 qemp86 inc86 durat black hispan born60, vce(robust) // оценим пуассоновскую регрессию. Для получения робастных стандартных ошибок для оценок параметров используем vce(robust). Коэффициент при xj показывает, насколько увеличится log(narr86) при увеличении xj на 1 единицу
poisson, irr //  incident rate ratios - показывает, во сколько раз увеличится число преступлений при изменении xj на 1 единиц

poisson narr86 pcnv avgsen tottime ptime86 qemp86 inc86 durat black hispan born60, vce(robust) 
test black = hispan

predict narr86_f_p, n // прогноз числа преступлений
estat gof // goodness-of-fit H0: модель адекватна
fitstat

poisson narr86 pcnv avgsen tottime ptime86 qemp86 inc86 durat i.black hispan born60, vce(robust) 
margins i.black, atmeans // вычислим прогнозируемое количество преступлений на каждом уровне переменной black, сохраняя все другие переменные в модели на их среднем уровне

separate narr86_f_p, by(black)
twoway scatter narr86_f_p0 narr86_f_p1 inc86, connect(l l) sort ///
       ytitle("Predicted Count of Crimes") ylabel( ,nogrid) legend(rows(3)) ///
       legend(ring(0) position(10)) scheme(sr1mono)
	   

* Negative binomial regression analysis
use crime.dta, clear

nbreg narr86 pcnv avgsen tottime ptime86 qemp86 inc86 durat black hispan born60, dispersion(constant) nolog // NB-I регрессия
nbreg, irr // incidence-rate ratios = exp(b_k) показывает, во скольок раз изменится зависимая пермеенная (частота события) при увеличении объясняющей переменной на 1 единицу
predict narr86_f_nb1, n
predict narr_12, pr(1,2) // прогноз вероятности P(a <= y_j <= b), где a=1, b=2 для данного случая
summarize narr86 numnarr narr_12

nbreg narr86 pcnv avgsen tottime ptime86 qemp86 inc86 durat i.black i.hispan i.born60, dispersion(mean) nolog // NB-II регрессия
predict narr86_f_nb2, n
test black hispan

margins i.black, atmeans // прогнозируемое количество преступлений в зависимости от значения переменной black при среднем уровне всех остальных переменных
margins, at(avgsen = (5(1)10)) vsquish // среднее количество преступлений, рассчитанное для avgsen={5,6,7,8,9,10}
margins, at(avgsen = (5(1)10)) atmeans // количество преступлений, рассчитанное для avgsen={5,6,7,8,9,10} при использовании средних значений для остальных переменных
fitstat

separate narr86_f_nb1, by(black)
twoway scatter narr86_f_nb10 narr86_f_nb11 inc86, connect(l l) sort ///
       ytitle("Predicted Count of Crimes") ylabel( ,nogrid) legend(rows(3)) ///
       legend(ring(0) position(10)) scheme(sr1mono)

log close // завершаем запись в файл
