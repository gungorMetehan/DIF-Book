# **Sunuş ve Kitap Hakkında**

Erken dönemlerde “test yanlılığı (test bias)”, devamında “madde yanlılığı (item bias)” ve evrilmiş tanımlamasıyla “Değişen Madde Fonksiyonu (DMF)” analizleri, eğitim ve psikolojik ölçmelerde, testlerin adilliğini, geçerliliğini ve gruplar arası puan karşılaştırılabilirliğini değerlendirmede yüksek vurguyla önerilmektedir. İlgilenilen özellikle ilişkili birtakım geri plan
özellikleri açısından çeşitlilik içeren gruplara uygulanan testlerde DMF’nin belirlenmesi ve yorumlanması, test geliştirme, uyarlama ve değerlendirme süreçlerinin temel bir bileşeni haline gelmiştir. Bu kapsamda, psikometrinin son yüz yıllık gelişiminde, teorik temelleri, varsayımları,
istatistiksel özellikleri ve pratik uygulamaları bakımından farklılık gösteren çok çeşitli DMF belirleme teknikleri önerilmiştir.

Bu kitap, katkıda bulunan tüm yazarlarımızla birlikte, Madde Tepki Kuramı (MTK) ve uygulamaları üzerinde uzun yıllardır süren etkileşimli ve işbirlikli çalışmalarımızın ve çabalarımızın bir ürünüdür. Yazarların tamamı, MTK ve MTK uygulamaları kapsamında hem lisansüstü eğitimleri süresince hem sonrasında, kapsamlı ve teknik açıdan güçlü çalışmalarıyla alana
katkılar sağlamış ve sağlamaya devam etmektedir. Bu deneyim ve yetkinliklerimizi, alana katkı sağlayacağına inandığımız özel bir kitapta bir araya getirmeyi ve paylaşmayı, sorumluluğumuz ve alanımıza bir vefa borcumuz olarak gördük. Bu bağlamda, ölçme ve değerlendirme uzmanları, psikometristler, test geliştiriciler ve araştırmacıların yanı sıra ilgili alanlardaki doktora ve
doktora sonrası öğrencileri için yararlanılabilecek bir temel kaynak eser ortaya çıkarmaya çalıştık. Amacımız; yaygın ve operasyonel kullanımı olan DMF tekniklerini tanımlamak ve tanıtmanın yanı sıra, okuyuculara bu tekniklerin uygulanmasına yönelik gerekli kavramsal ve teknik
ayrıntıları sağlamak ve pratik uygulamalarla bu yetkinlikleri pekiştirmek ve desteklemektir.

Belirgin bir sınırlılık olarak bu kitap, tek boyutlu yapılara ve ikili puanlanan (dichotomous) maddelere yönelik DMF tekniklerine odaklıdır. İlgili alan, çok boyutlu ve karmaşık modellere doğru evrilmekle birlikte, tek boyutluluk altında çalışan teknikler ve modeller yorumlanabilirliği, metodolojik netliği ve uygulanabilirliği gibi yönleriyle operasyonel test ortamlarında yaygın
olarak kullanılmaya devam etmektedir. Bu kapsamda, bu kitap, tek boyutlu yapılara yönelik modeller sınırlılığında hem MTK tabanlı olmayan hem de MTK tabanlı DMF belirleme yöntem ve tekniklerini ele almaktadır. Bunların yanı sıra bu yaygın ve temel modelleri genişleten karma-MTK yaklaşımıyla da konuya faklı bir yön çizmektedir.

DMF belirleme ve incelemeye yönelik yöntem ve tekniklerin yedi bölümde ele alındığı bu kitapta, her bölüm, ilgili teknik veya tekniklerin; (1) kuramsal ve algoritmik geri planı, (2) temel varsayımları, (3) temel istatistiksel çıktıları ve bunların yorumlanması ve (4) bazı diğer özgün
teknik ayrıntılar olmak üzere ayrıntılı bir şekilde tartışılmakta ve tanıtılmaktadır. Bu kuramsal zemin üzerine, ilk bölüm hariç, diğer bölümler, R yazılım dili kullanılarak yürütülen örnek uygulamalar içermektedir.

Birinci bölüm; temel terminoloji ve tanımlar, DMF türleri, DMF belirleme tekniklerinin sınıflandırılması da dahil olmak üzere, DMF ile ilgili temel kuramsal ve teknik bilgileri, açıklama ve tartışmaları içermektedir. İkinci bölüm, erken dönem ve MTK tabanlı olmayan yöntemlere odaklanmaktadır. Üçüncü bölüm, yaygın DMF belirleme tekniklerinden biri olan Mantel-Haenszel ki-kare yöntemine; dördüncü bölüm, hem tek biçimli hem de tek biçimli olmayan
DMF’yi incelemede kullanılabilen esnek bir teknik olarak lojistik regresyon tekniğine; beşinci bölüm, algoritmik geri planıyla MTK tabanlı yöntemler arasında kabul edilebilen SIBTEST tekniğine ayrılmıştır. Altıncı bölüm, Raju’nun alan ölçüleri, Lord’un ki-kare testi, Lordif ve
TSW-olasılık oranı testini içermek üzere tek boyutlu MTK tabanlı DMF tekniklerine ve yedinci bölüm ise DMF belirlemede karma-MTK yaklaşım ve modellerinin kullanımına yöneliktir. Her bölüm, okuyucuların daha fazla teorik ve uygulamalı çalışma yapmalarına olanak tanıyan kendi
referans ve kaynak listesiyle sona ermektedir.

# **Veri Seti**
Bu kitabın ilk ve yedinci bölümleri hariç diğer bölümleri, ilgili tekniğin R tabanlı örnek uygulamalarını içermektedir. Örnek uygulamaların tamamında “dataDIF” olarak isimlendirilmiş olan örnek bir veri seti kullanılmıştır. Kitapta sunulan tüm analizler bu veri seti kullanılarak
gerçekleştirilmiştir ve her uygulama için eksiksiz R kodu ilgili bölümlerin sonunda ek olarak verilmiştir. Bu veri seti, bu kitap için simülasyon tabanlı olarak oluşturulmuş olup, 20 test maddesi ve 1000 katılımcıya ilişkin bilgileri içermektedir. Katılımcıların 700’ü referans, diğerleri odak
gruba atanmıştır. Veri setindeki maddelerden altısı, farklı DMF türlerini temsil edecek şekilde tasarlanmıştır. Ortak bir veri setinin kullanılmasının, okuyucuların teknikleri karşılaştırmasına ve farklı tekniklerin aynı DMF yapısına nasıl yanıt verdiğini daha iyi anlamasına imkân sağlayabileceği düşünülmektedir. Bu örnek veri setine, aşağıda verilen bağlantılar kullanılarak, ister
.csv ister .xlsx formatında, doğrudan indirebilecek şekilde ulaşılabilmektedir.

CSV formatı : https://erguldemir.github.io/dataset/dataDIF.csv

Excel formatı : https://erguldemir.github.io/dataset/dataDIF.xlsx
