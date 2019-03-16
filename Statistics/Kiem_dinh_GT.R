install.packages("BSDA")
install.packages('stats')
install.packages('stats4')
install.packages('utils')
install.packages('binom')
install.packages('datasets')
install.packages('EnvStats')
install.packages('graphics')
install.packages('grDevices')
install.packages('lattice')
install.packages('methods')


library("BSDA")



##  BAI 1

#Dung Z.test vi: Da biet phuong sai = 0.08 va tuan theo pp chuan

x = c(11.2 , 12.4, 10.8, 11.6, 12.5, 10.1, 12.2, 10.6)

z.test(x, sigma.x = 0.08)



## Bai 2

# Dung Z.test do da biet phuong sai va do lech chuan 

# zsum boi vi ko biet duoc du lieu dau vao ma chi biet: n, mean, sigma
zsum.test(n.x = 20, mean.x = 1.2, sigma.x = 0.2, conf.level = 0.99)

##  Bai 3

# Dung tsum.test boi vi ko biet do lech chuan
# dung tsum boi khong biet du lieu 

tsum.test(n.x = 20, mean.x = 1.2, s.x= 0.04, conf.level = 0.99)

## Bai 4

# Dung T.test boi vi la pp chuan, n nho, ko biet phuong sai
x = c(330, 322, 345, 328.6, 331, 342, 342.4, 340, 329.7, 334, 326.5, 325.8, 337.5, 327.3, 322.6, 341, 340, 333, 343.3, 331, 341, 329.5, 332.3, 340)

t.test(x, conf.level = 0.99 )

## Bai 5

x = c(140, 136, 150, 144, 148, 152, 138, 141, 143, 151)

# UL hop ly cuc dai cua
# Gia tri TB = mean, boi vi day la pp chuan
mean(x)
# Phuong sai cua tong the
(length(x)-1)*sd(x)^2/length(x)

# UL phuong sai
varTest(x, conf.level = 0.95)   # add EnvStats library



### BT UL hop ly cuc dai

## UL Hop ly Phan phoi nhi thuc
str(mtcars)

length(mtcars$am)

x = mtcars$am

# Ham hop ly: L (x, p) = P(x = xi) = x^xi * (1-p)^(1-xi)
L = function(p, x) prod(dbinom(x, size = 1, prob = p))

# Tim UL hop ly cuc dai cua p: L(p*)  = max L(p), P thuoc (0, 1)

optimize(L, interval = c(0, 1), x = x, maximum = TRUE)

summary()

## UL Hop ly Phan phoi chuan

str(PlantGrowth)

x = PlantGrowth$weight

# tim UL hop ly cuc dai cua Ky vong, Phuong sai cua pp chuan: P bo cua trong luong cay: weight

# Ham hop ly pp chuan: f(x) = (1/ can(2 *pi) * sigma) * e^(-1.2 * ((x - mu)/sigma)
minuslogL = function(mu, sigma2) {-sum(dnorm(x, mean = mu, sd =sqrt(sigma2), log = TRUE))}

MaxLikeEst = mle(minuslogL, start = list(mu = 5, sigma2 = 0.5)) # add stats4 library

summary(MaxLikeEst)
                 
## Bai 7

# UL ty le

prop.test(p = 0.5016, conf.level = 0.99)


## Bai 8

prop.test(p = 0.16, conf.level = 0.95)

## Bai 9

x = c(8.18, 8.17, 8.16, 8.15, 8.17, 8.21, 8.22, 8.16, 8.19, 8.18)

# biet phuong sai, pp chuan

z.test(x, sigma.x = 0.02, mu = 8.2, conf.level = 0.95)

## Bai 10

# Dung alt = 'l' boi nghi ngo dieu gi ?

x= c(210, 198, 195, 202, 197.5, 196, 199, 195.5)

z.test(x , sigma.x = 5, mu = 200, alt = 'l')


## Bai 11

x = c(183, 173, 176, 185, 181, 183, 180, 188, 168, 179)

z.test(x, sigma.x = 7.6, mu = 178)


## Bai 14

# Ho: lon hon, H1: nho hon

zsum.test(x, alt = 'l')

zsum.test(n.x = 16, mean.x = 7.2, mu = 7.6, sigma.x = 1.2, alternative = 'l')


## Bai 15

zsum.test(n.x = 20, mean.x = 105, mu = 100, sigma.x = 5, alternative = 'g')


## Bai 16

zsum.test(n.x = 2500, mu = 3, mean.x = 2.95, sigma.x = 1, alternative = 'g')


## Bai 17

tsum.test(n.x = 25, mean.x = 19.7, mu = 20, s.x = 1.3)


## Bai 18

x = c(237, 242, 232, 242, 248, 230, 244, 243, 254, 262, 234, 220, 225, 236, 232, 218, 228, 240)

t.test(x, mu = 240, alternative = 'l')


## Bai 19

zsum.test(n.x = 40, mean.x = 0.162, mu = 0.15, sigma.x = 0.04, alternative = 'g', conf.level = 0.99)


## Bai 20

x = c(30.1, 32.7, 22.5, 27.5, 27.7, 29.8, 28.9, 31.4, 31.2, 24.3, 26.4, 22.8, 29.1, 33.4, 32.5, 21.7)

shapiro.test(x)

qqnorm(x)
qqline(x)

t.test(x, mu = 30, alternative = 'l')


## Bai 21

#x = data.frame(value = c(19, 20, 21, 22, 23), freq = c(10, 59, 20, 6,5))

x = c( rep(19,10), rep(20, 59), rep(21, 20), rep(22, 6), rep(23, 5) )

t.test(x, mu= 20)


## Bai 22

x1 = c(5.5, 6, 7, 6, 7.5, 6, 7.5, 5.5, 7, 6.5, 8.5)

x2 = c(6.5, 6, 8.5, 7, 6.5, 8, 7.5, 6.5, 7.5, 6, 7)

t.test(x1, x2, paired=F, var.equal=F, alternative= 'g')


## Bai 24

x1 = c(3250, 3268, 4302, 3184, 3266, 3297, 3332, 3502, 3064, 3116)

x2 = c(3094, 3106, 3004, 3066, 2984, 3124, 3316, 3212, 3380, 3018)

# Day la 2 tong the, lai co cung var => var.equal = T

t.test(x1, x2, var.equal = T, paired = F)

# Kq: df: bac tu do cua pp T

# p-value

# Doi thiet la f

# Khoang tin cay: E(x1) - E(x2)

# mean(x1)  mean(x2)


### Phan tich Phuong sai

# Phan tich phuong sai nham muc dich: So sanh gia tri trung binh cua nhieu tong the

# 1 nhan to: mu1 = muk
# cac cap nhan to: 
# tuong tac:S, mu1 

## Bai1

# gop theo dong
sanLuong = c(86.92, 88, 77, 84, 92, 91, 81, 93, 75, 80, 83, 79)

# phan loai theo loai phan bon, dung ham gl
loaiPhanBon = gl(3,4, length = 12)

# phan loai theo giong lua, dung ham gl
giongLua = gl(4,1, length = 12)

anova(lm(sanLuong~loaiPhanBon+giongLua))

# Kq: p-value cua ca phanbon va gionglua 2 lam 2 phan khac nhau, so sanh vs conf.level

# Tukey anova: Di sau hon vao tung nhom
TukeyHSD(aov(sanLuong~loaiPhanBon+giongLua))


## Bai 2

# a.
# nhap x theo cot
x = c(4.05, 4.01, 4.02, 4.04, 3.99, 4.02, 4.01, 3.99, 4.00, 4.00,3.97, 3.98, 3.97, 3.95, 4, 4,  4.00, 4.02, 3.99, 4.01)

phanLoai = c(rep(1, 4), rep(2, 6), rep(3, 6), rep(4, 4))

anova(lm(x~factor(phanLoai)))

TukeyHSD(aov(x~factor(phanLoai)))

# b.


## Bai 3

# nhap x theo hang
x = c(124.1, 131.5, 127, 126.4, 130.6, 128.4, 127.2, 132.7, 125.6)

xang = gl(3, 3, length=9)

phuGia = gl(3, 1, length= 9)

anova(lm(x~xang+phuGia))

TukeyHSD(aov(x~xang+phuGia))


## Bai 4

x = c(121.1, 124.8, 131.5, 131.6, 127, 126,6, 126.4, 127.2, 142.1, 128.4, 142.6, 127.2, 124.9, 132.7, 133.0, 125.6, 120.9)

xang = gl(3, 6, length=18)

phuGia = gl(3, 2, length= 18)

anova(lm(x ~ xang + phuGia))

TukeyHSD(aov(x~xang + phuGia))


## Bai 5

chisq.test(x =c(18, 16, 23, 20, 23), p = c(0.2, 0.2, 0.2, 0.2, 0.2))


## Bai 6

chisq.test(x= c(458, 30, 12), p = c(0.93, 0.05, 0.02) )


## Bai 7

chisq.test(x= c(180, 100, 120), p =c(0.4, 0.3, 0.3))


## Bai 8



## Bai 9



## Bai10

# nhap theo hang
x = c(175, 124, 92, 118, 110, 126, 127, 82, 147)

yKien = gl(3, 1, length=9)
thuNhap = gl(3, 3, length=9)

anova(lm(x~yKien+thuNhap))
# or
summary(aov(x~yKien + thuNhap))

TukeyHSD(aov(x~yKien + thuNhap))


## Bai11

current_software = c(9.98, 9.98, 9.84, 9.99, 9.94, 9.84, 9.86, 10.12, 9.9, 9.91)

new_software = c(9.88, 9.86, 9.75, 9.8, 9.87, 9.84, 9.87, 9.86, 9.83, 9.86)

# Kiem tra xem co phan phoi chuan khong
shapiro.test(current_software)
shapiro.test(new_software)


# Do khong co phan phoi chuan
wilcox.test(current_software, new_software, paired = F)

## Bai12

nam2001 = c(63.1, 67.1, 65.5, 68, 66.6, 65.7, 69.2, 67, 65.2, 60.7)
nam2002 = c(57.4, 66.4, 68.8, 65.3, 63.5, 66.4, 64.9, 65.2, 65.1, 62.2)

# a. Co phan phoi chuan

t.test(x = nam2001, y = nam2002, paired= F ,var.equal= F, alternative= 'g')


# b. Khong co pp chuan

shapiro.test(nam2001)
shapiro.test(nam2002)


qqnorm(nam2001)
qqline(nam2001)

qqnorm(nam2002)
qqline(nam2002)

# do la phan phoi chuan

t.test(x = nam2001, y = nam2002, paired= F ,var.equal= F, alternative= 'g')

# neu khong, su dung wilcox.test

wilcox.test(x = nam2001, y= nam2002, paired= F, alt ='g')



### Import from excel

# Can convert to csv, sau do

setwd("/Users/p.t.hhieu/Desktop/KHDL") # Neu can set thu muc chua du lieu

data = read.csv("./exercice.csv", header =TRUE)

attach(data)

# Kiem tra phan phoi chuan cua data
shapiro.test(data)

# so sanh trung binh cua x1, x2 
wilcox.test(x1, x2, pair = T, alt = 'g')




##### UL ty le tong the khi biet mau

## Kiem tra khoang tiem can
binom.confint(2, 15, methods = 'asymptotic')

binom.confint(2, 15, methods = 'all')


# Kiem dinh Wilson khi n nho

# method = 'wilson' p van ko gan 0 va 1: 
binom.confint(2, 15, methods = 'wilson')

# method = 'ac' khi p gan 0 va 1
binom.confint(2, 15, methods = 'ac')


## Kiem dinh Khi binh phuong
# n.p va n(1-p) >= 5
prop.test(9, 15)


## Kiem dinh Chinh xac

#Neu khong thoa man dk tren, chuyen sang binom.test()
binom.test(9, 15)

### So sanh 2 ty le: So sanh ty le a trong 2 nhom

A = Titanic

str(A)

# So luong Nam - Nu song sot - Bang 2 chieu
y = xtabs(Freq ~ Sex + Survived, data = Titanic)

addmargins(y, margin = 2) # Bang tan so 2 chieu them tong hang
addmargins(y, margin = 1) # Bang tan so 2 chieu them tong cot

prop.table(y, margin = 2) # Bang tan suat 2 chieu them tong cot
prop.table(y, margin = 1) # Bang tan suat 2 chieu them tong hang

# So sanh ty le chet giua nam vs nu. 
prop.test(y)


## Kiem dinh chinh xac Fisher, khi du lieu khong phu thuoc nhau

A = matrix (c(9, 13, 6, 7), nrow = 2)

fisher.test(A)

## Kiem dinh chinh xac McNemar, khi du lieu phu thuoc nhau

mcnemar.test()


## Kiem dnh khi binh phuong ve tinh thuan nhat
chisq.test()



## Bieu do hop va row
x = c(1, 2, 3, 4,  5, 6, 7, 50)

boxplot(x)





