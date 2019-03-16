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
varTest(x, conf.level = 0.99)



### BT UL hop ly cuc dai

## UL Hop ly Phan phoi nhi thuc
str(mtcars)

length(mtcars$am)

x = mtcars$am

# Ham hop ly: L (x, p) = P(x = xi) = x^xi * (1-p)^(1-xi)
L = function(p, x) prod(dbinom(x, size = 1, prob = p))

# Tim UL hop ly cuc dai cua p: L(p*)  = max L(p), P thuoc (0, 1)

optimize(L, interval = c(0, 1), x = x, maximum = TRUE)


## UL Hop ly Phan phoi chuan

str(PlantGrowth)

x = PlantGrowth$weight

# tim UL hop ly cuc dai cua Ky vong, Phuong sai cua pp chuan: P bo cua trong luong cay: weight

# Ham hop ly pp chuan: f(x) = (1/ can(2 *pi) * sigma) * e^(-1.2 * ((x - mu)/sigma)
minuslogL = function(mu, sigma2) {-sum(dnorm(x, mean = mu, sd =sqrt(sigma2), log = TRUE))}

MaxLikeEst = mle(minuslogL, start = list(mu = 5, sigma2 = 0.5))

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


## Bai 24

x1 = c(3250, 3268, 4302, 3184, 3266, 3297, 3332, 3502, 3064, 3116)

x2 = c(3094, 3106, 3004, 3066, 2984, 3124, 3316, 3212, 3380, 3018)

# Day la 2 tong the, lai co cung var => var.equal = T

t.test(x1, x2, var.equal = T)

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




anova(lm(sanLuong~loaiPhanBon+giongLua))





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


# Bai 1

zsum.test(n.x = 200, mean.x = 935, sigma.x= 180, conf.level = 0.95)

# Bai 2

x = c(120,123,134,123,132,111,176,192,145,133,126,138,
             123,143,132,153,143,164,174,184,142,165,102,123,
             112,127,156,176,145,204,275,284,195,143,134,127,
             119,134,245,256,364,373,367,283,293,274,246,234)

# Phan loai theo cua hang
cuaHang = gl(4,12, length = 48)

# Phan loai theo thang
dtThang = gl(12,1, length = 48)

anova(lm(x~cuaHang+dtThang))

# Tukey anova: Di sau hon vao tung nhom
TukeyHSD(aov(x~cuaHang+dtThang))


# Bai 4

x = c(9.98, 9.98, 9.84, 9.99, 9.94, 9.94, 9.86, 10.12, 9.9, 9.91, 9.88, 9.86, 9.75, 9.8, 9.87, 9.84, 9.87, 9.86, 9.83, 9.86)


# phan loai theo cua hang
phienBan = gl(2,10, length = 20)

# phan loai theo Lenh
lenh = gl(10, 1, length = 20)

anova(lm(x~phienBan+lenh))


# Bai 5


