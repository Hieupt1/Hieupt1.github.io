library("BSDA")

zsum.test(mean.x = 935, sigma.x = 180, n.x = 200, mu = 900, conf.level = 0.95)


prop.test(x = c())

res = chisq.test(x = c(180, 100, 120), p = c(0.4, 0.3, 0.3))

res

current_software = c(9.98, 9.98, 9.84, 9.99, 9.94, 9.84, 9.86, 10.12, 9.9, 9.91)
new_software = c(9.88, 9.86, 9.75, 9.8, 9.87, 9.84, 9.87, 9.86, 9.83, 9.86)

qqnorm(current_software)
qqline(current_software)

qqnorm(new_software)
qqline(new_software)

shapiro.test(current_software)
shapiro.test(new_software)

wilcox.test(current_software, new_software, alternative = "g")


trong_luong = c(13.6, 13.8, 14.0, 13.9, 13.4, 13.2, 13.7, 14.1, 14.2, 14.0, 14.6, 13, 14.2, 14.4, 14.6, 13.8, 13.5)
day_chuyen  = c("line_I", "line_I","line_I","line_I","line_I","line_I", "line_II", "line_II", "line_II", "line_II", "line_II",
                "line_III", "line_III", "line_III", "line_III", "line_III", "line_III" )

dulieu = data.frame(trong_luong, day_chuyen)
result_aov = aov(dulieu$trong_luong ~ dulieu$day_chuyen)
summary(result_aov)

TukeyHSD(result_aov)


month = c("thang1", "thang2", "thang3","thang4","thang5","thang6","thang7", "thang8", "thang9", "thang10", "thang11", "thang12")
shop1 = c(120, )
shop2 = c()
shop3 = c()
shop4 = c()




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


## Bai 3 

# Kiem dinh ty le 
trungThuong = c(180, 100, 120)

chisq.test(trungThuong, p = c(0.4, 0.3, 0.3))



##  Bai 4

current_software = c(9.98, 9.98, 9.84, 9.99, 9.94, 9.84, 9.86, 10.12, 9.9, 9.91)

new_software = c(9.88, 9.86, 9.75, 9.8, 9.87, 9.84, 9.87, 9.86, 9.83, 9.86)

# Kiem tra xem co phan phoi chuan khong
shapiro.test(current_software)
shapiro.test(new_software)


# Do khong co phan phoi chuan
wilcox.test(current_software, new_software, paired = F)


# Bai 5


trong_luong = c(13.6, 13.8, 14.0, 13.9, 13.4, 13.2, 13.7, 14.1, 14.2, 14.0, 14.6, 13, 14.2, 14.4, 14.6, 13.8, 13.5)

line = c (rep(1, 6), rep(2, 5), rep(3, 6))

anova(lm(trong_luong ~ factor(line)))


