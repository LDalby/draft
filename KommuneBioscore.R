# Cascade plots for FS & BNY
# Date: 25/5/2016
# Author: Lars Dalby

library(data.table)
library(readxl)
library(ggplot2)
library(RColorBrewer)

ScipenDefault = getOption('scipen')  # In case it should be turned back again.
options(scipen = 99)  # To avoid scientific notation in the resulting file

muni = as.data.table(read_excel('C:/Users/lada/Desktop/NatKvalIndex2.xlsx'))
munisID = unique(munis[,KommuneID])
# Fix multibioscore error in data:
muni = unique(muni[, Areal:=sum(Areal), by = c('KommuneID', 'Type', 'Bioscore', 'Navn')])
# Calc the proportions
muni[, Areal:=Areal/10000]
muni[, ArealTotal:=sum(Areal), by = 'KommuneID']
muni[, TypeAreal:=sum(Areal), by = c('KommuneID', 'Type')]
muni[, TypeAndel:=(TypeAreal/ArealTotal)*100, by = c('KommuneID', 'Type')]
muni[, ScoreAndel:=(Areal/TypeAreal)*100, by = c('KommuneID', 'Type')]
muni[, ymax:=cumsum(ScoreAndel), by = c('KommuneID', 'Type')]
muni[, ymin:=ymax - ScoreAndel]
# Area of type:
typen = unique(muni[,.(KommuneID, Type, TypeAndel)])
typen[,xmax:=cumsum(TypeAndel), by = 'KommuneID']
typen[,xmin:=xmax - TypeAndel, by = 'KommuneID']
# Merge them back together:
final = merge(muni, typen, by = c('KommuneID', 'Type'))
final[, AvgScore:=weighted.mean(Bioscore, Areal), by = c('KommuneID', 'Type')]
# Set up the color scheme:
cols = brewer.pal(8, 'Dark2')
cols = c("Skov" = cols[1], "Mark" = cols[2], "HedeOverdrev" = cols[5], "EngMose" = cols[4],
	"Sø" = cols[3], "ByerGrønt" = cols[6], "ByerHuseVeje" = cols[7], "Andet" = cols[8] )
# Plot:
pdf(file = 'C:/Users/lada/Desktop/NatKvalIndex4_LD.pdf')
for (i in seq_along(munisID)) {
	komnavn = unique(final[KommuneID == munisID[i],Navn])
	p = ggplot(final[KommuneID == munisID[i],], aes(ymin = 0, ymax = AvgScore, xmin = xmin, xmax = xmax,
		fill = factor(Type))) + ylab('Gns. bioscore') + xlab('Areal andel') + ggtitle(komnavn)
	p = p + geom_rect(colour = I("grey")) + theme_bw() + 
	scale_y_continuous(breaks = c(0,1,3,8,13), limits = c(0,20), labels = c('Ingen', 'Ringe', 'Lokal', 'Regional', 'National')) + 
	theme(panel.grid.minor = element_blank()) + scale_fill_manual(values = cols, guide = guide_legend(title = "Type"))
	print(p)
}
dev.off()
