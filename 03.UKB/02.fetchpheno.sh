WIR_DIR=/home/richards/tomoko.nakanishi/09.COVID19/scratch/01.UKBB/02.LongCOVID

gawk -f ~/my_project/bin/fetch_tab.awk ${WIR_DIR}/data \
~/projects/richards/restricted/ukb-27449/scratch/old-storage/dataset/ukb27449_20688.tab | gzip -c > ${WIR_DIR}/ukb27449_20688_Sx.tab.gz

gawk -f ~/my_project/bin/fetch_tab.awk ${WIR_DIR}/vaccination \
/project/richards/restricted/ukb-27449/data/ukb50908.tab | gzip -c > ${WIR_DIR}/ukb27449_50908_vaccineation.tab.gz

#gawk -f ~/my_project/bin/fetch_tab.awk ../data/data.liver \
#~/projects/richards/restricted/ukb-27449/scratch/old-storage/dataset/ukb27449_20688.tab | gzip -c > ${WIR_DIR}/ukb27449_20688_count.tab.gz
