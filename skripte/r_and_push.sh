# ova skripta je za bolja vremena
# kad skužim kako cron koristi ssh ključeve

/usr/bin/Rscript /home/ivan/temp_mora/R/temp_mora.R

cd /home/ivan/temp_mora
HOME=/home/ivan git pull
git add -A
git commit -m "here we go again"
HOME=/home/ivan git push
