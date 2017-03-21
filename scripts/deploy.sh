#!/usr/bin/env bash

sudo apt-get update

sudo apt-get install libcurl4-openssl-dev
sudo apt-get install libxml2-dev
sudo apt-get install libssl-dev

sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9

sudo add-apt-repository 'deb [arch=amd64,i386] https://cran.rstudio.com/bin/linux/ubuntu xenial/'

sudo apt-get install r-base-dev


sudo apt-get install gdebi-core
wget https://download2.rstudio.org/rstudio-server-1.0.136-amd64.deb
sudo gdebi rstudio-server-1.0.136-amd64.deb

echo "www-port=80" | sudo tee -a /etc/rstudio/rserver.conf
sudo rstudio-server restart

echo "rstudio:rstudio" | sudo chpasswd
sudo chmod -R 0777 /home/rstudio

git clone https://github.com/HBGDki/gc-india-training
sudo chown -R rstudio:rstudio /home/rstudio/

# add 10 users for training session
for u in {1..10}
do
  sudo useradd user$u
  echo "user$u:user$u" | sudo chpasswd
  sudo mkdir /home/user$u
  sudo chown user$u:user$u /home/user$u
  sudo chmod -R 0777 /home/user$u
done


sudo cp -rf /home/ubuntu/gc-india-training/material/data /home/rstudio/
sudo cp -rf /home/ubuntu/gc-india-training/material/images /home/rstudio/
sudo cp -f /home/ubuntu/gc-india-training/material/training.Rmd /home/rstudio/
sudo cp -f /home/ubuntu/gc-india-training/material/ModelFunctions.R /home/rstudio/
sudo chown -R rstudio:rstudio /home/rstudio/

# since we'll be updating files, make this a separate loop
for u in {1..10}
do
  sudo cp -rf /home/ubuntu/gc-india-training/material/data /home/user$u/
  sudo cp -rf /home/ubuntu/gc-india-training/material/images /home/user$u/
  sudo cp -f /home/ubuntu/gc-india-training/material/training.Rmd /home/user$u/
  sudo cp -f /home/ubuntu/gc-india-training/material/ModelFunctions.R /home/user$u/
  sudo chown -R user$u:user$u /home/user$u/
done

sudo su - -c "R -e \"install.packages('tidyverse', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('rmarkdown', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('formatR', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('devtools', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('brokenstick', repos=c(CRAN='http://cran.rstudio.com/', deltarho='http://packages.deltarho.org'))\""
sudo su - -c "R -e \"install.packages('face', repos=c(CRAN='http://cran.rstudio.com/', deltarho='http://packages.deltarho.org'))\""
sudo su - -c "R -e \"devtools::install_github('hafen/trelliscopejs')\""
sudo su - -c "R -e \"devtools::install_github('hafen/hbgd@tidy')\""
sudo su - -c "R -e \"install.packages('plotly', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('XML', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('Hmisc', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('reshape', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('gamlss', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('metrumrg', repos='http://R-Forge.R-project.org')\""
sudo su - -c "R -e \"install.packages('nlme', repos='http://cran.rstudio.com/')\""

sudo chmod -R 755 /usr/local/lib/R/site-library
