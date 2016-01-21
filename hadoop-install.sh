#!/bin/bash

# This installs hadoop
sudo wget http://ftp.heanet.ie/mirrors/www.apache.org/dist/hadoop/common/hadoop-2.7.1/hadoop-2.7.1.tar.gz
sudo gunzip hadoop-2.7.1.tar.gz 
sudo tar -xvf hadoop-2.7.1.tar 
sudo rm -f hadoop-2.7.1.tar 

# edit the environment - set the correct ${JAVA_HOME}
cd hadoop-2.7.1/etc/hadoop
sudo vi hadoop-env.sh  

# add the hadoop user and group
sudo groupadd hadoop
sudo useradd -g hadoop hduser

# make the hadoop install owned by the hadoop useer
sudo chown -R hduser:hadoop ./hadoop-2.7.1
sudo ln -s hadoop-2.7.1 hadoop
sudo chown -R hduser:hadoop ./hadoop

# set export HADOOP_HOME=/dev/hadoop
sudo vi /home/hduser/.bashrc

# set up a HDFS partition
cd /dev/hadoop/etc/hadoop
sudo vi hadoop-env.sh
sudo mkdir -p /dev/hadoop/tmp
sudo chown hduser:hadoop /dev/hadoop/tmp
sudo vi core-site.xml 
sudo vi mapred-site.xml.template 
sudo vi hdfs-site.xml 
sudo su - hduser

# Enable SSH for formating HDFS
ssh-keygen -t rsa -P ""
cat $HOME/.ssh/id_rsa.pub >> $HOME/.ssh/authorized_keys

# test SSH works
ssh localhost
/dev/hadoop/bin/hadoop namenode -format

# Now format the HDFS partition
hadoop namenode -format

# start hadoop
/dev/hadoop/sbin/start-all.sh

# Check all hadoop services are running
jps

# test it is all working with word count sample (the 'hello world' of hadoop)
mkdir /home/hduser/gutenberg
cd /home/hduser/gutenberg
wget http://172.17.2.246/pg4300.txt
wget http://172.17.2.246/5000-8.txt
wget http://172.17.2.246/pg20417.txt

hadoop fs -mkdir -p /user/hduser
hdfs dfs -copyFromLocal /home/hduser/gutenberg /user/hduser/gutenberg
hdfs dfs -ls /user/hduser/gutenberg
hadoop jar ./share/hadoop/mapreduce/hadoop-mapreduce-examples-2.7.1.jar wordcount /user/hduser/gutenberg /user/hduser/gutenberg-output
hdfs dfs -ls /user/hduser/gutenberg-output
hdfs dfs -cat /user/hduser/gutenberg-output/part-r-00000

# delete the test data
hdfs dfs -rm -r hdfs:///user/hduser/gutenberg
hdfs dfs -rm -r hdfs:///user/hduser/gutenberg-output

