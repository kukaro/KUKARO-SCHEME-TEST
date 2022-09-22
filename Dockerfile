FROM ubuntu:latest

ENV HOME /root

RUN apt-get update && apt-get install -y git
RUN apt-get install -y vim
RUN apt-get install -y wget
RUN apt-get install -y mit-scheme

ADD ./sh ${HOME}/sh

CMD ["/bin/bash"]