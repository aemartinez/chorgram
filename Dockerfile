#########################################################################
# Dockerfile for testing and development of ChorGram.
# 
# Install Docker for your platform from https://docs.docker.com/install/
# 
# Build the image with:
# $ docker build -t chorgram .
# 
# To open a shell with the toolchain you can use:
# $ docker run -v $(pwd):/chorgram --rm -it chorgram bash
# 
# To use the GTK based UI from a container, you need to set 
# the DISPLAY environment variable correctly to point to your X Server.
# For example, assuming your IP is 192.168.1.133, you can use 
# the following:
# 
# docker run --rm -it -v $(pwd):/chorgram -e DISPLAY=192.168.1.133:0 chorgram python3 cc/gui.py
# 
#################################################################

FROM mcr.microsoft.com/vscode/devcontainers/base:0-ubuntu-18.04

ENV DEBIAN_FRONTEND=noninteractive
RUN dpkg --add-architecture i386 \ 
   && apt-get update \
   && apt-get -y install --no-install-recommends \
      libc6:i386 \
      git \
      make \
      locales \
      haskell-platform \
      graphviz \
      python3-pip \
      python3-gi \ 
      gobject-introspection \ 
      gir1.2-gtk-3.0 \
   # Clean up
   && apt-get autoremove -y \
   && apt-get clean -y \
   && rm -rf /var/lib/apt/lists/*
ENV DEBIAN_FRONTEND=dialog

# Handle locales
ENV LANG C.UTF-8 

# External tools
RUN wget http://perso.ens-lyon.fr/damien.pous/hknt/hknt-1.0.tar.bz2 \
   && tar xf hknt-1.0.tar.bz2 \
   && rm hknt-1.0.tar.bz2 

RUN wget https://www.lsi.upc.edu/\~jordicf/petrify/distrib/petrify-5.2-linux.tar.gz \
   && tar xf petrify-5.2-linux.tar.gz \
   && rm petrify-5.2-linux.tar.gz

# Haskell libs
RUN cabal update \
    && cabal install MissingH hxt

# Extra Python libs
ADD requirements.txt .
RUN pip3 install -r requirements.txt

ADD . /chorgram
WORKDIR /chorgram

# Compile Haskell libs
RUN make setup
