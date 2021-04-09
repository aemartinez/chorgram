#########################################################################
# Dockerfile for testing and development of ChorGram.
# 
# Install Docker for your platform from https://docs.docker.com/install/
#
# Build the image with:
#
#   $ docker build -t chorgram .
# 
# The execution of the previous command may take a while (it is
# downloading and installing libraries, external tools, etc.).
#
# If the above command returns an error, you may want to execute
#
#   $ sudo adduser <your_user_ID> docker
#
# log out and then re-login (this allows non-sudo users to run Docker)
#
# To open a shell with the toolchain you can use:
#
#   $ docker run -v $PWD:/chorgram --rm -it chorgram bash
# 
# Using the GUI from a container is a little bit more involved, but
# you can try with the following.
# 
# Linux
# -----
# docker run --rm -it \ 
#     -v $PWD:/chorgram \ 
#     -v /tmp/.X11-unix:/tmp/.X11-unix \
#     -e DISPLAY=$DISPLAY \
#     chorgram python3 cc/gui.py
# 
# MacOS
# -----
# Install XQuartz from https://www.xquartz.org/
# You need to set the DISPLAY environment variable 
# to correctly point to your X Server.
# 
# docker run --rm -it \ 
#     -v $PWD:/chorgram \
#     -e DISPLAY=$(ipconfig getifaddr en0):0 \ 
#     chorgram python3 cc/gui.py
# 
# In this case, you might need to unrestrict access to the X Server
# The simplest way (though not the most secure) might be just running
# `xhost +`
# 
#################################################################

FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
   && apt-get -y install --no-install-recommends \
      git \
      openssh-client \
      wget \
      make \
      locales \
      haskell-platform \
      graphviz \
      libgraphviz-dev \
      gobject-introspection \ 
      gir1.2-gtk-3.0 \
      libcairo2-dev \
      # pyenv build dependencies
      build-essential \
      ca-certificates \
      curl \
      git \
      libbz2-dev \
      libffi-dev \
      libncurses5-dev \
      libncursesw5-dev \
      libreadline-dev \
      libsqlite3-dev \
      libssl-dev \
      liblzma-dev \
      llvm \
      make \
      netbase \
      pkg-config \
      tk-dev \
      wget \
      xz-utils \
      zlib1g-dev \
      libgirepository1.0-dev \
      ## HKC deps
      ocamlbuild \
      ocaml-nox \
   # Clean up
   && apt-get autoremove -y \
   && apt-get clean -y \
   && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
ENV DEBIAN_FRONTEND=dialog

ENV LANG="C.UTF-8" \
    LC_ALL="C.UTF-8" \
    PATH="/opt/pyenv/shims:/opt/pyenv/bin:$PATH" \
    PYENV_ROOT="/opt/pyenv" \
    PYENV_SHELL="bash"

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
RUN git clone --single-branch --depth 1 https://github.com/pyenv/pyenv.git $PYENV_ROOT
RUN pyenv install 3.8.6
RUN pyenv global 3.8.6

RUN pip3 install poetry
RUN poetry config virtualenvs.create false

# RUN opam init --comp 1.2.2
# RUN echo "eval `opam config env`" >> ~/.bashrc

ADD . /workspace/chorgram
WORKDIR /workspace/chorgram

RUN poetry install

RUN make setup
