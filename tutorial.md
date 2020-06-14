# ChorGram installation

The currently suggested installated method is through Docker. 
You can install Docker for your platform from [here](https://docs.docker.com/install/)

Clone or download the source files from [the repository](https://bitbucket.org/emlio_tuosto/chorgram/).

Build the image with:

```bash
docker build -t chorgram .
```

The execution of the previous command may take a while (it is
downloading and installing libraries, external tools, etc.).

If the above command returns an error, you may want to execute:

```bash
sudo adduser <your_user_ID> docker
```

log out and then re-login (this allows non-sudo users to run Docker).

To open a shell with the toolchain you can use:

```
docker run -v $PWD:/chorgram --rm -it chorgram bash
```

Using the GUI from a container is a little bit more involved, but
you can try with the following.

Linux
-----

```
docker run --rm -it \ 
    -v $PWD:/chorgram \ 
    -v /tmp/.X11-unix:/tmp/.X11-unix \
    -e DISPLAY=$DISPLAY \
    chorgram python3 cc/gui.py
```

MacOS
-----
Install XQuartz from https://www.xquartz.org/
You need to set the DISPLAY environment variable 
to correctly point to your X Server.

```
docker run --rm -it \ 
    -v $PWD:/chorgram \
    -e DISPLAY=$(ipconfig getifaddr en0):0 \ 
    chorgram python3 cc/gui.py
```

In this case, you might need to unrestrict access to the X Server
The simplest way (though not the most secure) might be just running
`xhost +`

Windows
-------
We haven't tested yet, but it should be possible in a similar way 
using Docker for Windows.