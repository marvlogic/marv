# Installation & Setup

## Installation from source - Linux & Mac 

- Install Racket from: [https://download.racket-lang.org](https://download.racket-lang.org/)
- Install the [Google Cloud SDK](https://cloud.google.com/sdk/docs/install-sdk)

Clone the Marv repo:
```
git clone https://www.github.com/marvlogic/marv
```

Install `marv` using `raco`:

```
cd marv
raco pkg install
```

NB [ ] ...setup aliases

## Installation using the Docker Image

Marv is available in a pre-built docker image:

```
docker pull marvlogic/marv:latest
```

The image contains the [Google cloud SDK](https://cloud.google.com/sdk) i.e the `gcloud` command is available. You will need to `gcloud auth login` from inside the container, or arrange authentication via some other method.

`marv` is available as an installed command when running in the container e.g.:

```
docker run -it marvlogic/marv:latest /bin/bash
.. [ set up environment - see below ]
marv --plan /usr/lib/marv/examples/gcp/01-networking.mrv
```

Alternatively, you can mount your local directory inside the container (at `/home/marv`), pass the necessary environment variables into it, and work that way:

```
docker run -v `pwd`:/home/marv \
    -e GCP_ACCESS_TOKEN=$GCP_ACCESS_TOKEN \
    -e MARV_GCP_PROJECT=$MARV_GCP_PROJECT \
    -e MARV_GCP_REGION=$MARV_GCP_REGION \
    -it marvlogic/marv:latest /bin/bash
cd /home/marv
```

## Setup

`marv` is not currently installed as a command-line program, you need to run it
via `racket`. It is much easier to create a shell alias (not needed if using the
docker image):


    alias marv="racket command.rkt"
