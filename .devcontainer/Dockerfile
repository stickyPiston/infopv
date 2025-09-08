#
# A Dockerfile that will install GHC 9.5.2, Z3 4.8.12, and Haskell z3-binding.
# This combination works.
# The dockerfile additionally also install Spin 6.5.1.
#
# * How to build the docker image
#      docker build -t <you-can-use-a-tag-id-if-youwant> .
# * How to run the image (so you get a running container) and enter a shell-mode:
#      docker run --rm -it <tag or image id>
# * How to test if z3 and spin work: see comments below
# * How to close the running container: just type exit at its shell.
#   Note that code/data you might produce during your shell interactions with the
#   container are lost unless you save it to e.g. a folder shared with your own
#   local host.
#
#

FROM gcc:latest

RUN echo "deb http://deb.debian.org/debian bookworm main non-free-firmware \
deb http://deb.debian.org/debian bookworm-updates main non-free-firmware \
deb-src http://deb.debian.org/debian bookworm main non-free-firmware \
deb-src http://deb.debian.org/debian bookworm-updates main non-free-firmware" >> /etc/apt/sources.list.d/bookworm.list

RUN apt-get update && \
    apt-get install alpine-pico
# change-dir to ~/ , which is /root
WORKDIR /root 

# installing ghcup, see https://stackoverflow.com/questions/67680726/installing-haskells-cabal-or-ghcup-inside-a-dockerfile-wont-work
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN bash -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh" && \
    bash -c "curl -sSL https://get.haskellstack.org/ | sh"
# Add ghcup to PATH
ENV PATH=${PATH}:/root/.ghcup/bin
# stack put binaries it builts here, so we also add the path:
ENV PATH=${PATH}:/root/.local/bin
# install ghc 9.2.5
RUN ghcup install ghc 9.2.5 && \
    ghcup set ghc 9.2.5

# Install libz3 (the library) and libz3-dev (the header files)
RUN apt-get install libz3-4=4.8.12-3.1 libz3-dev=4.8.12-3.1 -y

# This should do it. To test if your haskell-z3 is working try this from the shell 
# inside your container:
#    docker run --rm -it <image-id>
#    runhaskell gclparser/examples/examplesHaskellZ3/TestZ3_simple.hs
#    exit
# 
# The test-file you ran should not crash.

#installing spin.
# Building from src seems to be problematic, so below I'll just use the pre-compiled
# binary for version 6.5.1 (so, one sub-version lower).
#
RUN wget https://github.com/nimble-code/Spin/archive/refs/tags/version-6.5.2.zip && \
    unzip *.zip && \
    rm *.zip && \
    mv Spin-version-6.5.2 spin && \
    gunzip spin/Bin/spin651_linux64.gz 
  
# Testing spin-install
# Test-1, which only check the simulator:
#    /spin/Bin/spin651_linux64 ./spin/Examples/hello.pml
#
# Test-2, test that the verifier works:
#    ./spin/Bin/spin651_linux64 -a ./spin/Examples/loops.pml 
#    cc -DNOREDUCE -o pan pan.c
#    ./pan
