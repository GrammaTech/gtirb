ARG BASE_IMAGE
FROM ${BASE_IMAGE}

ARG CXX_COMPILER=g++-7

# Install capstone
RUN cd /usr/local/src \
    && wget https://github.com/aquynh/capstone/archive/4.0.1.tar.gz \
    && tar xf 4.0.1.tar.gz \
    && cd capstone-4.0.1 \
    && CAPSTONE_ARCHS=x86 ./make.sh \
    && CAPSTONE_ARCHS=x86 ./make.sh install

RUN cd /gt/gtirb/build \
    && cmake ../ -DCMAKE_CXX_COMPILER=${CXX_COMPILER} \
    && make -j

RUN TestGTIRB
