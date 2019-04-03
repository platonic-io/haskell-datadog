FROM haskell:8.4.4 as builder

# structured to increase the chance of cache hits
RUN cabal v2-update &&\
    cd / &&\
    git clone https://github.com/symbiont-io/haskell-datadog.git workdir &&\
    cd /workdir &&\
    cabal v2-build all --only-dependencies

COPY datadog-tracing.cabal /workdir/
RUN cd /workdir &&\
    cabal v2-build all --only-dependencies

RUN rm -rf /workdir
COPY . /workdir
RUN cd /workdir &&\
    cabal v2-install datadog-agent &&\
    cp -L /root/.cabal/bin/datadog-agent /usr/bin/ &&\
    cd / && rm -rf /workdir /root/.cabal

FROM bitnami/minideb:stretch
ENV LANG C.UTF-8
RUN install_packages libgmp10
COPY --from=builder /usr/bin/datadog-agent /usr/bin/
CMD ["/usr/bin/datadog-agent"]
