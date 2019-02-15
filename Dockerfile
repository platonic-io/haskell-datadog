FROM haskell:8.4.4 as builder
RUN cabal v2-update
COPY . /workdir
RUN cd /workdir &&\
    cabal v2-build all --only-dependencies
RUN cd /workdir &&\
    cabal v2-install datadog-agent &&\
    cp -L /root/.cabal/bin/datadog-agent /usr/bin/ &&\
    cd / && rm -rf /workdir /root/.cabal

FROM bitnami/minideb:stretch
ENV LANG C.UTF-8
RUN install_packages libgmp10
COPY --from=builder /usr/bin/datadog-agent /usr/bin/
CMD ["/usr/bin/datadog-agent"]
