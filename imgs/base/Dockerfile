FROM erlang:22.2 AS erlang
FROM rust:1.40.0 AS rust

FROM debian:10.2

LABEL author="Maxim Molchanov"
LABEL maintainer="m.molchanov@vonmo.com"
LABEL version="1.0"
LABEL description="Base docker image for EAPA"

ARG UID=1000
ARG GID=1000
ENV UID=${UID}
ENV GID=${GID}
RUN groupadd --gid $GID eapa \
  && useradd --uid $UID --gid $GID --shell /bin/bash --create-home eapa
RUN usermod -u $UID eapa && groupmod -g $GID eapa

ENV TZ=UTC
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

COPY --from=erlang /usr/local/lib/erlang /usr/local/lib/erlang
ENV PATH=/usr/local/lib/erlang/bin:$PATH

COPY --from=rust /usr/local/rustup /usr/local/rustup
COPY --from=rust /usr/local/cargo /usr/local/cargo
RUN chown $GID:$UID /usr/local/rustup && chown $GID:$UID /usr/local/cargo

ENV RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo \
    PATH=/usr/local/cargo/bin:$PATH

RUN apt-get update && apt-get install --no-install-recommends -y \
    apt-transport-https \
    ca-certificates \
    wget \
    curl \
    build-essential \
    git \
    openssl \
    libssl-dev \
    libncurses5 \
    libncurses5-dev \
    xsltproc \
    automake \
    autoconf \
    clang \
    libclang-dev \
    procps \
    ca-certificates \
    python-dev \
    python-pip \
    python-setuptools \
    make \
    cmake \
    locales \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists /var/cache/apt \
    && find /usr/local -name examples | xargs rm -rf

RUN echo "LC_ALL=en_US.UTF-8" >> /etc/environment
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
RUN echo "LANG=en_US.UTF-8" > /etc/locale.conf
RUN locale-gen en_US.UTF-8

ADD ./soft/rebar3 /usr/local/bin/rebar3
RUN chmod a+rwx /usr/local/bin/rebar3

USER eapa

ADD ./docker-entry.sh /docker-entry.sh
CMD ["/docker-entry.sh"]
