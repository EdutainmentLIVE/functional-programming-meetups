## Updated Dockerfile for a recent haskell environment
FROM       debian:stretch

## ensure locale is set during build
ENV LANG            C.UTF-8

RUN apt-get update && \
    apt-get install -y postgresql-client-9.6 libpq-dev gnupg2 procps make && \
    echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu xenial main' > /etc/apt/sources.list.d/ghc.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286 && \
    apt-get update && \
    apt-get install -y --no-install-recommends cabal-install-2.0 ghc-8.2.2 happy-1.19.5 alex-3.1.7 \
            zlib1g-dev libtinfo-dev libsqlite3-0 libsqlite3-dev ca-certificates g++ git curl && \
    curl -fSL https://github.com/commercialhaskell/stack/releases/download/v1.6.3/stack-1.6.3-linux-x86_64-static.tar.gz -o stack.tar.gz && \
    curl -fSL https://github.com/commercialhaskell/stack/releases/download/v1.6.3/stack-1.6.3-linux-x86_64-static.tar.gz.asc -o stack.tar.gz.asc && \
    apt-get purge -y --auto-remove curl && \
    export GNUPGHOME="$(mktemp -d)" && \
    gpg --keyserver ha.pool.sks-keyservers.net --recv-keys C5705533DA4F78D8664B5DC0575159689BEFB442 && \
    gpg --batch --verify stack.tar.gz.asc stack.tar.gz && \
    tar -xf stack.tar.gz -C /usr/local/bin --strip-components=1 && \
    /usr/local/bin/stack config set system-ghc --global true && \
    apt-get clean && \
    rm -rf "$GNUPGHOME" /var/lib/apt/lists/* /stack.tar.gz.asc /stack.tar.gz

ENV PATH /root/.cabal/bin:/root/.local/bin:/opt/cabal/2.0/bin:/opt/ghc/8.2.2/bin:/opt/happy/1.19.5/bin:/opt/alex/3.1.7/bin:$PATH

## Local stuff

ENV STACK_ROOT=/stack-root

RUN  stack config set system-ghc --global true && \
     stack --resolver lts-10.5 install stylish-haskell hlint

VOLUME /stack-root
