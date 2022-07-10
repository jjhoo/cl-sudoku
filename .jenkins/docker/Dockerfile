FROM debian:bullseye-slim

ARG JENKINS_UID=1001
ENV DEBIAN_FRONTEND teletype
ENV LAST_UPDATED 2022-07-10-001

COPY 42local.conf /etc/apt/apt.conf.d/42local.conf

RUN apt-get update
RUN apt-get install -y \
    ca-certificates \
    curl \
    git \
    cl-quicklisp \
    sbcl

RUN useradd -u $JENKINS_UID jenkins -m

WORKDIR /home/jenkins
USER jenkins

COPY --chown=jenkins:jenkins install-quicklisp.lisp /tmp/install-quicklisp.lisp
COPY --chown=jenkins:jenkins sbcl.rc .sbclrc
RUN mkdir quicklisp
RUN mkdir -p .cache/common-lisp
