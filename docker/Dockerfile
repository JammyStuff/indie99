FROM jammy/chicagoboss
MAINTAINER James Wheatley <james@jammy.co>

RUN set -xe \
    && yum --nogpgcheck install -y gcc gcc-c++ \
    && yum clean -y all
