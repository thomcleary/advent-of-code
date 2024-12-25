FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get -y install unminimize \
    && (echo "y"; echo "y") | unminimize

RUN apt-get -y install wget gcc make man-db manpages curl fish bat less

# https://github.com/sharkdp/hyperfine?tab=readme-ov-file#on-ubuntu
RUN wget https://github.com/sharkdp/hyperfine/releases/download/v1.18.0/hyperfine_1.18.0_amd64.deb && \
    dpkg -i hyperfine_1.18.0_amd64.deb && \
    rm hyperfine_1.18.0_amd64.deb

WORKDIR /root

RUN curl -sS https://starship.rs/install.sh | sh -s -- --yes

RUN mkdir .config && echo '[container]\nformat = "🐳 "\n' >> .config/starship.toml && \
    mkdir .config/fish && \
    echo 'alias bat=batcat' >> .config/fish/config.fish && \
    echo "set -x MANPAGER \"sh -c 'col -bx | batcat -l man -p'\"" >> .config/fish/config.fish && \
    echo "set -x MANROFFOPT "-c"" >> .config/fish/config.fish && \
    echo 'starship init fish | source' >> .config/fish/config.fish

WORKDIR /2024
CMD ["fish"]
