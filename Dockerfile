FROM debian:bookworm-slim

# Install dependencies
RUN apt-get update && apt-get install -y \
    curl git make unzip autoconf build-essential libssl-dev \
    libncurses-dev wget ca-certificates \
    python3 python3-numpy python3-pandas python3-matplotlib \
    locales \
    && sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen \
    && locale-gen \
    && update-locale LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8
ENV LANGUAGE=en_US.UTF-8

# Install ASDF
ENV ASDF_DIR=/opt/asdf
RUN git clone https://github.com/asdf-vm/asdf.git $ASDF_DIR --branch v0.14.0
ENV PATH="$ASDF_DIR/bin:$ASDF_DIR/shims:$PATH"

# Install Erlang and Elixir
RUN . $ASDF_DIR/asdf.sh \
 && asdf plugin add erlang https://github.com/asdf-vm/asdf-erlang.git \
 && asdf plugin add elixir https://github.com/asdf-vm/asdf-elixir.git \
 && asdf install erlang 26.2.2 \
 && asdf install elixir 1.14 \
 && asdf global erlang 26.2.2 \
 && asdf global elixir 1.14

# Create app directory
WORKDIR /app
COPY . .

# Precompile deps and build escript
RUN . $ASDF_DIR/asdf.sh && make

# Create output directory
RUN mkdir -p output
VOLUME ["output"]

ENV TERM=xterm

# Run the escript to generate CSVs
ENTRYPOINT ["/bin/sh", "-c", ". /opt/asdf/asdf.sh && ./bench.sh"]
