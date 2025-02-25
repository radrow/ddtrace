if [ ! $ASDF_DIR ]; then
    if [ -d "$HOME/.asdf" ]; then
        export ASDF_DIR=$HOME/asdf
        echo "### Detected existing asdf directory " $ASDF_DIR
    else
        export ASDF_DIR=$(pwd)/asdf
        echo "### Setting local asdf directory " $ASDF_DIR
    fi
else
    echo "### ASDF_DIR already defined"
fi

if [ ! $ASDF_DATA_DIR ]; then
    export ASDF_DATA_DIR=$ASDF_DIR
    echo "### ASDF_DATA_DIR not set. Setting to " $ASDF_DATA_DIR
fi

if [ ! -d $ASDF_DIR ]; then
    echo "### ASDF_DIR points to a non-existent directory. Installing there."
    git clone https://github.com/asdf-vm/asdf.git $ASDF_DIR --branch v0.14.0
fi

source $ASDF_DIR/asdf.sh

asdf plugin add erlang https://github.com/asdf-vm/asdf-erlang.git
asdf plugin add elixir https://github.com/asdf-vm/asdf-elixir.git

asdf install erlang 26.2.2
asdf install elixir 1.14

asdf local erlang 26.2.2
asdf local elixir 1.14
