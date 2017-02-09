#!/bin/bash

#set -x

TOOLS="jq"

usage() {
    echo "Usage:"
    echo "$(basename $0) <command> <args>"
    echo
    echo "Commands:"
    echo "    ip  <container-name>       - list IPs of given container"
    echo "    env <container-name>       - list container environment"
    echo "    volumes <container-name>   - list container volume mounts"
}

ip() {
    [ -n "$1" ] || {
        echo "no container name, see --help"
        exit 1
    }

    docker inspect "$1" | jq -M -c -r  '.[].NetworkSettings.Networks[].IPAddress'
}

showenv() {
    [ -n "$1" ] || {
        echo "no container name, see --help"
        exit 1
    }
    docker inspect "$1" | jq -M -c -r '.[].Config.Env[]'
}

volumes() {
    [ -n "$1" ] || {
        echo "no container name, see --help"
        exit 1
    }
    docker inspect "$1" | jq -M -r '"\(.[].Mounts[].Source) -> \(.[].Mounts[].Destination)"'
}

cmd=$1
shift
case "$cmd" in
    "ip")
        ip "$@"
        ;;
    "env"|e)
        showenv "$@"
        ;;
    "volumes"|v)
        volumes "$@"
        ;;
    -h|--help)
        usage
        ;;
    *)
        echo "no command?"
        usage
        exit 1
esac
